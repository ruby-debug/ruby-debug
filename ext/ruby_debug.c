#include <stdio.h>
#include <ruby.h>
#include <node.h>
#include <rubysig.h>
#include <st.h>

#define DEBUG_VERSION "0.5"

#define CTX_FL_MOVED        (1<<1)
#define CTX_FL_SUSPEND      (1<<2)
#define CTX_FL_TRACING      (1<<3)
#define CTX_FL_SKIPPED      (1<<4)
#define CTX_FL_IGNORE       (1<<5)

#define CTX_FL_TEST(c,f) ((c)->flags & (f))
#define CTX_FL_SET(c,f) do { (c)->flags |= (f); } while (0)
#define CTX_FL_UNSET(c,f) do { (c)->flags &= ~(f); } while (0)

#define DID_MOVED   (debug_context->last_line != line || \
                          debug_context->last_file == Qnil || \
                          rb_str_cmp(debug_context->last_file, file) != 0)

#define IS_STARTED  (threads_tbl != Qnil)

typedef struct {
    int thnum;
    int flags;
    int stop_next;
    int dest_frame;
    int stop_line;
    int stop_frame;
    VALUE frames;
    VALUE thread;
    VALUE last_file;
    VALUE last_line;
} debug_context_t;

typedef struct {
    VALUE file;
    VALUE line;
    VALUE binding;
    ID id;
} debug_frame_t;

typedef struct {
    VALUE source;
    VALUE pos;
    VALUE expr;
} debug_breakpoint_t;

typedef struct {
    st_table *tbl;
} threads_table_t;

static VALUE threads_tbl = Qnil;
static VALUE breakpoints = Qnil;
static VALUE catchpoint  = Qnil;
static VALUE tracing     = Qfalse;
static VALUE locker      = Qnil;
static VALUE post_mortem = Qfalse;

static VALUE mDebugger;
static VALUE cThreadsTable;
static VALUE cContext;
static VALUE cFrame;
static VALUE cBreakpoint;

static VALUE file_separator;
static VALUE alt_file_separator;

static ID idAtLine;
static ID idAtBreakpoint;
static ID idAtCatchpoint;
static ID idAtTracing;
static ID idBinding;
static ID idBasename;
static ID idEval;
static ID idList;
static ID idClear;
static ID idIndex;

static int start_count = 0;
static int thnum_max = 0;
static int last_debugged_thnum = -1;

static VALUE create_binding(VALUE);
static VALUE debug_stop(VALUE);

typedef struct locked_thread_t { 
    VALUE thread;
    struct locked_thread_t *next;
} locked_thread_t;

static locked_thread_t *locked_head = NULL;
static locked_thread_t *locked_tail = NULL;

static int
is_in_locked(VALUE thread)
{
    locked_thread_t *node;
    
    if(!locked_head)
        return 0;
    
    for(node = locked_head; node != locked_tail; node = node->next)
    {
        if(node->thread == thread) return 1;
    }
    return 0;
}

static void
add_to_locked(VALUE thread)
{
    locked_thread_t *node;
    
    if(is_in_locked(thread))
        return;

    node = ALLOC(locked_thread_t);
    node->thread = thread;
    node->next = NULL;
    if(locked_tail)
        locked_tail->next = node;
    locked_tail = node;
    if(!locked_head)
        locked_head = node;
}

static VALUE
remove_from_locked()
{
    VALUE thread;
    locked_thread_t *node;
    
    if(locked_head == NULL)
        return Qnil;
    node = locked_head;
    locked_head = locked_head->next;
    if(locked_tail == node)
        locked_tail = NULL;
    thread = node->thread;
    xfree(node);
    return thread;
}

static int
thread_hash(VALUE thread)
{
    return (int)FIX2LONG(rb_obj_id(thread));
}

static int
thread_cmp(VALUE a, VALUE b)
{
    if(a == b) return 0;
    if(a < b) return -1;
    return 1;
}

static struct st_hash_type st_thread_hash = {
    thread_cmp,
    thread_hash
};

static int
threads_table_mark_keyvalue(VALUE key, VALUE value, int dummy)
{
    rb_gc_mark(key);
    rb_gc_mark(value);
    return ST_CONTINUE;
}

static void
threads_table_mark(void* data)
{
    threads_table_t *threads_table = (threads_table_t*)data;
    st_foreach(threads_table->tbl, threads_table_mark_keyvalue, 0);
}

static void
threads_table_free(void* data)
{
    threads_table_t *threads_table = (threads_table_t*)data;
    st_free_table(threads_table->tbl);
    xfree(threads_table);
}

static VALUE
threads_table_create()
{
    threads_table_t *threads_table;
    
    threads_table = ALLOC(threads_table_t);
    threads_table->tbl = st_init_table(&st_thread_hash);
    return Data_Wrap_Struct(cThreadsTable, threads_table_mark, threads_table_free, threads_table);
}

static int
threads_table_clear_i(VALUE key, VALUE value, VALUE dummy)
{
    return ST_DELETE;
}

static void
threads_table_clear(VALUE table)
{
    threads_table_t *threads_table;
    
    Data_Get_Struct(table, threads_table_t, threads_table);
    st_foreach(threads_table->tbl, threads_table_clear_i, 0);
}

/*
 *   call-seq:
 *      Debugger.started? -> bool
 *   
 *   Returns +true+ the debugger is started.
 */
static VALUE
debug_is_started(VALUE self)
{
    return IS_STARTED ? Qtrue : Qfalse;
}

static void
debug_check_started()
{
    if(!IS_STARTED)
    {
        rb_raise(rb_eRuntimeError, "Debugger.start is not called yet.");
    }
}

static void
debug_context_mark(void* data)
{
    debug_context_t *debug_context = (debug_context_t *)data;
    rb_gc_mark(debug_context->frames);
    rb_gc_mark(debug_context->thread);
    rb_gc_mark(debug_context->last_file);
    rb_gc_mark(debug_context->last_line);
}

static VALUE
debug_context_create(VALUE thread)
{
    VALUE result;
    debug_context_t *debug_context;
    
    debug_context = ALLOC(debug_context_t);
    debug_context-> thnum = ++thnum_max;
    
    debug_context->last_file = Qnil;
    debug_context->last_line = Qnil;
    debug_context->flags = 0;

    debug_context->stop_next = -1;
    debug_context->dest_frame = -1;
    debug_context->stop_line = -1;
    debug_context->stop_frame = -1;
    debug_context->frames = rb_ary_new();
    debug_context->thread = thread;
    result = Data_Wrap_Struct(cContext, debug_context_mark, xfree, debug_context);
    return result;
}

static VALUE
thread_context_lookup(VALUE thread)
{
    VALUE context;
    threads_table_t *threads_table;

    debug_check_started();

    Data_Get_Struct(threads_tbl, threads_table_t, threads_table);
    if(!st_lookup(threads_table->tbl, thread, &context))
    {
        context = debug_context_create(thread);
        st_insert(threads_table->tbl, thread, context);
    }
    return context;
}

static void
debug_frame_mark(void *data)
{
    debug_frame_t *debug_frame = (debug_frame_t *)data;
    rb_gc_mark(debug_frame->binding);
    rb_gc_mark(debug_frame->file);
    rb_gc_mark(debug_frame->line);
}

static VALUE
debug_frame_create(VALUE file, VALUE line, VALUE binding, ID id)
{
    VALUE result;
    debug_frame_t *debug_frame;
    
    debug_frame = ALLOC(debug_frame_t);
    debug_frame->file = file;
    debug_frame->line = line;
    debug_frame->binding = binding;
    debug_frame->id = id;
    result = Data_Wrap_Struct(cFrame, debug_frame_mark, xfree, debug_frame);
    
    return result;
}

static void
save_current_position(VALUE context)
{
    VALUE cur_frame;
    debug_context_t *debug_context;
    debug_frame_t *debug_frame;

    Data_Get_Struct(context, debug_context_t, debug_context);
    if(RARRAY(debug_context->frames)->len == 0)
        return;

    cur_frame = *RARRAY(debug_context->frames)->ptr;
    Data_Get_Struct(cur_frame, debug_frame_t, debug_frame);
    
    debug_context->last_file = debug_frame->file;
    debug_context->last_line = debug_frame->line;
    CTX_FL_UNSET(debug_context, CTX_FL_MOVED);
}

static VALUE
call_at_line_unprotected(VALUE args)
{
    VALUE context;
    context = *RARRAY(args)->ptr;
    return rb_funcall2(context, idAtLine, RARRAY(args)->len - 1, RARRAY(args)->ptr + 1);
}

static VALUE
call_at_line(VALUE context, int thnum, VALUE binding, VALUE file, VALUE line)
{
    VALUE args;
    
    last_debugged_thnum = thnum;
    save_current_position(context);
    
    args = rb_ary_new3(4, context, file, line, binding);
    return rb_protect(call_at_line_unprotected, args, 0);
}

static void
set_frame_source(debug_context_t *debug_context, VALUE file, VALUE line)
{
    VALUE frame;
    debug_frame_t *top_frame;

    if(RARRAY(debug_context->frames)->len > 0)
    {
        frame = *RARRAY(debug_context->frames)->ptr;
        Data_Get_Struct(frame, debug_frame_t, top_frame);
        top_frame->file = file;
        top_frame->line = line;
    }
}

static void
save_call_frame(VALUE self, VALUE file, VALUE line, ID mid, debug_context_t *debug_context)
{
    VALUE frame, binding;
    
    binding = self? create_binding(self) : Qnil;
    frame = debug_frame_create(file, line, binding, mid);
    rb_ary_unshift(debug_context->frames, frame);
}

static VALUE
basename(VALUE filename)
{
    return rb_funcall(rb_cFile, idBasename, 1, filename);
}

static int
filename_cmp(debug_breakpoint_t *debug_breakpoint, VALUE file)
{
    VALUE flag = Qnil;
    
    flag = rb_funcall(debug_breakpoint->source, idIndex, 1, file_separator);
    if(alt_file_separator != Qnil && flag == Qnil)
        flag = rb_funcall(debug_breakpoint->source, idIndex, 1, alt_file_separator);
    if(flag == Qnil)
        file = basename(file);
    else
        file = rb_file_expand_path(file, Qnil);
    return(rb_str_cmp(debug_breakpoint->source, file) == 0);
}

static int
classname_cmp(debug_breakpoint_t *debug_breakpoint, VALUE klass)
{
    return (klass != Qnil && rb_str_cmp(debug_breakpoint->source, rb_mod_name(klass)) == 0);
}

static int
check_breakpoints(debug_context_t *debug_context, VALUE file, VALUE klass, VALUE pos)
{
    VALUE breakpoint; 
    debug_breakpoint_t *debug_breakpoint;
    int i;

    if(RARRAY(breakpoints)->len == 0)
        return -1;
    if(!CTX_FL_TEST(debug_context, CTX_FL_MOVED))
        return -1;
    
    for(i = 0; i < RARRAY(breakpoints)->len; i++)
    {
        breakpoint = rb_ary_entry(breakpoints, i);
        Data_Get_Struct(breakpoint, debug_breakpoint_t, debug_breakpoint);
        if(debug_breakpoint->pos != pos && !(TYPE(pos) == T_STRING && 
            TYPE(debug_breakpoint->pos) == T_STRING && rb_str_cmp(debug_breakpoint->pos, pos) == 0))
            continue;
        if(filename_cmp(debug_breakpoint, file) || classname_cmp(debug_breakpoint, klass))
            return i;
    }
    return -1;
}

/*
 * This is a NASTY HACK. For some reasons rb_f_binding is decalred 
 * static in eval.c
 */
static VALUE
create_binding(VALUE self)
{
    typedef VALUE (*bind_func_t)(VALUE);
    static bind_func_t f_binding = NULL;

    if(f_binding == NULL)
    {
        NODE *body, *method;
        st_lookup(RCLASS(rb_mKernel)->m_tbl, idBinding, (st_data_t *)&body);
        method       = (NODE *)body->u2.value;
        f_binding    = (bind_func_t)method->u1.value;
    }
    return f_binding(self);
}

static VALUE
get_breakpoint_at(int index) 
{
    return rb_ary_entry(breakpoints, index);
}

static VALUE
eval_expression(VALUE args)
{
    return rb_funcall2(rb_mKernel, idEval, 2, RARRAY(args)->ptr);
}

static int
check_breakpoint_expression(VALUE breakpoint, VALUE binding)
{
    debug_breakpoint_t *debug_breakpoint;
    VALUE args, expr_result;
    
    Data_Get_Struct(breakpoint, debug_breakpoint_t, debug_breakpoint);
    if(NIL_P(debug_breakpoint->expr))
        return 1;
    
    args = rb_ary_new3(2, debug_breakpoint->expr, binding);
    expr_result = rb_protect(eval_expression, args, 0);
    return RTEST(expr_result);
}

static void
debug_event_hook(rb_event_t event, NODE *node, VALUE self, ID mid, VALUE klass)
{
    VALUE thread, context, breakpoint;
    VALUE binding = Qnil;
    debug_context_t *debug_context;
    VALUE file = Qnil, line = Qnil;
    int breakpoint_index = -1;
    
    if (mid == ID_ALLOCATOR) return;
    if(!node) return;
    
    thread = rb_thread_current();
    context = thread_context_lookup(thread);
    Data_Get_Struct(context, debug_context_t, debug_context);
    
    /* return if thread is marked as 'ignored'.
       debugger's threads are marked this way
    */
    if(CTX_FL_TEST(debug_context, CTX_FL_IGNORE)) return;
    
    while(1)
    {
        /* halt execution of the current thread if the debugger
           is activated in another
        */
        while(locker != Qnil && locker != thread)
        {
            add_to_locked(thread);
            rb_thread_stop();
        }

        /* stop the current thread if it's marked as suspended */
        if(CTX_FL_TEST(debug_context, CTX_FL_SUSPEND))
            rb_thread_stop();
        else break;
    }
    
    /* return if the current thread is the locker */
    if(locker != Qnil) return;
    
    /* only the current thread can proceed */
    locker = thread;

    /* ignore a skipped section of code */
    if(CTX_FL_TEST(debug_context, CTX_FL_SKIPPED)) goto cleanup;

    file = rb_str_new2(node->nd_file);
    line = INT2FIX(nd_line(node));

    if(DID_MOVED)
        CTX_FL_SET(debug_context, CTX_FL_MOVED);

    switch(event)
    {
    case RUBY_EVENT_LINE:
    {
        set_frame_source(debug_context, file, line);

        if(RTEST(tracing) || CTX_FL_TEST(debug_context, CTX_FL_TRACING))
        {
            rb_funcall(context, idAtTracing, 2, file, line);
        }
        
        if(debug_context->dest_frame == -1 || 
            RARRAY(debug_context->frames)->len == debug_context->dest_frame)
        {
            debug_context->stop_next--;
            if(debug_context->stop_next < 0)
                debug_context->stop_next = -1;
            /* we check that we actualy moved to another line */
            if(DID_MOVED)
            {
                debug_context->stop_line--;
            }
        }
        else if(RARRAY(debug_context->frames)->len < debug_context->dest_frame)
        {
            debug_context->stop_next = 0;
        }

        if(RARRAY(debug_context->frames)->len == 0)
        {
            save_call_frame(self, file, line, mid, debug_context);
        }

        if(debug_context->stop_next == 0 || debug_context->stop_line == 0 ||
            (breakpoint_index = check_breakpoints(debug_context, file, klass, line)) != -1)
        {
            binding = self? create_binding(self) : Qnil;
            /* check breakpoint expression */
            if(breakpoint_index != -1)
            {
                breakpoint = get_breakpoint_at(breakpoint_index);
                if(check_breakpoint_expression(breakpoint, binding))
                {
                    rb_funcall(context, idAtBreakpoint, 1, breakpoint);
                }
                else
                    break;
            }

            /* reset all pointers */
            debug_context->dest_frame = -1;
            debug_context->stop_line = -1;
            debug_context->stop_next = -1;

            call_at_line(context, debug_context->thnum, binding, file, line);
        }
        break;
    }
    case RUBY_EVENT_C_CALL:
    {
        set_frame_source(debug_context, file, line);
        break;
    }
    case RUBY_EVENT_CALL:
    {
        save_call_frame(self, file, line, mid, debug_context);
        breakpoint_index = check_breakpoints(debug_context, file, klass, rb_str_new2(rb_id2name(mid)));
        if(breakpoint_index != -1)
        {
            binding = self? create_binding(self) : Qnil;
            breakpoint = get_breakpoint_at(breakpoint_index);
            if(check_breakpoint_expression(breakpoint, binding))
            {
                rb_funcall(context, idAtBreakpoint, 1, breakpoint);
                call_at_line(context, debug_context->thnum, binding, file, line);
            }
        }
        break;
    }
    case RUBY_EVENT_RETURN:
    case RUBY_EVENT_END:
    {
        if(RARRAY(debug_context->frames)->len == debug_context->stop_frame)
        {
            debug_context->stop_next = 1;
            debug_context->stop_frame = 0;
        }
        rb_ary_shift(debug_context->frames);
        break;
    }
    case RUBY_EVENT_CLASS:
    {
        save_call_frame(self, file, line, mid, debug_context);
        break;
    }
    case RUBY_EVENT_RAISE:
    {
        VALUE ancestors;
        VALUE expn_class, aclass;
        int i;
        
        if(post_mortem == Qtrue && self)
        {
            binding = create_binding(self);
            rb_ivar_set(ruby_errinfo, rb_intern("@__debug_binding"), binding);
            rb_ivar_set(ruby_errinfo, rb_intern("@__debug_file"), file);
            rb_ivar_set(ruby_errinfo, rb_intern("@__debug_line"), line);
            rb_ivar_set(ruby_errinfo, rb_intern("@__debug_binding"), binding);
            rb_ivar_set(ruby_errinfo, rb_intern("@__debug_frames"), rb_obj_dup(debug_context->frames));
        }

        expn_class = rb_obj_class(ruby_errinfo);
        if( !NIL_P(rb_class_inherited_p(expn_class, rb_eSystemExit)) )
        {
            debug_stop(mDebugger);
            break;
        }

        if(catchpoint == Qnil)
            break;

        ancestors = rb_mod_ancestors(expn_class);
        for(i = 0; i < RARRAY(ancestors)->len; i++)
        {
            aclass = rb_ary_entry(ancestors, i);
            if(rb_str_cmp(rb_mod_name(aclass), catchpoint) == 0)
            {
                rb_funcall(context, idAtCatchpoint, 1, ruby_errinfo);
                if(self && binding == Qnil)
                    binding = create_binding(self);
                call_at_line(context, debug_context->thnum, binding, file, line);
                break;
            }
        }

        break;
    }
    case RUBY_EVENT_C_RETURN:
    {
        break;
    }
    }

    cleanup:
    
    /* release a lock */
    locker = Qnil;
    /* let the next thread to run */
    thread = remove_from_locked();
    if(thread != Qnil)
        rb_thread_run(thread);
}

static VALUE
debug_stop_i(VALUE self)
{
    if(IS_STARTED)
        debug_stop(self);
    return Qnil;
}

/*
 *   call-seq:
 *      Debugger.start -> bool
 *      Debugger.start { ... } -> obj
 *   
 *   This method activates the debugger. 
 *   If it's called without a block it returns +true+, unless debugger was already started.
 *   If a block is given, it starts debugger and yields to block. When the block is finished
 *   executing it stops the debugger with Debugger.stop method.
 *
 *   <i>Note that if you want to stop debugger, you must call Debugger.stop as many time as you 
 *   called Debugger.start method.</i>
 */
static VALUE
debug_start(VALUE self)
{
    VALUE result;
    start_count++;
    
    if(IS_STARTED)
        result = Qfalse;
    else
    {
        breakpoints = rb_ary_new();
        locker      = Qnil;
        threads_tbl = threads_table_create();

        rb_add_event_hook(debug_event_hook, RUBY_EVENT_ALL);
        result = Qtrue;
    }
    
    if(rb_block_given_p())
        return rb_ensure(rb_yield, Qnil, debug_stop_i, self);
    return result;
}

/*
 *   call-seq:
 *      Debugger.stop -> bool
 *   
 *   This method disacivates the debugger. It returns +true+ if the debugger is disacivated, 
 *   otherwise it returns +false+.
 *
 *   <i>Note that if you want to stop debugger, you must call Debugger.stop as many time as you 
 *   called Debugger.start method.</i>
 */
static VALUE
debug_stop(VALUE self)
{
    debug_check_started();
    
    start_count--;
    if(start_count)
        return Qfalse;
    
    rb_remove_event_hook(debug_event_hook);

    locker      = Qnil;
    breakpoints = Qnil;
    threads_tbl = Qnil;

    return Qtrue;
}

static void
breakpoint_mark(void *data)
{
    debug_breakpoint_t *breakpoint;
    breakpoint = (debug_breakpoint_t *)data;
    rb_gc_mark(breakpoint->source);
    rb_gc_mark(breakpoint->pos);
    rb_gc_mark(breakpoint->expr);
}

/*
 *   call-seq:
 *      Debugger.add_breakpoint(source, pos, condition = nil) -> breakpoint
 *   
 *   Adds a new breakpoint.
 *   <i>source</i> is a name of a file or a class.
 *   <i>pos</i> is a line number or a method name if <i>source</i> is a class name.
 *   <i>condition</i> is a string which is evaluated to +true+ when this breakpoint
 *   is activated.
 */
static VALUE
debug_add_breakpoint(int argc, VALUE *argv, VALUE self)
{
    VALUE source, pos, expr;
    VALUE result;
    debug_breakpoint_t *breakpoint;

    debug_check_started();
    
    if(rb_scan_args(argc, argv, "21", &source, &pos, &expr) == 2)
    {
        expr = Qnil;
    }

    breakpoint = ALLOC(debug_breakpoint_t);
    breakpoint->source = StringValue(source);
    breakpoint->pos = pos;
    breakpoint->expr = NIL_P(expr) ? expr: StringValue(expr);
    result = Data_Wrap_Struct(cBreakpoint, breakpoint_mark, xfree, breakpoint);
    rb_ary_push(breakpoints, result);
    return result;
}

/*
 *   call-seq:
 *      Debugger.breakpoints -> array
 *   
 *   Returns an array of breakpoints.
 */
static VALUE
debug_breakpoints(VALUE self)
{
    debug_check_started();

    return breakpoints;
}

/*
 *   call-seq:
 *      Debugger.checkpoint -> string
 *   
 *   Returns a current checkpoint, which is a name of exception that will 
 *   trigger a debugger when raised.
 */
static VALUE
debug_catchpoint(VALUE self)
{
    debug_check_started();

    return catchpoint;
}

/*
 *   call-seq:
 *      Debugger.checkpoint = string -> string
 *   
 *   Sets checkpoint.
 */
static VALUE
debug_set_catchpoint(VALUE self, VALUE value)
{
    debug_check_started();

    if (!NIL_P(value) && TYPE(value) != T_STRING) {
        rb_raise(rb_eTypeError, "value of checkpoint must be String");
    }
    if(NIL_P(value))
    catchpoint = Qnil;
    else
    catchpoint = rb_str_dup(value);
    return value;
}

static int
find_last_context_func(VALUE key, VALUE value, VALUE *result)
{
    debug_context_t *debug_context;
    Data_Get_Struct(value, debug_context_t, debug_context);
    if(debug_context->thnum == last_debugged_thnum)
    {
        *result = value;
        return ST_STOP;
    }
    return ST_CONTINUE;
}

/*
 *   call-seq:
 *      Debugger.last_interrupted -> context
 *   
 *   Returns last debugged context.
 */
static VALUE
debug_last_interrupted(VALUE self)
{
    VALUE result = Qnil;
    threads_table_t *threads_table;

    debug_check_started();

    Data_Get_Struct(threads_tbl, threads_table_t, threads_table);
    
    st_foreach(threads_table->tbl, find_last_context_func, (st_data_t)&result);
    return result;
}

/*
 *   call-seq:
 *      Debugger.current_context -> context
 *   
 *   Returns current context. 
 *   <i>Note:</i> Debugger.current_context.thread == Thread.current
 */
static VALUE
debug_current_context(VALUE self)
{
    VALUE thread, context;

    debug_check_started();

    thread = rb_thread_current();
    context = thread_context_lookup(thread);

    return context;
}

/*
 *   call-seq:
 *      Debugger.contexts -> array
 *   
 *   Returns an array of all contexts.
 */
static VALUE
debug_contexts(VALUE self)
{
    volatile VALUE list;
    volatile VALUE new_list;
    VALUE thread, context;
    threads_table_t *threads_table;
    debug_context_t *debug_context;
    int i;

    debug_check_started();

    new_list = rb_ary_new();
    list = rb_funcall(rb_cThread, idList, 0);
    for(i = 0; i < RARRAY(list)->len; i++)
    {
        thread = rb_ary_entry(list, i);
        context = thread_context_lookup(thread);
        rb_ary_push(new_list, context);
    }
    threads_table_clear(threads_tbl);
    Data_Get_Struct(threads_tbl, threads_table_t, threads_table);
    for(i = 0; i < RARRAY(new_list)->len; i++)
    {
        context = rb_ary_entry(new_list, i);
        Data_Get_Struct(context, debug_context_t, debug_context);
        st_insert(threads_table->tbl, debug_context->thread, context);
    }

    return new_list;
}

/*
 *   call-seq:
 *      Debugger.suspend -> Debugger
 *   
 *   Suspends all contexts.
 */
static VALUE
debug_suspend(VALUE self)
{
    VALUE current, context;
    VALUE saved_crit;
    VALUE context_list;
    debug_context_t *debug_context;
    int i;

    debug_check_started();

    saved_crit = rb_thread_critical;
    rb_thread_critical = Qtrue;
    context_list = debug_contexts(self);
    current = thread_context_lookup(rb_thread_current());

    for(i = 0; i < RARRAY(context_list)->len; i++)
    {
        context = rb_ary_entry(context_list, i);
        if(current == context)
            continue;
        Data_Get_Struct(context, debug_context_t, debug_context);
        CTX_FL_SET(debug_context, CTX_FL_SUSPEND);
    }
    rb_thread_critical = saved_crit;

    if(rb_thread_critical == Qfalse)
        rb_thread_schedule();

    return self;
}

/*
 *   call-seq:
 *      Debugger.resume -> Debugger
 *   
 *   Resumes all contexts.
 */
static VALUE
debug_resume(VALUE self)
{
    VALUE current, context;
    VALUE saved_crit;
    VALUE context_list;
    debug_context_t *debug_context;
    int i;

    debug_check_started();

    saved_crit = rb_thread_critical;
    rb_thread_critical = Qtrue;
    context_list = debug_contexts(self);

    current = thread_context_lookup(rb_thread_current());
    for(i = 0; i < RARRAY(context_list)->len; i++)
    {
        context = rb_ary_entry(context_list, i);
        if(current == context)
            continue;
        Data_Get_Struct(context, debug_context_t, debug_context);
        if(CTX_FL_TEST(debug_context, CTX_FL_SUSPEND))
        {
            CTX_FL_UNSET(debug_context, CTX_FL_SUSPEND);
            rb_thread_run(debug_context->thread);
        }
    }
    rb_thread_critical = saved_crit;

    rb_thread_schedule();

    return self;
}

/*
 *   call-seq:
 *      Debugger.tracing -> bool
 *   
 *   Returns +true+ if the global tracing is activated.
 */
static VALUE
debug_tracing(VALUE self)
{
    return tracing;
}

/*
 *   call-seq:
 *      Debugger.tracing = bool
 *   
 *   Sets the global tracing flag.
 */
static VALUE
debug_set_tracing(VALUE self, VALUE value)
{
    tracing = RTEST(value) ? Qtrue : Qfalse;
    return value;
}

/*
 *   call-seq:
 *      Debugger.post_mortem? -> bool
 *   
 *   Returns +true+ if post-moterm debugging is enabled.
 */
static VALUE
debug_post_mortem(VALUE self)
{
    return post_mortem;
}

/*
 *   call-seq:
 *      Debugger.post_mortem = bool
 *   
 *   Sets post-moterm flag.
 *   FOR INTERNAL USE ONLY.
 */
static VALUE
debug_set_post_mortem(VALUE self, VALUE value)
{
    post_mortem = RTEST(value) ? Qtrue : Qfalse;
    return value;
}

/*
 *   call-seq:
 *      Debugger.debug_load(file) -> nil
 *   
 *   Same as Kernel#load but resets current context's frames. 
 *   FOR INTERNAL USE ONLY. Use Debugger.post_mortem method instead.
 */
static VALUE
debug_debug_load(VALUE self, VALUE file)
{
    VALUE context;
    debug_context_t *debug_context;
    
    debug_start(self);
    
    context = debug_current_context(self);
    Data_Get_Struct(context, debug_context_t, debug_context);
    rb_ary_clear(debug_context->frames);
    rb_load(file, 0);
    
    debug_stop(self);
    return Qnil;
}

static VALUE
set_current_skipped_status(VALUE status)
{
    VALUE context;
    debug_context_t *debug_context;
    
    context = debug_current_context(Qnil);
    Data_Get_Struct(context, debug_context_t, debug_context);
    if(status) 
        CTX_FL_SET(debug_context, CTX_FL_SKIPPED);
    else
        CTX_FL_UNSET(debug_context, CTX_FL_SKIPPED);
    return Qnil;
}

/*
 *   call-seq:
 *      Debugger.skip { block } -> obj or nil
 *   
 *   The code inside of the block is escaped from the debugger.
 */
static VALUE
debug_skip(VALUE self)
{
    if (!rb_block_given_p()) {
        rb_raise(rb_eArgError, "called without a block");
    }
    if(!IS_STARTED)
        return rb_yield(Qnil);
    set_current_skipped_status(Qtrue);
    return rb_ensure(rb_yield, Qnil, set_current_skipped_status, Qfalse);
}

static VALUE
debug_at_exit_c(VALUE proc)
{
    return rb_funcall(proc, rb_intern("call"), 0);
}

static void
debug_at_exit_i(VALUE proc)
{
    if(!IS_STARTED)
    {
        debug_at_exit_c(proc);
    }
    else
    {
        set_current_skipped_status(Qtrue);
        rb_ensure(debug_at_exit_c, proc, set_current_skipped_status, Qfalse);
    }
}

/*
 *   call-seq:
 *      Debugger.debug_at_exit { block } -> proc
 *   
 *   Register <tt>at_exit</tt> hook which is escaped from the debugger.
 *   FOR INTERNAL USE ONLY.
 */
static VALUE
debug_at_exit(VALUE self)
{
    VALUE proc;
    if (!rb_block_given_p()) {
        rb_raise(rb_eArgError, "called without a block");
    }
    proc = rb_block_proc();
    rb_set_end_proc(debug_at_exit_i, proc);
    return proc;
}

/*
 *   call-seq:
 *      context.stop_next = steps
 *   
 *   Stops the current context after a number +steps+ are made.
 */
static VALUE
context_stop_next(VALUE self, VALUE steps)
{
    debug_context_t *debug_context;

    debug_check_started();
    
    Data_Get_Struct(self, debug_context_t, debug_context);
    if(FIX2INT(steps) < 0)
    rb_raise(rb_eRuntimeError, "Steps argument can't be negative.");
    debug_context->stop_next = FIX2INT(steps);
    
    return steps;
}

/*
 *   call-seq:
 *      context.step_over(steps)
 *   
 *   Steps over a +steps+ number of times.
 */
static VALUE
context_step_over(int argc, VALUE *argv, VALUE self)
{
    VALUE lines, frame;
    debug_context_t *debug_context;

    debug_check_started();

    Data_Get_Struct(self, debug_context_t, debug_context);
    if(RARRAY(debug_context->frames)->len == 0)
        rb_raise(rb_eRuntimeError, "No frames collected.");

    rb_scan_args(argc, argv, "11", &lines, &frame);
    debug_context->stop_line = FIX2INT(lines);
    if(argc == 1)
    {
        debug_context->dest_frame = RARRAY(debug_context->frames)->len;
    }
    else
    {
        if(FIX2INT(frame) < 0 && FIX2INT(frame) >= RARRAY(debug_context->frames)->len)
            rb_raise(rb_eRuntimeError, "Destination frame is out of range.");
        debug_context->dest_frame = FIX2INT(frame);
    }

    return Qnil;
}

/*
 *   call-seq:
 *      context.stop_frame(frame)
 *   
 *   Stops when a frame with number +frame+ is activated. Implements +up+ and +down+ commands.
 */
static VALUE
context_stop_frame(VALUE self, VALUE frame)
{
    debug_context_t *debug_context;

    debug_check_started();

    Data_Get_Struct(self, debug_context_t, debug_context);
    if(FIX2INT(frame) < 0 && FIX2INT(frame) >= RARRAY(debug_context->frames)->len)
        rb_raise(rb_eRuntimeError, "Stop frame is out of range.");
    debug_context->stop_frame = FIX2INT(frame);

    return frame;
}

/*
 *   call-seq:
 *      context.frames -> array
 *   
 *   Returns an array of frames.
 */
static VALUE
context_frames(VALUE self)
{
    debug_context_t *debug_context;

    Data_Get_Struct(self, debug_context_t, debug_context);
    return debug_context->frames;
}

/*
 *   call-seq:
 *      context.thread -> trhread
 *   
 *   Returns a thread this context is associated with.
 */
static VALUE
context_thread(VALUE self)
{
    debug_context_t *debug_context;

    Data_Get_Struct(self, debug_context_t, debug_context);
    return debug_context->thread;
}

/*
 *   call-seq:
 *      context.thnum -> int
 *   
 *   Returns the context's number.
 */
static VALUE
context_thnum(VALUE self)
{
    debug_context_t *debug_context;

    Data_Get_Struct(self, debug_context_t, debug_context);
    return INT2FIX(debug_context->thnum);
}

/*
 *   call-seq:
 *      context.suspend -> nil
 *   
 *   Suspends the thread when it is running.
 */
static VALUE
context_suspend(VALUE self)
{
    debug_context_t *debug_context;

    debug_check_started();

    Data_Get_Struct(self, debug_context_t, debug_context);
    if(CTX_FL_TEST(debug_context, CTX_FL_SUSPEND))
        rb_raise(rb_eRuntimeError, "Already suspended.");
    CTX_FL_SET(debug_context, CTX_FL_SUSPEND);
    return Qnil;
}

/*
 *   call-seq:
 *      context.resume -> nil
 *   
 *   Resumes the thread from the suspended mode.
 */
static VALUE
context_resume(VALUE self)
{
    debug_context_t *debug_context;

    debug_check_started();

    Data_Get_Struct(self, debug_context_t, debug_context);
    if(!CTX_FL_TEST(debug_context, CTX_FL_SUSPEND))
        rb_raise(rb_eRuntimeError, "Thread is not suspended.");
    CTX_FL_UNSET(debug_context, CTX_FL_SUSPEND);
    rb_thread_run(debug_context->thread);
    return Qnil;
}

/*
 *   call-seq:
 *      context.tracing -> bool
 *   
 *   Returns the tracing flag for the current context.
 */
static VALUE
context_tracing(VALUE self)
{
    debug_context_t *debug_context;

    debug_check_started();

    Data_Get_Struct(self, debug_context_t, debug_context);
    return CTX_FL_TEST(debug_context, CTX_FL_TRACING) ? Qtrue : Qfalse;
}

/*
 *   call-seq:
 *      context.tracking = bool
 *   
 *   Controls the tracing for this context.
 */
static VALUE
context_set_tracing(VALUE self, VALUE value)
{
    debug_context_t *debug_context;

    debug_check_started();

    Data_Get_Struct(self, debug_context_t, debug_context);
    if(RTEST(value))
        CTX_FL_SET(debug_context, CTX_FL_TRACING);
    else
        CTX_FL_UNSET(debug_context, CTX_FL_TRACING);
    return value;
}

/*
 *   call-seq:
 *      context.ignore -> bool
 *   
 *   Returns the ignore flag for the current context.
 */
static VALUE
context_ignore(VALUE self)
{
    debug_context_t *debug_context;

    debug_check_started();

    Data_Get_Struct(self, debug_context_t, debug_context);
    return CTX_FL_TEST(debug_context, CTX_FL_IGNORE) ? Qtrue : Qfalse;
}

/*
 *   call-seq:
 *      context.tracking = bool
 *   
 *   Controls the ignore flag for this context.
 */
static VALUE
context_set_ignore(VALUE self, VALUE value)
{
    debug_context_t *debug_context;

    debug_check_started();

    Data_Get_Struct(self, debug_context_t, debug_context);
    if(RTEST(value))
        CTX_FL_SET(debug_context, CTX_FL_IGNORE);
    else
        CTX_FL_UNSET(debug_context, CTX_FL_IGNORE);
    return value;
}

/*
 *   call-seq:
 *      frame.file -> string
 *   
 *   Returns the name of the file.
 */
static VALUE
frame_file(VALUE self)
{
    debug_frame_t *debug_frame;

    Data_Get_Struct(self, debug_frame_t, debug_frame);
    return debug_frame->file;
}

/*
 *   call-seq:
 *      frame.line -> int
 *   
 *   Returns the line number in the file.
 */
static VALUE
frame_line(VALUE self)
{
    debug_frame_t *debug_frame;

    Data_Get_Struct(self, debug_frame_t, debug_frame);
    return debug_frame->line;
}

/*
 *   call-seq:
 *      frame.binding -> binding
 *   
 *   Returns the binding captured at the moment this frame was created.
 */
static VALUE
frame_binding(VALUE self)
{
    debug_frame_t *debug_frame;

    Data_Get_Struct(self, debug_frame_t, debug_frame);
    return debug_frame->binding;
}

/*
 *   call-seq:
 *      frame.id -> sym
 *   
 *   Returns the sym of the called method.
 */
static VALUE
frame_id(VALUE self)
{
    debug_frame_t *debug_frame;

    Data_Get_Struct(self, debug_frame_t, debug_frame);
    return debug_frame->id ? ID2SYM(debug_frame->id): Qnil;
}

/*
 *   call-seq:
 *      breakpoint.source -> string
 *   
 *   Returns a source of the breakpoint.
 */
static VALUE
breakpoint_source(VALUE self)
{
    debug_breakpoint_t *breakpoint;

    Data_Get_Struct(self, debug_breakpoint_t, breakpoint);
    return breakpoint->source;
}

/*
 *   call-seq:
 *      breakpoint.pos -> string or int
 *   
 *   Returns a position of this breakpoint.
 */
static VALUE
breakpoint_pos(VALUE self)
{
    debug_breakpoint_t *breakpoint;

    Data_Get_Struct(self, debug_breakpoint_t, breakpoint);
    return breakpoint->pos;
}

/*
 *   call-seq:
 *      breakpoint.expr -> string
 *   
 *   Returns a codition expression when this breakpoint should be activated.
 */
static VALUE
breakpoint_expr(VALUE self)
{
    debug_breakpoint_t *breakpoint;

    Data_Get_Struct(self, debug_breakpoint_t, breakpoint);
    return breakpoint->expr;
}

/*
 *   Document-class: Context
 *   
 *   == Summary
 *   
 *   Debugger keeps a single instance of this class for each Ruby thread. 
 *   This class provides access to stack frames (see Frame class). Also it
 *   gives you ability to step thought the code.
 */
static void
Init_context()
{
    cContext = rb_define_class_under(mDebugger, "Context", rb_cObject);
    rb_define_method(cContext, "stop_next=", context_stop_next, 1);
    rb_define_method(cContext, "step_over", context_step_over, -1);
    rb_define_method(cContext, "stop_frame=", context_stop_frame, 1);
    rb_define_method(cContext, "frames", context_frames, 0);
    rb_define_method(cContext, "thread", context_thread, 0);
    rb_define_method(cContext, "thnum", context_thnum, 0);
    rb_define_method(cContext, "suspend", context_suspend, 0);
    rb_define_method(cContext, "resume", context_resume, 0);
    rb_define_method(cContext, "tracing", context_tracing, 0);
    rb_define_method(cContext, "tracing=", context_set_tracing, 1);
    rb_define_method(cContext, "ignore", context_ignore, 0);
    rb_define_method(cContext, "ignore=", context_set_ignore, 1);
}

/*
 *   Document-class: Frame
 *   
 *   == Summary
 *   
 *   This class holds infomation about a particular call frame.
 */
static void
Init_frame()
{
    cFrame = rb_define_class_under(cContext, "Frame", rb_cObject);
    rb_define_method(cFrame, "file", frame_file, 0);
    rb_define_method(cFrame, "line", frame_line, 0);
    rb_define_method(cFrame, "binding", frame_binding, 0);
    rb_define_method(cFrame, "id", frame_id, 0);
}

/*
 *   Document-class: Breakpoint
 *   
 *   == Summary
 *   
 *   This class represents a breakpoint. It defines position of the breakpoint and 
 *   condition when this breakpoint should be triggered.
 */
static void
Init_breakpoint()
{
    cBreakpoint = rb_define_class_under(mDebugger, "Breakpoint", rb_cObject);
    rb_define_method(cBreakpoint, "source", breakpoint_source, 0);
    rb_define_method(cBreakpoint, "pos", breakpoint_pos, 0);
    rb_define_method(cBreakpoint, "expr", breakpoint_expr, 0);
}

/*
 *   Document-class: Debugger
 *   
 *   == Summary
 *   
 *   This is a singleton class allows controlling the debugger. Use it to start/stop debugger, 
 *   set/remove breakpoints, etc.
 */
#if defined(_WIN32)
__declspec(dllexport) 
#endif
void
Init_ruby_debug()
{
    mDebugger = rb_define_module("Debugger");
    rb_define_const(mDebugger, "VERSION", rb_str_new2(DEBUG_VERSION));
    rb_define_module_function(mDebugger, "start", debug_start, 0);
    rb_define_module_function(mDebugger, "stop", debug_stop, 0);
    rb_define_module_function(mDebugger, "started?", debug_is_started, 0);
    rb_define_module_function(mDebugger, "breakpoints", debug_breakpoints, 0);
    rb_define_module_function(mDebugger, "add_breakpoint", debug_add_breakpoint, -1);
    rb_define_module_function(mDebugger, "catchpoint", debug_catchpoint, 0);
    rb_define_module_function(mDebugger, "catchpoint=", debug_set_catchpoint, 1);
    rb_define_module_function(mDebugger, "last_context", debug_last_interrupted, 0);
    rb_define_module_function(mDebugger, "contexts", debug_contexts, 0);
    rb_define_module_function(mDebugger, "current_context", debug_current_context, 0);
    rb_define_module_function(mDebugger, "suspend", debug_suspend, 0);
    rb_define_module_function(mDebugger, "resume", debug_resume, 0);
    rb_define_module_function(mDebugger, "tracing", debug_tracing, 0);
    rb_define_module_function(mDebugger, "tracing=", debug_set_tracing, 1);
    rb_define_module_function(mDebugger, "debug_load", debug_debug_load, 1);
    rb_define_module_function(mDebugger, "skip", debug_skip, 0);
    rb_define_module_function(mDebugger, "debug_at_exit", debug_at_exit, 0);
    rb_define_module_function(mDebugger, "post_mortem?", debug_post_mortem, 0);
    rb_define_module_function(mDebugger, "post_mortem=", debug_set_post_mortem, 1);
    
    cThreadsTable = rb_define_class_under(mDebugger, "ThreadsTable", rb_cObject);
    
    Init_context();
    Init_frame();
    Init_breakpoint();

    idAtLine       = rb_intern("at_line");
    idAtBreakpoint = rb_intern("at_breakpoint");
    idAtCatchpoint = rb_intern("at_catchpoint");
    idAtTracing    = rb_intern("at_tracing");
    idBinding      = rb_intern("binding");
    idBasename     = rb_intern("basename");
    idEval         = rb_intern("eval");
    idList         = rb_intern("list");
    idClear        = rb_intern("clear");
    idIndex        = rb_intern("index");

    file_separator = rb_eval_string("File::SEPARATOR");
    alt_file_separator = rb_eval_string("File::ALT_SEPARATOR");

    rb_global_variable(&threads_tbl);
    rb_global_variable(&breakpoints);
    rb_global_variable(&catchpoint);
    rb_global_variable(&locker);
    rb_global_variable(&file_separator);
    rb_global_variable(&alt_file_separator);
}
