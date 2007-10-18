package org.jruby.debug;

import org.jruby.Ruby;
import org.jruby.RubyClass;
import org.jruby.RubyModule;
import org.jruby.debug.RubyDebugBaseLibrary.DebugThread;
import org.jruby.runtime.Block;
import org.jruby.runtime.CallbackFactory;
import org.jruby.runtime.ObjectAllocator;
import org.jruby.runtime.builtin.IRubyObject;

public final class DebuggerDef {

    static final String DEBUG_THREAD_NAME = "DebugThread";
    static final String CONTEXT_NAME = "Context";
    
    private static final String VERSION = "0.9.3";
    private static Debugger debugger;
    
    public static RubyModule createDebuggerModule(Ruby runtime) {
        
        /* Debugger module. */
        RubyModule debuggerMod = runtime.defineModule("Debugger");
        CallbackFactory callbackFactory = runtime.callbackFactory(DebuggerDef.class);
        debuggerMod.defineConstant("VERSION", runtime.newString(VERSION));
        debuggerMod.defineModuleFunction("start", callbackFactory.getSingletonMethod("start"));
        debuggerMod.defineModuleFunction("stop", callbackFactory.getSingletonMethod("stop"));
        debuggerMod.defineModuleFunction("started?", callbackFactory.getSingletonMethod("started_p"));
        debuggerMod.defineModuleFunction("breakpoints", callbackFactory.getSingletonMethod("breakpoints"));
        debuggerMod.defineModuleFunction("add_breakpoint", callbackFactory.getOptSingletonMethod("add_breakpoint"));
        debuggerMod.defineModuleFunction("remove_breakpoint", callbackFactory.getSingletonMethod("remove_breakpoint", IRubyObject.class));
        debuggerMod.defineModuleFunction("catchpoint", callbackFactory.getSingletonMethod("catchpoint"));
        debuggerMod.defineModuleFunction("catchpoint=", callbackFactory.getSingletonMethod("catchpoint_set", IRubyObject.class));
        debuggerMod.defineModuleFunction("last_context", callbackFactory.getSingletonMethod("last_context"));
        debuggerMod.defineModuleFunction("contexts", callbackFactory.getSingletonMethod("contexts"));
        debuggerMod.defineModuleFunction("current_context", callbackFactory.getSingletonMethod("current_context"));
        debuggerMod.defineModuleFunction("thread_context", callbackFactory.getSingletonMethod("thread_context", IRubyObject.class));
        debuggerMod.defineModuleFunction("suspend", callbackFactory.getSingletonMethod("suspend"));
        debuggerMod.defineModuleFunction("resume", callbackFactory.getSingletonMethod("resume"));
        debuggerMod.defineModuleFunction("tracing", callbackFactory.getSingletonMethod("tracing"));
        debuggerMod.defineModuleFunction("tracing=", callbackFactory.getSingletonMethod("tracing_set", IRubyObject.class));
        debuggerMod.defineModuleFunction("debug_load", callbackFactory.getOptSingletonMethod("debug_load"));
        debuggerMod.defineModuleFunction("skip", callbackFactory.getSingletonMethod("skip"));
        debuggerMod.defineModuleFunction("debug_at_exit", callbackFactory.getSingletonMethod("debug_at_exit"));
        debuggerMod.defineModuleFunction("post_mortem?", callbackFactory.getSingletonMethod("post_mortem_p"));
        debuggerMod.defineModuleFunction("post_mortem=", callbackFactory.getSingletonMethod("post_mortem_set", IRubyObject.class));
        debuggerMod.defineModuleFunction("keep_frame_binding?", callbackFactory.getSingletonMethod("keep_frame_binding_p"));
        debuggerMod.defineModuleFunction("keep_frame_binding=", callbackFactory.getSingletonMethod("keep_frame_binding_set", IRubyObject.class));
        debuggerMod.defineModuleFunction("debug", callbackFactory.getSingletonMethod("debug"));
        debuggerMod.defineModuleFunction("debug=", callbackFactory.getSingletonMethod("debug_set", IRubyObject.class));


        /* Debugger::ThreadsTable */
        RubyClass threadsTable = debuggerMod.defineClassUnder("ThreadsTable", runtime.getObject(), runtime.getObject().getAllocator());
        CallbackFactory dtCallbackFactory = runtime.callbackFactory(DebugThread.class);
        
        
        /* Debugger::DebugThread */
        RubyClass debugThread = debuggerMod.defineClassUnder(DEBUG_THREAD_NAME, runtime.getClass("Thread"), runtime.getClass("Thread").getAllocator());
        debugThread.getSingletonClass().defineMethod("inherited", dtCallbackFactory.getSingletonMethod("inherited", IRubyObject.class));

        
        /* Debugger::Breakpoint */
        CallbackFactory bCallbackFactory = runtime.callbackFactory(Breakpoint.class);
        RubyClass breakpoint = debuggerMod.defineClassUnder("Breakpoint", runtime.getObject(), BREAKPOINT_ALLOCATOR);
        breakpoint.defineMethod("id", bCallbackFactory.getMethod("id"));
        breakpoint.defineMethod("source", bCallbackFactory.getMethod("source"));
        breakpoint.defineMethod("source=", bCallbackFactory.getMethod("source_set", IRubyObject.class));
        breakpoint.defineMethod("pos", bCallbackFactory.getMethod("pos"));
        breakpoint.defineMethod("pos=", bCallbackFactory.getMethod("pos_set", IRubyObject.class));
        breakpoint.defineMethod("expr", bCallbackFactory.getMethod("expr"));
        breakpoint.defineMethod("expr=", bCallbackFactory.getMethod("expr_set", IRubyObject.class));
        breakpoint.defineMethod("hit_count", bCallbackFactory.getMethod("hit_count"));
        breakpoint.defineMethod("hit_value", bCallbackFactory.getMethod("hit_value"));
        breakpoint.defineMethod("hit_value=", bCallbackFactory.getMethod("hit_value_set", IRubyObject.class));
        breakpoint.defineMethod("hit_condition", bCallbackFactory.getMethod("hit_condition"));
        breakpoint.defineMethod("hit_condition=", bCallbackFactory.getMethod("hit_condition_set", IRubyObject.class));
        
        
        /* Debugger::Context */
        CallbackFactory cCallbackFactory = runtime.callbackFactory(Context.class);
        RubyClass context = debuggerMod.defineClassUnder(CONTEXT_NAME, runtime.getObject(), CONTEXT_ALLOCATOR);
        context.defineMethod("stop_next=", cCallbackFactory.getOptMethod("stop_next_set"));
        context.defineMethod("step", cCallbackFactory.getOptMethod("stop_next_set"));
        context.defineMethod("step_over", cCallbackFactory.getOptMethod("step_over"));
        context.defineMethod("stop_frame=", cCallbackFactory.getMethod("stop_frame_set", IRubyObject.class));
        context.defineMethod("thread", cCallbackFactory.getMethod("thread"));
        context.defineMethod("thnum", cCallbackFactory.getMethod("thnum"));
        context.defineMethod("stop_reason", cCallbackFactory.getMethod("stop_reason"));
        context.defineMethod("suspend", cCallbackFactory.getMethod("suspend"));
        context.defineMethod("suspended?", cCallbackFactory.getMethod("suspended_p"));
        context.defineMethod("resume", cCallbackFactory.getMethod("resume"));
        context.defineMethod("tracing", cCallbackFactory.getMethod("tracing"));
        context.defineMethod("tracing=", cCallbackFactory.getMethod("tracing_set", IRubyObject.class));
        context.defineMethod("ignored?", cCallbackFactory.getMethod("ignored_p"));
        context.defineMethod("frame_args", cCallbackFactory.getMethod("frame_args", IRubyObject.class));
        context.defineMethod("frame_binding", cCallbackFactory.getMethod("frame_binding", IRubyObject.class));
        context.defineMethod("frame_id", cCallbackFactory.getMethod("frame_method", IRubyObject.class));
        context.defineMethod("frame_method", cCallbackFactory.getMethod("frame_method", IRubyObject.class));
        context.defineMethod("frame_line", cCallbackFactory.getMethod("frame_line", IRubyObject.class));
        context.defineMethod("frame_file", cCallbackFactory.getMethod("frame_file", IRubyObject.class));
        context.defineMethod("frame_locals", cCallbackFactory.getMethod("frame_locals", IRubyObject.class));
        context.defineMethod("frame_self", cCallbackFactory.getMethod("frame_self", IRubyObject.class));
        context.defineMethod("frame_class", cCallbackFactory.getMethod("frame_class", IRubyObject.class));
        context.defineMethod("stack_size", cCallbackFactory.getMethod("stack_size"));
        context.defineMethod("dead?", cCallbackFactory.getMethod("dead_p"));
        context.defineMethod("breakpoint", cCallbackFactory.getMethod("breakpoint"));
        context.defineMethod("set_breakpoint", cCallbackFactory.getOptMethod("set_breakpoint"));

        // FIXME: some constants and global vars missing here...
        return debuggerMod;
    }
    
    private static Debugger debugger() {
        synchronized (DebuggerDef.class) {
            if (debugger == null) {
                debugger = new Debugger();
            }
        }
        return debugger;
    }
    
    /**
     * This method activates the debugger. If it's called without a block it
     * returns +true+, unless debugger was already started. If a block is given,
     * it starts debugger and yields to block. When the block is finished
     * executing it stops the debugger with Debugger.stop method.
     * <p>
     * <i>Note that if you want to stop debugger, you must call Debugger.stop as
     * many time as you called Debugger.start method.</i>
     * </p>
     */
    public static IRubyObject start(IRubyObject recv, Block block) {
        return debugger().start(recv, block);
    }

    /**
     * This method deacivates the debugger. It returns +true+ if the debugger is
     * deacivated, otherwise it returns +false+.
     * <p>
     * <i>Note that if you want to stop debugger, you must call Debugger.stop as
     * many time as you called Debugger.start method.</i>
     * </p>
     */
    public static IRubyObject stop(IRubyObject recv, Block block) {
        boolean stopped = debugger().stop(recv.getRuntime());
        return Util.toRBoolean(recv, stopped);
    }
    
    public static IRubyObject started_p(IRubyObject recv, Block block) {
        return Util.toRBoolean(recv, debugger().isStarted());
    }

    public static IRubyObject breakpoints(IRubyObject recv, Block block) {
        return debugger().getBreakpoints();
    }

    public static IRubyObject add_breakpoint(IRubyObject recv, IRubyObject[] args, Block block) {
        return debugger().addBreakpoint(recv, args);
    }

    public static IRubyObject remove_breakpoint(IRubyObject recv, IRubyObject breakpointId, Block block) {
        return debugger().removeBreakpoint(recv, breakpointId);
    }

    public static IRubyObject catchpoint(IRubyObject recv, Block block) {
        throw new UnsupportedOperationException("not implemented yet");
    }

    public static IRubyObject catchpoint_set(IRubyObject recv, IRubyObject catchpoint, Block block) {
        throw new UnsupportedOperationException("not implemented yet");
    }

    public static IRubyObject last_context(IRubyObject recv, Block block) {
        return debugger().lastInterrupted(recv);
    }

    public static IRubyObject contexts(IRubyObject recv, Block block) {
        return debugger().getDebugContexts(recv);
    }

    public static IRubyObject current_context(IRubyObject recv, Block block) {
        return debugger().getCurrentContext(recv);
    }

    public static IRubyObject thread_context(IRubyObject recv, IRubyObject context, Block block) {
        throw new UnsupportedOperationException("not implemented yet");
    }

    public static IRubyObject suspend(IRubyObject recv, Block block) {
        throw new UnsupportedOperationException("not implemented yet");
    }

    public static IRubyObject resume(IRubyObject recv, Block block) {
        throw new UnsupportedOperationException("not implemented yet");
    }

    public static IRubyObject tracing(IRubyObject recv, Block block) {
        throw new UnsupportedOperationException("not implemented yet");
    }

    public static IRubyObject tracing_set(IRubyObject recv, IRubyObject tracing, Block block) {
        debugger().setTracing(Util.toBoolean(tracing));
        return Util.nil(recv);
    }
    
    public static IRubyObject debug_load(IRubyObject recv, IRubyObject[] args, Block block) {
        debugger().load(recv, args);
        return Util.nil(recv);
    }

    public static IRubyObject skip(IRubyObject recv, Block block) {
        throw new UnsupportedOperationException("not implemented yet");
    }

    public static IRubyObject debug_at_exit(IRubyObject recv, Block block) {
        System.err.println("FIXME> IMPLEMENT ME: DebuggerDef.debug_at_exit()");
        return Util.nil(recv);
    }

    public static IRubyObject post_mortem_p(IRubyObject recv, Block block) {
        return Util.toRBoolean(recv, debugger().isPostMortem());
    }

    public static IRubyObject post_mortem_set(IRubyObject recv, IRubyObject postMortem, Block block) {
        debugger().setPostMortem(Util.toBoolean(postMortem));
        return Util.nil(recv);
    }

    public static IRubyObject keep_frame_binding_p(IRubyObject recv, Block block) {
        throw new UnsupportedOperationException("not implemented yet");
    }

    public static IRubyObject keep_frame_binding_set(IRubyObject recv, IRubyObject keepFrameBinding, Block block) {
        debugger().setKeepFrameBinding(Util.toBoolean(keepFrameBinding));
        return Util.nil(recv);
    }

    public static IRubyObject debug(IRubyObject recv, Block block) {
        return Util.toRBoolean(recv, debugger().isDebug());
    }

    public static IRubyObject debug_set(IRubyObject recv, IRubyObject debug, Block block) {
        debugger().setDebug(Util.toBoolean(debug));
        return Util.nil(recv);
    }

    private static ObjectAllocator BREAKPOINT_ALLOCATOR = new ObjectAllocator() {
        public IRubyObject allocate(Ruby runtime, RubyClass klass) {
            return new Breakpoint(runtime, klass);
        }
    };

    private static ObjectAllocator CONTEXT_ALLOCATOR = new ObjectAllocator() {
        public IRubyObject allocate(Ruby runtime, RubyClass klass) {
            return new Context(runtime, klass, debugger());
        }
    };
}
