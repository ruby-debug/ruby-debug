/*
 * header & license
 * Copyright (c) 2007 Martin Krauskopf
 * Copyright (c) 2007 Peter Brant
 * 
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 * 
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
package org.jruby.debug;

import java.io.File;
import java.util.Iterator;
import java.util.Map;

import org.jruby.MetaClass;
import org.jruby.Ruby;
import org.jruby.RubyArray;
import org.jruby.RubyBinding;
import org.jruby.RubyKernel;
import org.jruby.RubyString;
import org.jruby.RubyThread;
import org.jruby.debug.DebugContext.StopReason;
import org.jruby.debug.DebugFrame.Info;
import org.jruby.debug.Debugger.DebugContextPair;
import org.jruby.exceptions.RaiseException;
import org.jruby.runtime.Block;
import org.jruby.runtime.EventHook;
import org.jruby.runtime.ThreadContext;
import org.jruby.runtime.builtin.IRubyObject;

final class DebugEventHook implements EventHook {

    private final Debugger debugger;
    private final Ruby runtime;
    
    private int hookCount;
    private int lastDebuggedThnum;
    private int lastCheck;
    
    private boolean inDebugger;

    public DebugEventHook(final Debugger debugger, final Ruby runtime) {
        this.debugger = debugger;
        lastDebuggedThnum = -1;
        this.runtime = runtime;
    }

    @SuppressWarnings("fallthrough")
    public void event(final ThreadContext tCtx, final int event, final String file, final int line0,
            final String methodName, final IRubyObject klass) {
        boolean needsSuspend = false;
        
        RubyThread currThread;
        DebugContextPair contexts;
        
        currThread = tCtx.getThread();
        contexts = debugger.threadContextLookup(currThread, true);
        
        // return if thread is marked as 'ignored'. debugger's threads are marked this way
        if (contexts.debugContext.isIgnored()) {
            return;
        }

        /* ignore a skipped section of code */
        if (contexts.debugContext.isSkipped()) {
            cleanUp(contexts.debugContext);
            return;
        }
        
        needsSuspend = contexts.debugContext.isSuspended();
        
        if (needsSuspend) {
            RubyThread.stop(currThread);
        }
        
        synchronized (this) {
            if (isInDebugger()) {
                return;
            }
            //dumpEvent(event, file, line0, methodName, klass);
            setInDebugger(true);
            try {
                processEvent(tCtx, event, file, line0, methodName, klass, contexts);
            } finally {
                setInDebugger(false);
            }
        }
    }

    private void processEvent(final ThreadContext tCtx, final int event, final String file, final int line0, 
            final String methodName, final IRubyObject klass, DebugContextPair contexts) {
        // one-based; jruby by default passes zero-based
        int line = line0 + 1;
        hookCount++;
        Ruby runtime = tCtx.getRuntime();
        IRubyObject breakpoint = getNil();
        IRubyObject binding = getNil();
        IRubyObject context = contexts.context;
        DebugContext debugContext = contexts.debugContext;

//        debug("jrubydebug> %s:%d [%s] %s\n", file, line, EVENT_NAMES[event], methodName);

        boolean moved = false;
        if (debugContext.getLastLine() != line || debugContext.getLastFile() == null || !debugContext.getLastFile().equals(file)) {
            debugContext.setEnableBreakpoint(true);
            moved = true;
        }
        //        else if(event != RUBY_EVENT_RETURN && event != RUBY_EVENT_C_RETURN) {
        //            if(debug == Qtrue)
        //                fprintf(stderr, "nodeless [%s] %s\n", get_event_name(event), rb_id2name(methodName));
        //            goto cleanup;
        //        } else {
        //            if(debug == Qtrue)
        //                fprintf(stderr, "nodeless [%s] %s\n", get_event_name(event), rb_id2name(methodName));
        //        }
        if (event != RUBY_EVENT_LINE) {
            debugContext.setStepped(true);
        }

        switch (event) {
            case RUBY_EVENT_LINE:
                if (debugContext.getStackSize() == 0) {
                    saveCallFrame(event, tCtx, file, line, methodName, debugContext);
                } else {
                    setFrameSource(event, debugContext, tCtx, file, line, methodName);
                }
                if (debugger.isTracing() || debugContext.isTracing()) {
                    IRubyObject[] args = new IRubyObject[]{
                        runtime.newString(file),
                        runtime.newFixnum(line)
                    };
                    context.callMethod(tCtx, DebugContext.AT_TRACING, args);
                }
                if (debugContext.getDestFrame() == -1 || debugContext.getStackSize() == debugContext.getDestFrame()) {
                    if (moved || !debugContext.isForceMove()) {
                        debugContext.setStopNext(debugContext.getStopNext() - 1);
                    }
                    if (debugContext.getStopNext() < 0) {
                        debugContext.setStopNext(-1);
                    }
                    if (moved || (debugContext.isStepped() && !debugContext.isForceMove())) {
                        debugContext.setStopLine(debugContext.getStopLine() - 1);
                        debugContext.setStepped(false);
                    }
                } else if (debugContext.getStackSize() < debugContext.getDestFrame()) {
                    debugContext.setStopNext(0);
                }

                if (debugContext.getStopNext() == 0 || debugContext.getStopLine() == 0 ||
                        !(breakpoint = checkBreakpointsByPos(debugContext, file, line)).isNil()) {
                    binding = (tCtx != null ? RubyBinding.newBinding(runtime) : getNil());
                    saveTopBinding(debugContext, binding);

                    debugContext.setStopReason(DebugContext.StopReason.STEP);

                    /* Check breakpoint expression. */
                    if (!breakpoint.isNil()) {
                        if (!checkBreakpointExpression(breakpoint, binding)) {
                            break;
                        }
                        if (!checkBreakpointHitCondition(breakpoint)) {
                            break;
                        }
                        if (breakpoint != debugContext.getBreakpoint()) {
                            debugContext.setStopReason(DebugContext.StopReason.BREAKPOINT);
                            context.callMethod(tCtx, DebugContext.AT_BREAKPOINT, breakpoint);
                        } else {
                            debugContext.setBreakpoint(getNil());
                        }
                    }

                    /* reset all pointers */
                    debugContext.setDestFrame(-1);
                    debugContext.setStopLine(-1);
                    debugContext.setStopNext(-1);
                    callAtLine(tCtx, context, debugContext, runtime, file, line);
                }
                break;
            case RUBY_EVENT_CALL:
                saveCallFrame(event, tCtx, file, line, methodName, debugContext);
                breakpoint = checkBreakpointsByMethod(debugContext, klass, methodName);
                if (!breakpoint.isNil()) {
                    DebugFrame debugFrame = getTopFrame(debugContext);
                    if (debugFrame != null) {
                        binding = debugFrame.getBinding();
                    }
                    if (!binding.isNil() && tCtx != null) {
                        binding = (tCtx != null ? RubyBinding.newBinding(runtime) : getNil());
                    }
                    saveTopBinding(debugContext, binding);

                    if(!checkBreakpointExpression(breakpoint, binding))
                        break;
                    if(!checkBreakpointHitCondition(breakpoint))
                        break;
                    if (breakpoint != debugContext.getBreakpoint()) {
                        debugContext.setStopReason(DebugContext.StopReason.BREAKPOINT);
                        context.callMethod(tCtx, DebugContext.AT_BREAKPOINT, breakpoint);
                    } else {
                        debugContext.setBreakpoint(getNil());
                    }
                    callAtLine(tCtx, context, debugContext, runtime, file, line);
                }
                break;
            case RUBY_EVENT_C_CALL:
                if(cCallNewFrameP(klass, methodName)) {
                    saveCallFrame(event, tCtx, file, line, methodName, debugContext);
                } else {
                    setFrameSource(event, debugContext, tCtx, file, line, methodName);
                }
                break;
            case RUBY_EVENT_C_RETURN:
                /* note if a block is given we fall through! */
                if (!cCallNewFrameP(klass, methodName)) {
                    break;
                }
            case RUBY_EVENT_RETURN:
            case RUBY_EVENT_END:
                if (debugContext.getStackSize() == debugContext.getStopFrame()) {
                    debugContext.setStopNext(1);
                    debugContext.setStopFrame(0);
                }
                while (debugContext.getStackSize() > 0) {
                    DebugFrame topFrame = debugContext.popFrame();
                    String origMethodName = topFrame.getOrigMethodName();
                    if ((origMethodName == null && methodName == null) ||
                            (origMethodName != null && origMethodName.equals(methodName))) {
                        break;
                    }
                }
                debugContext.setEnableBreakpoint(true);
                break;
            case RUBY_EVENT_CLASS:
                resetFrameMid(debugContext);
                saveCallFrame(event, tCtx, file, line, methodName, debugContext);
                break;
            case RUBY_EVENT_RAISE:
//                throw new UnsupportedOperationException("not implemented yet");

//                setFrameSource(event, debugContext, tCtx, file, line, methodName);
//
//                if (debugger.isPostMortem() && tCtx != null) {
//                    System.err.println("FIXME> IMPLEMENT ME: postMorten in DebugEventHook");
////                    binding = createBinding(tCtx);
////                    rbIvarSet(rubyErrinfo, rbIntern("@_DebugFile"), rbStrNew2(file));
////                    rbIvarSet(rubyErrinfo, rbIntern("@_DebugLine"), INT2FIX(line));
////                    rbIvarSet(rubyErrinfo, rbIntern("@_DebugBinding"), binding);
////                    rbIvarSet(rubyErrinfo, rbIntern("@_DebugContext"), debugContextDup(debugContext));
//                }
//
//                IRubyObject expnClass = rbObjClass(rubyErrinfo);
//                if( !NIL_P(rbClassInheritedP(expnClass, rbESystemExit)) )
//                {
//                    debugStop(mDebugger);
//                    break;
//                }
//
//                if(catchpoint == Qnil)
//                    break;
//
//                IRubyObject ancestors = rbModAncestors(expnClass);
//                IRubyObject aclass;
//                for(int i = 0; i < RARRAY(ancestors).getLen(); i++)
//                {
//                    aclass = rbAryEntry(ancestors, i);
//                    if(rbStrCmp(rbModName(aclass), catchpoint) == 0)
//                    {
//                        debugContext.getStopReason() = CTX_STOP_CATCHPOINT;
//                        rbFuncall(context, idAtCatchpoint, 1, rubyErrinfo);
//                        if(tCtx && binding == Qnil)
//                            binding = createBinding(tCtx);
//                        saveTopBinding(debugContext, binding);
//                        callAtLine(context, debugContext, rbStrNew2(file), INT2FIX(line));
//                        break;
//                    }
//                }
//
//                break;
        }
        cleanUp(debugContext);
    }

    private IRubyObject getNil() {
        return runtime.getNil();
    }

    private IRubyObject getBreakpoints() {
        return debugger.getBreakpoints();
    }

    private void cleanUp(DebugContext debugContext) {
        debugContext.setStopReason(StopReason.NONE);

        /* check that all contexts point to alive threads */
        if(hookCount - lastCheck > 3000) {
            checkThreadContexts();
            lastCheck = hookCount;
        }
    }

    public boolean isInterestedInEvent(int event) {
        return true;
    }

    /*
    private void debug(final String format, final Object... args) {
        System.err.printf(format, args);
    }

    private void dumpEvent(int event, String file, int line, String name, IRubyObject klass) {
        System.out.println("DEBUG> event: \"" + EVENT_NAMES[event] + '\"');
        System.out.println("DEBUG>   file: \"" + file + '\"');
        System.out.println("DEBUG>   line: \"" + line + '\"');
        System.out.println("DEBUG>   name: \"" + name + '\"');
//        System.out.println("DEBUG>   klass: \"" + klass + '\"');
    }
    */

    private void saveCallFrame(final int event, final ThreadContext tCtx, final String file,
            final int line, final String methodName, final DebugContext debugContext) {

        IRubyObject binding = (debugger.isKeepFrameBinding()) ? RubyBinding.newBinding(tCtx.getRuntime()) : tCtx.getRuntime().getNil();

        DebugFrame debugFrame = new DebugFrame();
        debugFrame.setArgc(tCtx.getCurrentScope().getArgValues());
        debugFrame.setFile(file);
        debugFrame.setLine(line);
        debugFrame.setBinding(binding);
        debugFrame.setMethodName(methodName);
        debugFrame.setOrigMethodName(methodName);
        debugFrame.setDead(false);
        debugFrame.setSelf(tCtx.getFrameSelf());
        Info info = debugFrame.getInfo();
        info.setFrame(tCtx.getCurrentFrame());
        info.setScope(tCtx.getCurrentScope().getStaticScope());
        info.setDynaVars(event == RUBY_EVENT_LINE ? tCtx.getCurrentScope() : null);
        debugContext.addFrame(debugFrame);
        if (debugger.isTrackFrameArgs()) {
            copyScalarArgs(debugFrame);
        }
    }

    /** Save scalar arguments or a class name. */
    private void copyScalarArgs(final DebugFrame debugFrame) {
        throw new UnsupportedOperationException("not implemented yet");
//            int i;
//            StaticScope ruby_scope = null;
//            Object tbl = null;
//            if (tbl != null && ruby_scope.getVariables().length > 0) {
//                int n = *tbl++;
//                if (debug_frame.argc+2 < n) n = debug_frame.argc+2;
//                debug_frame.arg_ary = rb_ary_new2(n);
//                for (i=2; i<n; i++)
//                {
//                    /* skip flip states */
//                    if (rb_is_local_id(tbl[i]))
//                    {
//                        final char *name = rb_id2name(tbl[i]);
//                        VALUE val = rb_eval_string (name);
//                        if (arg_value_is_small(val))
//                            rb_ary_push(debug_frame.arg_ary, val);
//                        else
//                            rb_ary_push(debug_frame.arg_ary,
//                                    rb_str_new2(rb_obj_classname(val)));
//                    }
//                }
//            }
    }

    private void setFrameSource(int event, DebugContext debug_context, ThreadContext tCtx,
            String file, int line, String methodName) {
        DebugFrame topFrame = getTopFrame(debug_context);
        if (topFrame != null) {
            topFrame.setSelf(tCtx.getFrameSelf());
            topFrame.setFile(file);
            topFrame.setLine(line);
            topFrame.setMethodName(methodName);
            topFrame.getInfo().setDynaVars(event == RUBY_EVENT_C_CALL ? null : tCtx.getCurrentScope());
        }
    }

    private DebugFrame getTopFrame(final DebugContext debugContext) {
        if (debugContext.getStackSize() == 0) {
            return null;
        } else {
            return debugContext.getTopFrame();
        }
    }

    private IRubyObject checkBreakpointsByPos(DebugContext debugContext, String file, int line) {
        if (!debugContext.isEnableBreakpoint()) {
            return getNil();
        }
        if (checkBreakpointByPos(debugContext.getBreakpoint(), file, line)) {
            return debugContext.getBreakpoint();
        }
        RubyArray arr = getBreakpoints().convertToArray();
        if (arr.isEmpty()) {
            return getNil();
        }
        for (int i = 0; i < arr.size(); i++) {
            IRubyObject breakpoint = arr.entry(i);
            if (checkBreakpointByPos(breakpoint, file, line)) {
                return breakpoint;
            }
        }
        return getNil();
    }

    private boolean checkBreakpointByPos(IRubyObject breakpoint, String file, int line) {
        if (breakpoint.isNil()) {
            return false;
        }
        DebugBreakpoint debugBreakpoint = (DebugBreakpoint) breakpoint.dataGetStruct();
        if (debugBreakpoint.getType() != DebugBreakpoint.Type.POS) {
            return false;
        }
        if (debugBreakpoint.getPos().getLine() != line) {
            return false;
        }
        String source = ((RubyString) debugBreakpoint.getSource()).toString();
        String sourceName = new File(source).getName();
        String fileName = new File(file).getName();
        if (sourceName.equals(fileName)) {
            return true;
        }
        return false;
    }

    private IRubyObject checkBreakpointsByMethod(DebugContext debugContext,
            IRubyObject klass, String methodName) {
        if (!debugContext.isEnableBreakpoint()) {
            return getNil();
        }
        if (checkBreakpointByMethod(debugContext.getBreakpoint(), klass, methodName)) {
            return debugContext.getBreakpoint();
        }
        RubyArray arr = getBreakpoints().convertToArray();
        if (arr.isEmpty()) {
            return getNil();
        }
        for (int i = 0; i < arr.size(); i++) {
            IRubyObject breakpoint = arr.entry(i);
            if (checkBreakpointByMethod(breakpoint, klass, methodName)) {
                return breakpoint;
            }
        }
        return getNil();
    }

    private boolean checkBreakpointByMethod(IRubyObject breakpoint, IRubyObject klass,
            String methodName) {
        if (breakpoint.isNil()) {
            return false;
        }
        DebugBreakpoint debugBreakpoint = (DebugBreakpoint) breakpoint.dataGetStruct();
        if (debugBreakpoint.getType() != DebugBreakpoint.Type.METHOD) {
            return false;
        }
        if (! debugBreakpoint.getPos().getMethodName().equals(methodName)) {
            return false;
        }
        if (debugBreakpoint.getSource().asString().eql(klass.asString())) {
            return true;
        }
        return false;
    }

    private boolean checkBreakpointExpression(IRubyObject breakpoint, IRubyObject binding) {
        DebugBreakpoint debugBreakpoint = (DebugBreakpoint) breakpoint.dataGetStruct();
        if (debugBreakpoint.getExpr().isNil()) {
            return true;
        }
        
        try {
            IRubyObject result = RubyKernel.eval(
                    breakpoint, 
                    new IRubyObject[] { debugBreakpoint.getExpr(), binding },
                    Block.NULL_BLOCK);
            return result.isTrue();
        } catch (RaiseException e) {
            // XXX Seems like we should tell the user about this, but this how
            // ruby-debug behaves
            return false;
        }
    }

    private boolean checkBreakpointHitCondition(IRubyObject breakpoint) {
        DebugBreakpoint debugBreakpoint = (DebugBreakpoint) breakpoint.dataGetStruct();
        
        debugBreakpoint.setHitCount(debugBreakpoint.getHitCount()+1);
        
        if (debugBreakpoint.getHitCondition() == null) {
            return true;
        }
        
        switch (debugBreakpoint.getHitCondition()) {
        case NONE:
            return true;
        case GE:
            if (debugBreakpoint.getHitCount() >= debugBreakpoint.getHitValue()) {
                return true;
            }
            break;
        case EQ:
            if (debugBreakpoint.getHitCount() == debugBreakpoint.getHitValue()) {
                return true;
            }
            break;
        case MOD:
            if (debugBreakpoint.getHitCount() % debugBreakpoint.getHitValue() == 0) {
                return true;
            }
            break;
        }
        
        return false;
    }

    private void saveTopBinding(DebugContext context, IRubyObject binding) {
        DebugFrame debugFrame = getTopFrame(context);
        if (debugFrame != null) {
            debugFrame.setBinding(binding);
        }
    }

    private IRubyObject callAtLine(ThreadContext tCtx,
            IRubyObject context, DebugContext debugContext,
            Ruby runtime, String file, int line) {
        return callAtLine(tCtx, context, debugContext, runtime.newString(file), runtime.newFixnum(line));
    }
    
    private IRubyObject callAtLine(ThreadContext tCtx,
            IRubyObject context, DebugContext debugContext,
            IRubyObject file, IRubyObject line) {
        lastDebuggedThnum = debugContext.getThnum();
        saveCurrentPosition(debugContext);
        IRubyObject[] args = new IRubyObject[]{
            file,
            line
        };
        return context.callMethod(tCtx, DebugContext.AT_LINE, args);
    }

    private void saveCurrentPosition(final DebugContext debugContext) {
        DebugFrame debugFrame = getTopFrame(debugContext);
        if (debugFrame == null) {
            return;
        }
        debugContext.setLastFile(debugFrame.getFile());
        debugContext.setLastLine(debugFrame.getLine());
        debugContext.setEnableBreakpoint(false);
        debugContext.setStepped(false);
        debugContext.setForceMove(false);
    }

    @SuppressWarnings("unchecked")
    private void checkThreadContexts() {
        Map<RubyThread, IRubyObject> threadsTable = (Map<RubyThread, IRubyObject>) debugger.getThreadsTbl().dataGetStruct();
        for (Iterator<Map.Entry<RubyThread, IRubyObject>> it = threadsTable.entrySet().iterator(); it.hasNext();) {
            Map.Entry<RubyThread, IRubyObject> entry = it.next();
            if (runtime.getFalse().eql(entry.getKey().alive_p())) {
                it.remove();
            }
        }

    }

    private boolean cCallNewFrameP(IRubyObject klass, String methodName) {
        klass = realClass(klass);
        // TODO - block_given?
//        if(rb_block_given_p()) return true;
        String cName = klass.getType().getName();
        return "Proc".equals(cName) || "RubyKernel".equals(cName) || "Module".equals(cName);
    }

    private IRubyObject realClass(IRubyObject klass) {
        if (klass instanceof MetaClass) {
            return ((MetaClass)klass).getRealClass();
        }
        
        return klass;
    }

    private void resetFrameMid(DebugContext debugContext) {
        DebugFrame topFrame = getTopFrame(debugContext);
        if (topFrame != null) {
            topFrame.setMethodName("");
        }
    }

    private boolean isInDebugger() {
        return inDebugger;
    }

    private void setInDebugger(boolean inDebugger) {
        this.inDebugger = inDebugger;
    }
    
    int getLastDebuggedThnum() {
        return lastDebuggedThnum;
    }
}
