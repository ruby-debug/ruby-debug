/*
 * header & license
 * Copyright (c) 2007-2008 Martin Krauskopf
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

import org.jruby.*;
import org.jruby.debug.DebugContext.StopReason;
import org.jruby.debug.DebugFrame.Info;
import org.jruby.debug.Debugger.DebugContextPair;
import org.jruby.exceptions.RaiseException;
import org.jruby.runtime.Block;
import org.jruby.runtime.EventHook;
import org.jruby.runtime.RubyEvent;
import org.jruby.runtime.ThreadContext;
import org.jruby.runtime.builtin.IRubyObject;

import static org.jruby.runtime.RubyEvent.*;

final class DebugEventHook extends EventHook {

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

    @Override
    public boolean isInterestedInEvent(RubyEvent event) {
        return true;
    }

    @Override
    public void eventHandler(final ThreadContext tCtx, String event, final String file, final int line,
            final String methodName, final IRubyObject klass) {

        RubyThread currThread = tCtx.getThread();
        DebugContextPair contexts = debugger.threadContextLookup(currThread, true);

        // return if thread is marked as 'ignored'. debugger's threads are marked this way
        if (contexts.debugContext.isIgnored()) {
            return;
        }

        /* ignore a skipped section of code */
        if (contexts.debugContext.isSkipped()) {
            cleanUp(contexts.debugContext);
            return;
        }

        /** Ignore JRuby core classes by default. Consider option for enabling it. */
        if (Util.isJRubyCore(file)) {
            return;
        }
        
        if (contexts.debugContext.isSuspended()) {
            RubyThread.stop(tCtx, currThread);
        }
        
        synchronized (this) {
            if (isInDebugger()) {
                return;
            }
            setInDebugger(true);
            try {
                processEvent(tCtx, Util.typeForEvent(event), Util.relativizeToPWD(file), line, methodName, klass, contexts);
            } finally {
                setInDebugger(false);
            }
        }
    }

    @SuppressWarnings("fallthrough")
    private void processEvent(final ThreadContext tCtx, final RubyEvent event, final String file, final int line,
            final String methodName, final IRubyObject klass, DebugContextPair contexts) {
        if (debugger.isDebug()) {
            Util.logEvent(event, file, line, methodName, klass);
        }
        // one-based; jruby by default passes zero-based
        hookCount++;
        Ruby _runtime = tCtx.getRuntime();
        IRubyObject breakpoint = getNil();
        IRubyObject binding = getNil();
        IRubyObject context = contexts.context;
        DebugContext debugContext = contexts.debugContext;

//        debug("jrubydebug> %s:%d [%s] %s\n", file, line, EVENT_NAMES[event], methodName);

        boolean moved = false;
        if (!debugContext.isForceMove() ||
            debugContext.getLastLine() != line || debugContext.getLastFile() == null ||
            !Util.areSameFiles(debugContext.getLastFile(), file)) {
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
        if (LINE == event) {
            debugContext.setStepped(true);
        }

        switch (event) {
            case LINE:
                if (debugContext.getStackSize() == 0) {
                    saveCallFrame(event, tCtx, file, line, methodName, debugContext);
                } else {
                    updateTopFrame(event, debugContext, tCtx, file, line, methodName);
                }
                if (debugger.isTracing() || debugContext.isTracing()) {
                    IRubyObject[] args = new IRubyObject[]{
                        _runtime.newString(file),
                        _runtime.newFixnum(line)
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
                    binding = (tCtx != null ? RubyBinding.newBinding(_runtime, tCtx.currentBinding()) : getNil());
                    saveTopBinding(debugContext, binding);

                    debugContext.setStopReason(DebugContext.StopReason.STEP);

                    /* Check breakpoint expression. */
                    if (!breakpoint.isNil()) {
                        if (!checkBreakpointExpression(tCtx, breakpoint, binding)) {
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
                    callAtLine(tCtx, context, debugContext, _runtime, file, line);
                }
                break;
            case CALL:
                saveCallFrame(event, tCtx, file, line, methodName, debugContext);
                breakpoint = checkBreakpointsByMethod(debugContext, klass, methodName);
                if (!breakpoint.isNil()) {
                    DebugFrame debugFrame = getTopFrame(debugContext);
                    if (debugFrame != null) {
                        binding = debugFrame.getBinding();
                    }
                    if (tCtx != null && binding.isNil()) {
                        binding = RubyBinding.newBinding(_runtime, tCtx.currentBinding());
                    }
                    saveTopBinding(debugContext, binding);

                    if(!checkBreakpointExpression(tCtx, breakpoint, binding)) {
                        break;
                    }
                    if(!checkBreakpointHitCondition(breakpoint)) {
                        break;
                    }
                    if (breakpoint != debugContext.getBreakpoint()) {
                        debugContext.setStopReason(DebugContext.StopReason.BREAKPOINT);
                        context.callMethod(tCtx, DebugContext.AT_BREAKPOINT, breakpoint);
                    } else {
                        debugContext.setBreakpoint(getNil());
                    }
                    callAtLine(tCtx, context, debugContext, _runtime, file, line);
                }
                break;
            case C_CALL:
                if(cCallNewFrameP(klass)) {
                    saveCallFrame(event, tCtx, file, line, methodName, debugContext);
                } else {
                    updateTopFrame(event, debugContext, tCtx, file, line, methodName);
                }
                break;
            case C_RETURN:
                /* note if a block is given we fall through! */
                if (!cCallNewFrameP(klass)) {
                    break;
                }
            case RETURN:
            case END:
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
            case CLASS:
                resetTopFrameMethodName(debugContext);
                saveCallFrame(event, tCtx, file, line, methodName, debugContext);
                break;
            case RAISE:
                updateTopFrame(event, debugContext, tCtx, file, line, methodName);
                
                // XXX Implement post mortem debugging

                IRubyObject exception = _runtime.getGlobalVariables().get("$!");
                // Might happen if the current ThreadContext is within 'defined?'
                if (exception.isNil()) {
                    break;
                }
                
                if (_runtime.getClass("SystemExit").isInstance(exception)) {
                    // Can't do this because this unhooks the event hook causing
                    // a ConcurrentModificationException because the runtime
                    // is still iterating over event hooks.  Shouldn't really
                    // matter.  We're on our way out regardless.
                    
                    // debugger.stop(runtime);
                    break;
                }
                
                if (debugger.getCatchpoints().isNil() || debugger.getCatchpoints().isEmpty()) {
                    break;
                }
                
                RubyArray ancestors = exception.getType().ancestors(tCtx);
                int l = ancestors.getLength();
                for (int i = 0; i < l; i++) {
                    RubyModule module = (RubyModule)ancestors.get(i);
                    IRubyObject modName = module.name();
                    IRubyObject hitCount = debugger.getCatchpoints().op_aref(tCtx, modName);
                    if (!hitCount.isNil()) {
                        hitCount = _runtime.newFixnum(RubyFixnum.fix2int(hitCount) + 1);
                        debugger.getCatchpoints().op_aset(tCtx, modName, hitCount);
                        debugContext.setStopReason(DebugContext.StopReason.CATCHPOINT);
                        context.callMethod(tCtx, DebugContext.AT_CATCHPOINT, exception);
                        
                        DebugFrame debugFrame = getTopFrame(debugContext);
                        if (debugFrame != null) {
                            binding = debugFrame.getBinding();
                        }
                        if (tCtx != null && binding.isNil()) {
                            binding = RubyBinding.newBinding(_runtime, tCtx.currentBinding());
                        }
                        saveTopBinding(debugContext, binding);
                        callAtLine(tCtx, context, debugContext, _runtime, file, line);
                        
                        break;
                    }
                }
                break;
            default:
                throw new IllegalArgumentException("unknown event type: " + event);
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
            debugger.checkThreadContexts(runtime);
            lastCheck = hookCount;
        }
    }

    private void saveCallFrame(final RubyEvent event, final ThreadContext tCtx, final String file,
            final int line, final String methodName, final DebugContext debugContext) {

        IRubyObject binding = (debugger.isKeepFrameBinding()) ? RubyBinding.newBinding(tCtx.getRuntime(), tCtx.currentBinding()) : tCtx.getRuntime().getNil();

        DebugFrame debugFrame = new DebugFrame();
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
        info.setDynaVars(event == LINE ? tCtx.getCurrentScope() : null);
        debugContext.addFrame(debugFrame);
        if (debugger.isTrackFrameArgs()) {
            copyScalarArgs(tCtx, debugFrame);
        } else {
            debugFrame.setArgValues(runtime.getNil());
        }
    }
    
    private boolean isArgValueSmall(IRubyObject value) {
        return value == RubyObject.UNDEF ||
                value instanceof RubyFixnum ||
                value instanceof RubyFloat ||
                value instanceof RubyNil ||
                value instanceof RubyModule ||
                value instanceof RubyFile ||
                value instanceof RubyBoolean ||
                value instanceof RubySymbol;
    }

    /** Save scalar arguments or a class name. */
    private void copyScalarArgs(ThreadContext tCtx, DebugFrame debugFrame) {
        RubyArray args = runtime.newArray(tCtx.getCurrentScope().getArgValues());
        
        int len = args.getLength();
        for (int i = 0; i < len; i++) {
            IRubyObject obj = args.entry(i);
            if (obj == null) {
                obj = runtime.getNil();
            }
            if (! isArgValueSmall(obj)) {
                args.store(i, runtime.newString(obj.getType().getName()));
            }
        }
        
        debugFrame.setArgValues(args);
    }

    private void updateTopFrame(RubyEvent event, DebugContext debug_context, ThreadContext tCtx,
            String file, int line, String methodName) {
        DebugFrame topFrame = getTopFrame(debug_context);
        if (topFrame != null) {
            topFrame.setSelf(tCtx.getFrameSelf());
            topFrame.setFile(file);
            topFrame.setLine(line);
            topFrame.setMethodName(methodName);
            topFrame.getInfo().setDynaVars(tCtx.getCurrentScope());
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
        if (!debugBreakpoint.isEnabled()) {
            return false;
        }
        if (debugBreakpoint.getType() != DebugBreakpoint.Type.POS) {
            return false;
        }
        if (debugBreakpoint.getPos().getLine() != line) {
            return false;
        }
        String source = debugBreakpoint.getSource().toString();
        if (source.startsWith("./")) {
            source = source.substring(2);
        }
        if (file.startsWith("./")) {
            file = file.substring(2);
        }
        return source.endsWith(file) || file.endsWith(source);
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
        if (!debugBreakpoint.isEnabled()) {
            return false;
        }
        if (debugBreakpoint.getType() != DebugBreakpoint.Type.METHOD) {
            return false;
        }
        if (! debugBreakpoint.getPos().getMethodName().equals(methodName)) {
            return false;
        }
        RubyString source = debugBreakpoint.getSource().asString();
        if (source.eql(klass.asString())) {
            return true;
        }
        if (klass instanceof MetaClass && source.eql(((MetaClass)klass).getAttached().asString())) {
            return true;
        }
        return false;
    }

    private boolean checkBreakpointExpression(ThreadContext tCtx, IRubyObject breakpoint, IRubyObject binding) {
        DebugBreakpoint debugBreakpoint = (DebugBreakpoint) breakpoint.dataGetStruct();
        if (debugBreakpoint.getExpr().isNil()) {
            return true;
        }
        
        try {
            IRubyObject result = RubyKernel.eval(
                    tCtx,
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
            default:
                throw new IllegalArgumentException("unknown hit condition: " + debugBreakpoint.getHitCondition());
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
        return callAtLine(tCtx, context, debugContext,
                runtime.newString(file), runtime.newFixnum(line));
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

    private boolean cCallNewFrameP(IRubyObject klass) {
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

    private void resetTopFrameMethodName(DebugContext debugContext) {
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
