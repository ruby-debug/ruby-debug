/*
 * header & license
 * Copyright (c) 2007-2008 Martin Krauskopf
 * Copyright (c) 2007 Christopher Nelson
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

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import org.jruby.Ruby;
import org.jruby.RubyArray;
import org.jruby.RubyClass;
import org.jruby.RubyFixnum;
import org.jruby.RubyHash;
import org.jruby.RubyNumeric;
import org.jruby.RubyObject;
import org.jruby.RubyString;
import org.jruby.anno.JRubyMethod;
import org.jruby.parser.StaticScope;
import org.jruby.runtime.Arity;
import org.jruby.runtime.Block;
import org.jruby.runtime.DynamicScope;
import org.jruby.runtime.builtin.IRubyObject;

public class Context extends RubyObject {
    private static final long serialVersionUID = 1L;
    
    private final Debugger debugger;

    Context(Ruby runtime, RubyClass type, Debugger debugger) {
        super(runtime, type);
        this.debugger = debugger;
    }

    DebugContext debugContext() {
        return (DebugContext) dataGetStruct();
    }

    @JRubyMethod(name={"stop_next=","step"}, required=1, optional=1)
    public IRubyObject stop_next_set(IRubyObject[] args, Block block) {
        Ruby rt = getRuntime();
        checkStarted();
        IRubyObject force;
        if (Arity.checkArgumentCount(rt, args, 1, 2) == 2) {
            force = args[1];
        } else {
            force = rt.getNil();
        }
        IRubyObject steps = args[0];

        if (RubyFixnum.fix2int(steps) < 0) {
            rt.newRuntimeError("Steps argument can't be negative.");
        }

        DebugContext debug_context = debugContext();
        debug_context.setStopNext(RubyFixnum.fix2int(steps));
        debug_context.setForceMove(!force.isNil() && force.isTrue());
        return steps;
    }

    @JRubyMethod(name="step_over", required=1, optional=2)
    public IRubyObject step_over(IRubyObject[] rawArgs, Block block) {
        Ruby rt = getRuntime();
        checkStarted();
        DebugContext debugContext = debugContext();
        if (debugContext.getStackSize() == 0) {
            rt.newRuntimeError("No frames collected.");
        }

        IRubyObject[] args = Arity.scanArgs(rt, rawArgs, 1, 2);
        
        IRubyObject lines = args[0];
        IRubyObject frame = args[1];
        IRubyObject force = args[2];
        
        debugContext.setStopLine(RubyFixnum.fix2int(lines));
        debugContext.setStepped(false);
        if (frame.isNil()) {
            debugContext.setDestFrame(debugContext.getStackSize());
        } else {
            int frameInt = checkFrameNumber(frame);
            debugContext.setDestFrame(debugContext.getStackSize() - frameInt);
        }
        debugContext.setForceMove(force.isTrue());
        return rt.getNil();
    }

    @JRubyMethod(name="stop_frame=", required=1)
    public IRubyObject stop_frame_set(IRubyObject rFrameNo, Block block) {
        checkStarted();
        DebugContext debugContext = debugContext();
        int frameNo = RubyNumeric.fix2int(rFrameNo);
        if (frameNo < 0 || frameNo >= debugContext.getStackSize()) {
            getRuntime().newRuntimeError("Stop frame is out of range.");
        }
        debugContext.setStopFrame(debugContext.getStackSize() - frameNo);
        
        return rFrameNo;
    }

    @JRubyMethod(name="thread")
    public IRubyObject thread(Block block) {
        checkStarted();
        return debugContext().getThread();
    }

    @JRubyMethod(name="thnum")
    public IRubyObject thnum(Block block) {
        return RubyFixnum.newFixnum(getRuntime(), debugContext().getThnum());
    }

    @JRubyMethod(name="stop_reason")
    public IRubyObject stop_reason(Block block) {
        Ruby rt = getRuntime();
        checkStarted();
        DebugContext debugContext = debugContext();

        String symName;
        switch(debugContext.getStopReason()) {
            case STEP:
                symName = "step";
                break;
            case BREAKPOINT:
                symName = "breakpoint";
                break;
            case CATCHPOINT:
                symName = "catchpoint";
                break;
            case NONE:
            default:
                symName = "none";
        }
        
        if (debugContext.isDead()) {
            symName = "post-mortem";
        }
        
        return rt.newSymbol(symName);
    }

    @JRubyMethod(name="suspend")
    public IRubyObject suspend(Block block) {
        checkStarted();
        DebugContext debugContext = debugContext();
        if (debugContext.isSuspended()) {
            throw getRuntime().newRuntimeError("Already suspended.");
        }
    
        suspend0(); 
        
        return getRuntime().getNil();
    }

    protected void suspend0() {
        DebugContext debugContext = debugContext();
        
        String status = debugContext.getThread().status().toString();
        if (status.equals("run") || status.equals("sleep")) {
            synchronized (this) {
                debugContext.setWasRunning(true);
                debugContext.setSuspended(true);
            }
        }
    }

    @JRubyMethod(name="suspended?")
    public IRubyObject suspended_p(Block block) {
        checkStarted();
        return getRuntime().newBoolean(debugContext().isSuspended());
    }

    @JRubyMethod(name="resume")
    public IRubyObject resume(Block block) {
        checkStarted();
        DebugContext debugContext = debugContext();
        
        if (!debugContext.isSuspended()) {
            throw getRuntime().newRuntimeError("Thread is not suspended.");
        }
        
        resume0();
        return getRuntime().getNil();
    }

    void resume0() {
        DebugContext debugContext = debugContext();

        synchronized (this) {
            debugContext.setSuspended(false);
        }
        
        if (debugContext.isWasRunning()) {
            debugContext.getThread().wakeup();
        }
    }

    @JRubyMethod(name="tracing")
    public IRubyObject tracing(Block block) {
        checkStarted();
        DebugContext debugContext = debugContext();
        return getRuntime().newBoolean(debugContext.isTracing());
    }

    @JRubyMethod(name="tracing=", required=1)
    public IRubyObject tracing_set(IRubyObject tracing, Block block) {
        checkStarted();
        DebugContext debugContext = debugContext();
        debugContext.setTracing(tracing.isTrue());
        return tracing;
    }

    @JRubyMethod(name="ignored?")
    public IRubyObject ignored_p(Block block) {
        checkStarted();
        return getRuntime().newBoolean(debugContext().isIgnored());
    }

    @JRubyMethod(name="frame_args", required=1)
    public IRubyObject frame_args(IRubyObject frameNo, Block block) {
        checkStarted();
        DebugFrame frame = getFrame(frameNo);
        if (frame.isDead()) {
            return frame.getInfo().getCopyArgs();
        } else {
            return contextCopyArgs(frame);
        }
     }

    @JRubyMethod(name="frame_binding", required=1)
    public IRubyObject frame_binding(IRubyObject frameNo, Block block) {
        checkStarted();
        return getFrame(frameNo).getBinding();
    }
    
    @JRubyMethod(name={"frame_id", "frame_method"}, required=1)
    public IRubyObject frame_method(IRubyObject frameNo, Block block) {
        checkStarted();
        String methodName = getFrame(frameNo).getMethodName();
        return methodName == null ? getRuntime().getNil() : getRuntime().newSymbol(methodName);
    }
    
    @JRubyMethod(name="frame_args_info", optional=1)
    public IRubyObject frame_args_info(IRubyObject[] args, Block block) {
        checkStarted();
        return getFrame(args).getArgValues();
    }

    @JRubyMethod(name="frame_line", optional=1)
    public IRubyObject frame_line(IRubyObject[] args, Block block) {
        checkStarted();
        return getRuntime().newFixnum(getFrame(args).getLine());
    }

    @JRubyMethod(name="frame_file", optional=1)
    public IRubyObject frame_file(IRubyObject[] args, Block block) {
        return getRuntime().newString(getFrame(args).getFile());
    }

    @JRubyMethod(name="frame_locals", required=1)
    public IRubyObject frame_locals(IRubyObject frameNo, Block block) {
        checkStarted();
        DebugFrame frame = getFrame(frameNo);
        if (frame.isDead()) {
            return frame.getInfo().getCopyLocals();
        } else {
            return contextCopyLocals(frame);
        }
    }

    @JRubyMethod(name="frame_self", required=1)
    public IRubyObject frame_self(IRubyObject frameNo, Block block) {
        checkStarted();
        return getFrame(frameNo).getSelf();
    }

    @JRubyMethod(name="frame_class", optional=1)
    public IRubyObject frame_class(IRubyObject[] args, Block block) {
        checkStarted();
        DebugFrame frame = getFrame(args);
        if (frame.isDead()) {
            return getRuntime().getNil();
        }
        // FIXME implement correctly
        return frame.getInfo().getFrame().getKlazz();
    }

    @JRubyMethod(name="stack_size")
    public IRubyObject stack_size(Block block) {
        checkStarted();
        return getRuntime().newFixnum(debugContext().getStackSize());
    }

    @JRubyMethod(name="dead?")
    public IRubyObject dead_p(Block block) {
        checkStarted();
        return Util.toRBoolean(this, debugContext().isDead());
    }

    @JRubyMethod(name="breakpoint")
    public IRubyObject breakpoint(Block block) {
        checkStarted();
        
        return debugContext().getBreakpoint();
    }

    /**
     * <pre>
     * call-seq:
     *    context.set_breakpoint(source, pos, condition = nil) -> breakpoint
     * </pre>
     * <p>
     * Sets a context-specific temporary breakpoint, which can be used to implement
     * 'Run to Cursor' debugger function. When this breakpoint is reached, it will be
     * cleared out.
     * </p>
     * <p>
     * <i>source</i> is a name of a file or a class.<br/>
     * <i>pos</i> is a line number or a method name if <i>source</i> is a class name.<br/>
     * <i>condition</i> is a string which is evaluated to <tt>true</tt> when this breakpoint is activated.<br/>
     * </p>
     */
    @JRubyMethod(name="set_breakpoint", required=2, optional=1)
    public IRubyObject set_breakpoint(IRubyObject[] args, Block block) {
        checkStarted();
        
        IRubyObject breakpoint = debugger.createBreakpointFromArgs(this, args, 0);
        debugContext().setBreakpoint(breakpoint);
        
        return breakpoint;
    }

    private IRubyObject getFrameNumber(final IRubyObject[] args) {
        return args.length == 1 ? args[0] : getRuntime().newFixnum(0);
    }

    private DebugFrame getFrame(final IRubyObject[] args) {
        return getFrame(getFrameNumber(args));
    }
    
    private DebugFrame getFrame(final IRubyObject frameNo) {
        DebugContext debugContext = debugContext();
        int frameNoInt = checkFrameNumber(frameNo);
        return debugContext.getFrame(frameNoInt);
    }

    private int checkFrameNumber(IRubyObject rFrameNo) {
        int frameNo = RubyFixnum.fix2int(rFrameNo);
        
        if (frameNo < 0 || frameNo >= debugContext().getStackSize()) {
            throw rFrameNo.getRuntime().newArgumentError(
                    String.format("Invalid frame number %d, stack (0...%d)", 
                            frameNo, debugContext().getStackSize()));
        }
        
        return frameNo;
    }
    
    private void checkStarted() {
        debugger.checkStarted(getRuntime());
    }

    /*
     *   call-seq:
     *      context.copy_args(frame) -> list of args
     *
     *   Returns a array of argument names.
     */
    private IRubyObject contextCopyArgs(DebugFrame debugFrame) {
        RubyArray result = getRuntime().newArray();
        StaticScope scope = debugFrame.getInfo().getScope();
        
        int count = scope.getRequiredArgs() + scope.getOptionalArgs();
        if (scope.getRestArg() >= 0) {
            count++;
        }
        
        String[] names = scope.getVariables();
        for (int i = 0; i < count; i++) {
            result.append(getRuntime().newString(names[i]));
        }
        
        return result;
    }

    private IRubyObject contextCopyLocals(final DebugFrame debugFrame) {
        RubyHash locals = RubyHash.newHash(getRuntime());
        DynamicScope scope = debugFrame.getInfo().getDynaVars();
        if (scope != null) {
            DynamicScope evalScope = null;            
            try {
              final Class<? extends DynamicScope> clazz = scope.getClass();
              final Method method = clazz.getDeclaredMethod("getEvalScope");
              final Class[] args = method.getParameterTypes();            
              evalScope = (DynamicScope)(args.length > 0 ? method.invoke(scope, getRuntime()) : method.invoke(scope));
            } catch (NoSuchMethodException ignored) { 
            } catch (IllegalAccessException ignored) {                
            } catch (InvocationTargetException ignored) {                
            }
            if (evalScope != null) {
                scope = evalScope;
            }
            while (scope != null) {
                String[] variableNames = scope.getStaticScope().getVariables();
                if (variableNames != null) {
                    for (int i = 0; i < variableNames.length; i++) {
                        locals.op_aset(getRuntime().getCurrentContext(),
                                RubyString.newString(getRuntime(), variableNames[i]),
                                scope.getValues()[i]);
                    }
                }
                scope = scope.getNextCapturedScope();
            }
        }
        return locals;
    }

}
