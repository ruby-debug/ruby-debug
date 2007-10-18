package org.jruby.debug;

import org.jruby.Ruby;
import org.jruby.RubyClass;
import org.jruby.RubyFixnum;
import org.jruby.RubyHash;
import org.jruby.RubyObject;
import org.jruby.RubyString;
import org.jruby.runtime.Arity;
import org.jruby.runtime.Block;
import org.jruby.runtime.DynamicScope;
import org.jruby.runtime.builtin.IRubyObject;

public class Context extends RubyObject {

    private final Debugger debugger;

    Context(Ruby runtime, RubyClass type, Debugger debugger) {
        super(runtime, type);
        this.debugger = debugger;
    }

    DebugContext debugContext() {
        return (DebugContext) dataGetStruct();
    }

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

        if (Util.toInt(steps) < 0) {
            rt.newRuntimeError("Steps argument can't be negative.");
        }

        DebugContext debug_context = debugContext();
        debug_context.setStopNext(Util.toInt(steps));
        debug_context.setForceMove(!force.isNil() && Util.toBoolean(force));
        return steps;
    }

    public IRubyObject step(Block block) {
        throw new UnsupportedOperationException("not implemented yet");
    }

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
            int frameInt = checkFrameNumber(debugContext, frame);
            debugContext.setDestFrame(debugContext.getStackSize() - frameInt);
        }
        debugContext.setForceMove(force.isTrue());
        return rt.getNil();
    }

    public IRubyObject stop_frame_set(IRubyObject frame, Block block) {
        throw new UnsupportedOperationException("not implemented yet");
    }

    public IRubyObject thread(Block block) {
        checkStarted();
        return debugContext().getThread();
    }

    public IRubyObject thnum(Block block) {
        return RubyFixnum.newFixnum(getRuntime(), debugContext().getThnum());
    }

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
        // FIXME
//        if(CTX_FL_TEST(debug_context, CTX_FL_DEAD))
//            sym_name = "post-mortem";
        
        return rt.newSymbol(symName);
    }

    public IRubyObject suspend(Block block) {
        throw new UnsupportedOperationException("not implemented yet");
    }

    public IRubyObject suspended_p(Block block) {
        throw new UnsupportedOperationException("not implemented yet");
    }

    public IRubyObject resume(Block block) {
        throw new UnsupportedOperationException("not implemented yet");
    }

    public IRubyObject tracing(Block block) {
        throw new UnsupportedOperationException("not implemented yet");
    }

    public IRubyObject tracing_set(IRubyObject tracing, Block block) {
        throw new UnsupportedOperationException("not implemented yet");
    }

    public IRubyObject ignored_p(Block block) {
        checkStarted();
        return getRuntime().newBoolean(debugContext().isIgnored());
    }

    public IRubyObject frame_args(IRubyObject frameNo, Block block) {
        checkStarted();
        DebugFrame frame = getFrame(frameNo);
        if (frame.isDead()) {
            return frame.getInfo().getCopyArgs();
        } else {
            return contextCopyArgs(frame);
        }
     }

    public IRubyObject frame_binding(IRubyObject frameNo, Block block) {
        checkStarted();
        return getFrame(frameNo).getBinding();
    }
    
    public IRubyObject frame_method(IRubyObject frameNo, Block block) {
        debugger.checkStarted(getRuntime());
        String methodName = getFrame(frameNo).getMethodName();
        return methodName == null ? getRuntime().getNil() : getRuntime().newSymbol(methodName);
    }

    public IRubyObject frame_line(IRubyObject frameNo, Block block) {
        return getRuntime().newFixnum(getFrame(frameNo).getLine());
    }

    public IRubyObject frame_file(IRubyObject frameNo, Block block) {
        return getRuntime().newString(getFrame(frameNo).getFile());
    }

    public IRubyObject frame_locals(IRubyObject frameNo, Block block) {
        checkStarted();
        DebugFrame frame = getFrame(frameNo);
        if (frame.isDead()) {
            return frame.getInfo().getCopyLocals();
        } else {
            return contextCopyLocals(frame);
        }
    }

    public IRubyObject frame_self(IRubyObject frameNo, Block block) {
        checkStarted();
        return getFrame(frameNo).getSelf();
    }

    public IRubyObject frame_class(IRubyObject frameNo, Block block) {
        debugger.checkStarted(getRuntime());
        DebugFrame frame = getFrame(frameNo);
        if (frame.isDead()) {
            return getRuntime().getNil();
        }
        // FIXME implement correctly
        return frame.getInfo().getFrame().getKlazz();
    }

    public IRubyObject stack_size(Block block) {
        debugger.checkStarted(getRuntime());
        return getRuntime().newFixnum(debugContext().getStackSize());
    }

    public IRubyObject dead_p(Block block) {
        debugger.checkStarted(getRuntime());
        return Util.toRBoolean(this, debugContext().isDead());
    }

    public IRubyObject breakpoint(Block block) {
        throw new UnsupportedOperationException("not implemented yet");
    }

    public IRubyObject set_breakpoint(IRubyObject[] args, Block block) {
        throw new UnsupportedOperationException("not implemented yet");
    }

    private DebugFrame getFrame(final IRubyObject frameNo) {
        DebugContext debugContext = debugContext();
        int frameNoInt = checkFrameNumber(debugContext, frameNo);
        return debugContext.getFrame(frameNoInt);
    }

    private int checkFrameNumber(DebugContext context, IRubyObject rFrameNo) {
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
//        ID *tbl;
//        int n, i;
//        Scope scope;
//        IRubyObject list = rb_ary_new2(0); /* [] */
//
//        scope = debugFrame->info.runtime.scope;
//        tbl = scope->local_tbl;
//
//        if (tbl && scope->local_vars) {
//            n = *tbl++;
//            if (debugFrame->argc+2 < n) n = debugFrame->argc+2;
//            list = rb_ary_new2(n);
//            /* skip first 2 ($_ and $~) */
//            for (i=2; i<n; i++) {   
//                /* skip first 2 ($_ and $~) */
//                if (!rb_is_local_id(tbl[i])) continue; /* skip flip states */
//                rb_ary_push(list, rb_str_new2(rb_id2name(tbl[i])));
//            }
//        }
//
//        return list;
        System.err.println("MK> " + new Exception().getStackTrace()[0] + " called...." + ", " + System.currentTimeMillis());
        System.err.println("MK>   IMPLEMENT ME");
        return getRuntime().newArray();
    }

    private IRubyObject contextCopyLocals(final DebugFrame debugFrame) {
    	RubyHash locals = RubyHash.newHash(getRuntime());
        DynamicScope scope = debugFrame.getInfo().getDynaVars();
        if (scope != null) {
            scope = scope.getBindingScope();
            while (scope != null) {
                String[] variableNames = scope.getStaticScope().getVariables();
                if (variableNames != null) {
                    for (int i = 0; i < variableNames.length; i++) {
                        locals.op_aset(RubyString.newString(getRuntime(), variableNames[i]),
                                scope.getValues()[i]);
                    }
                }
                scope = scope.getNextCapturedScope();
            }
        }
        return locals;
    }

}
