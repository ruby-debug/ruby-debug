package org.jruby.debug;

import org.jruby.Ruby;
import org.jruby.RubyClass;
import org.jruby.RubyModule;
import org.jruby.anno.JRubyMethod;
import org.jruby.debug.RubyDebugBaseLibrary.DebugThread;
import org.jruby.runtime.Block;
import org.jruby.runtime.ObjectAllocator;
import org.jruby.runtime.builtin.IRubyObject;

public final class RubyDebugger {

    static final String DEBUG_THREAD_NAME = "DebugThread";
    static final String CONTEXT_NAME = "Context";
    
    private static final String VERSION = "0.9.3";
    private static Debugger debugger;
    
    public static RubyModule createDebuggerModule(Ruby runtime) {
        
        /* Debugger module. */
        RubyModule debuggerMod = runtime.defineModule("Debugger");
        
        debuggerMod.defineConstant("VERSION", runtime.newString(VERSION));
        debuggerMod.defineAnnotatedMethods(RubyDebugger.class);

        /* Debugger::ThreadsTable */
        /* RubyClass threadsTable = */ debuggerMod.defineClassUnder("ThreadsTable", runtime.getObject(), runtime.getObject().getAllocator());
        
        /* Debugger::DebugThread */
        RubyClass debugThread = debuggerMod.defineClassUnder(DEBUG_THREAD_NAME, runtime.getClass("Thread"), runtime.getClass("Thread").getAllocator());
        debugThread.defineAnnotatedMethods(DebugThread.class);
        
        /* Debugger::Breakpoint */
        RubyClass breakpoint = debuggerMod.defineClassUnder("Breakpoint", runtime.getObject(), BREAKPOINT_ALLOCATOR);
        breakpoint.defineAnnotatedMethods(Breakpoint.class);
        
        /* Debugger::Context */
        RubyClass context = debuggerMod.defineClassUnder(CONTEXT_NAME, runtime.getObject(), CONTEXT_ALLOCATOR);
        context.defineAnnotatedMethods(Context.class);

        return debuggerMod;
    }
    
    private static Debugger debugger() {
        synchronized (RubyDebugger.class) {
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
    @JRubyMethod(name="start", module=true)
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
    @JRubyMethod(name="stop", module=true)
    public static IRubyObject stop(IRubyObject recv, Block block) {
        boolean stopped = debugger().stop(recv.getRuntime());
        return Util.toRBoolean(recv, stopped);
    }
    
    @JRubyMethod(name="started?", module=true)
    public static IRubyObject started_p(IRubyObject recv, Block block) {
        return Util.toRBoolean(recv, debugger().isStarted());
    }

    @JRubyMethod(name="breakpoints", module=true)
    public static IRubyObject breakpoints(IRubyObject recv, Block block) {
        return debugger().getBreakpoints();
    }

    @JRubyMethod(name="add_breakpoint", module=true, required=2, optional=1)
    public static IRubyObject add_breakpoint(IRubyObject recv, IRubyObject[] args, Block block) {
        return debugger().addBreakpoint(recv, args);
    }

    @JRubyMethod(name="remove_breakpoint", module=true, required=1)
    public static IRubyObject remove_breakpoint(IRubyObject recv, IRubyObject breakpointId, Block block) {
        return debugger().removeBreakpoint(recv, breakpointId);
    }

    @JRubyMethod(name="catchpoint", module=true)
    public static IRubyObject catchpoint(IRubyObject recv, Block block) {
        throw new UnsupportedOperationException("not implemented yet");
    }

    @JRubyMethod(name="catchpoint=", module=true, required=1)
    public static IRubyObject catchpoint_set(IRubyObject recv, IRubyObject catchpoint, Block block) {
        throw new UnsupportedOperationException("not implemented yet");
    }

    @JRubyMethod(name="last_context", module=true)
    public static IRubyObject last_context(IRubyObject recv, Block block) {
        return debugger().lastInterrupted(recv);
    }

    @JRubyMethod(name="contexts", module=true)
    public static IRubyObject contexts(IRubyObject recv, Block block) {
        return debugger().getDebugContexts(recv);
    }

    @JRubyMethod(name="current_context", module=true)
    public static IRubyObject current_context(IRubyObject recv, Block block) {
        return debugger().getCurrentContext(recv);
    }

    @JRubyMethod(name="thread_context", module=true, required=1)
    public static IRubyObject thread_context(IRubyObject recv, IRubyObject context, Block block) {
        throw new UnsupportedOperationException("not implemented yet");
    }

    @JRubyMethod(name="suspend", module=true)
    public static IRubyObject suspend(IRubyObject recv, Block block) {
        throw new UnsupportedOperationException("not implemented yet");
    }

    @JRubyMethod(name="resume", module=true)
    public static IRubyObject resume(IRubyObject recv, Block block) {
        throw new UnsupportedOperationException("not implemented yet");
    }

    @JRubyMethod(name="tracing", module=true)
    public static IRubyObject tracing(IRubyObject recv, Block block) {
        throw new UnsupportedOperationException("not implemented yet");
    }

    @JRubyMethod(name="tracing=", module=true, required=1)
    public static IRubyObject tracing_set(IRubyObject recv, IRubyObject tracing, Block block) {
        debugger().setTracing(Util.toBoolean(tracing));
        return Util.nil(recv);
    }
    
    @JRubyMethod(name="debug_load", module=true, required=1, optional=1)
    public static IRubyObject debug_load(IRubyObject recv, IRubyObject[] args, Block block) {
        debugger().load(recv, args);
        return Util.nil(recv);
    }

    @JRubyMethod(name="skip", module=true)
    public static IRubyObject skip(IRubyObject recv, Block block) {
        throw new UnsupportedOperationException("not implemented yet");
    }

    @JRubyMethod(name="debug_at_exit", module=true)
    public static IRubyObject debug_at_exit(IRubyObject recv, Block block) {
        System.err.println("FIXME> IMPLEMENT ME: DebuggerDef.debug_at_exit()");
        return Util.nil(recv);
    }

    @JRubyMethod(name="post_mortem?", module=true)
    public static IRubyObject post_mortem_p(IRubyObject recv, Block block) {
        return Util.toRBoolean(recv, debugger().isPostMortem());
    }

    @JRubyMethod(name="post_mortem=", module=true, required=1)
    public static IRubyObject post_mortem_set(IRubyObject recv, IRubyObject postMortem, Block block) {
        debugger().setPostMortem(Util.toBoolean(postMortem));
        return Util.nil(recv);
    }

    @JRubyMethod(name="keep_frame_binding?", module=true)
    public static IRubyObject keep_frame_binding_p(IRubyObject recv, Block block) {
        throw new UnsupportedOperationException("not implemented yet");
    }

    @JRubyMethod(name="keep_frame_binding=", module=true, required=1)
    public static IRubyObject keep_frame_binding_set(IRubyObject recv, IRubyObject keepFrameBinding, Block block) {
        debugger().setKeepFrameBinding(Util.toBoolean(keepFrameBinding));
        return Util.nil(recv);
    }
    
    @JRubyMethod(name="track_frame_args?", module=true)
    public static IRubyObject track_frame_args_p(IRubyObject recv, Block block) {
        throw new UnsupportedOperationException("not implemented yet");
    }

    @JRubyMethod(name="track_frame_args=", module=true, required=1)
    public static IRubyObject track_frame_args_set(IRubyObject recv, IRubyObject keepFrameBinding, Block block) {
        debugger().setKeepFrameBinding(Util.toBoolean(keepFrameBinding));
        return Util.nil(recv);
    }    

    @JRubyMethod(name="debug", module=true)
    public static IRubyObject debug(IRubyObject recv, Block block) {
        return Util.toRBoolean(recv, debugger().isDebug());
    }

    @JRubyMethod(name="debug=", module=true, required=1)
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
