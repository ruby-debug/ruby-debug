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

import org.jruby.Ruby;
import org.jruby.RubyClass;
import org.jruby.RubyKernel;
import org.jruby.RubyModule;
import org.jruby.RubyProc;
import org.jruby.anno.JRubyMethod;
import org.jruby.debug.RubyDebugBaseLibrary.DebugThread;
import org.jruby.runtime.Block;
import org.jruby.runtime.ObjectAllocator;
import org.jruby.runtime.builtin.IRubyObject;

public final class RubyDebugger {
    
    private RubyDebugger() {/* forbid instances */}

    static final String DEBUG_THREAD_NAME = "DebugThread";
    static final String CONTEXT_NAME = "Context";
    
    private static Debugger debugger;
    
    public static RubyModule createDebuggerModule(Ruby runtime) {
        
        /* Debugger module. */
        RubyModule debuggerMod = runtime.defineModule("Debugger");
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
    @JRubyMethod(name="start_", module=true)
    public static IRubyObject start(IRubyObject recv, Block block) {
        return debugger().start(recv, block);
    }

    /**
     * This method deactivates the debugger. It returns +true+ if the debugger
     * is deactivated, otherwise it returns +false+.
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
        debugger().checkStarted(recv);
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

    @JRubyMethod(name="catchpoints", module=true)
    public static IRubyObject catchpoint(IRubyObject recv, Block block) {
        debugger().checkStarted(recv);
        return debugger().getCatchpoints();
    }

    @JRubyMethod(name="add_catchpoint", module=true, required=1)
    public static IRubyObject addCatchpoint(IRubyObject recv, IRubyObject catchpoint, Block block) {
        debugger().addCatchpoint(recv, catchpoint);
        return catchpoint;
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
        return debugger().getCurrentContext(recv);
    }

    @JRubyMethod(name="suspend", module=true)
    public static IRubyObject suspend(IRubyObject recv, Block block) {
        debugger().suspend(recv);
        
        return recv;
    }

    @JRubyMethod(name="resume", module=true)
    public static IRubyObject resume(IRubyObject recv, Block block) {
        debugger().resume(recv);
        
        return recv;
    }

    @JRubyMethod(name="tracing", module=true)
    public static IRubyObject tracing(IRubyObject recv, Block block) {
        return recv.getRuntime().newBoolean(debugger().isTracing());
    }

    @JRubyMethod(name="tracing=", module=true, required=1)
    public static IRubyObject tracing_set(IRubyObject recv, IRubyObject tracing, Block block) {
        debugger().setTracing(tracing.isTrue());
        
        return tracing;
    }
    
    /**
     * <pre>
     * Debugger.debug_load(file, stop = false, increment_start = false) -> nil
     * </pre>
     * <p>
     * Same as Kernel#load but resets current context's frames.
     * <p>
     * FOR INTERNAL USE ONLY.
     * @param stop parameter forces the debugger to stop at the first line of
     *        code in the +file+
     * @param increment_start determines if start_count should be incremented.
     *        When control threads are used, they have to be set up before
     *        loading the debugger; so here +increment_start+ will be false.
     */
    @JRubyMethod(name="debug_load", module=true, required=1, optional=2)
    public static IRubyObject debug_load(IRubyObject recv, IRubyObject[] args, Block block) {
        return debugger().load(recv, args);
    }

    @JRubyMethod(name="skip", module=true)
    public static IRubyObject skip(IRubyObject recv, Block block) {
        return debugger().skip(recv, block);
    }

    @JRubyMethod(name="debug_at_exit", module=true)
    public static IRubyObject debug_at_exit(IRubyObject recv, Block block) {
        RubyProc proc = RubyKernel.proc(recv.getRuntime().getCurrentContext(), recv, block);
        recv.getRuntime().pushExitBlock(proc);
        
        return proc;
    }

    @JRubyMethod(name="post_mortem?", module=true)
    public static IRubyObject post_mortem_p(IRubyObject recv, Block block) {
        return Util.toRBoolean(recv, debugger().isPostMortem());
    }

    @JRubyMethod(name="post_mortem=", module=true, required=1)
    public static IRubyObject post_mortem_set(IRubyObject recv, IRubyObject postMortem, Block block) {
        throw recv.getRuntime().newRuntimeError("Post mortem debugging is not (yet) supported");
        /*
        debugger().setPostMortem(postMortem.isTrue());
        
        return postMortem;
        */
    }

    @JRubyMethod(name="keep_frame_binding?", module=true)
    public static IRubyObject keep_frame_binding_p(IRubyObject recv, Block block) {
        return recv.getRuntime().newBoolean(debugger().isKeepFrameBinding());
    }

    @JRubyMethod(name="keep_frame_binding=", module=true, required=1)
    public static IRubyObject keep_frame_binding_set(IRubyObject recv, IRubyObject keepFrameBinding, Block block) {
        debugger().setKeepFrameBinding(keepFrameBinding.isTrue());
        
        return keepFrameBinding;
    }
    
    @JRubyMethod(name="track_frame_args?", module=true)
    public static IRubyObject track_frame_args_p(IRubyObject recv, Block block) {
        return recv.getRuntime().newBoolean(debugger().isTrackFrameArgs());
    }

    @JRubyMethod(name="track_frame_args=", module=true, required=1)
    public static IRubyObject track_frame_args_set(IRubyObject recv, IRubyObject traceFrameArgs, Block block) {
        debugger().setTrackFrameArgs(traceFrameArgs.isTrue());
        
        return traceFrameArgs;
    }    

    @JRubyMethod(name="debug", module=true)
    public static IRubyObject debug(IRubyObject recv, Block block) {
        return Util.toRBoolean(recv, debugger().isDebug());
    }

    @JRubyMethod(name="debug=", module=true, required=1)
    public static IRubyObject debug_set(IRubyObject recv, IRubyObject debug, Block block) {
        debugger().setDebug(debug.isTrue());
        
        return debug;
    }

    private static final ObjectAllocator BREAKPOINT_ALLOCATOR = new ObjectAllocator() {
        public IRubyObject allocate(Ruby runtime, RubyClass klass) {
            return new Breakpoint(runtime, klass);
        }
    };

    private static final ObjectAllocator CONTEXT_ALLOCATOR = new ObjectAllocator() {
        public IRubyObject allocate(Ruby runtime, RubyClass klass) {
            return new Context(runtime, klass, debugger());
        }
    };
}
