package org.jruby.debug;

import java.util.IdentityHashMap;
import java.util.Map;

import org.jruby.Ruby;
import org.jruby.RubyArray;
import org.jruby.RubyClass;
import org.jruby.RubyFixnum;
import org.jruby.RubyHash;
import org.jruby.RubyString;
import org.jruby.RubyThread;
import org.jruby.debug.DebugBreakpoint.Type;
import org.jruby.runtime.Arity;
import org.jruby.runtime.Block;
import org.jruby.runtime.EventHook;
import org.jruby.runtime.builtin.IRubyObject;

final class Debugger {

    private EventHook debugEventHook;
    
    private IRubyObject threadsTbl; // RubyThread.id() -> DebugContext
    private IRubyObject breakpoints;
    private IRubyObject catchpoint;
    private boolean tracing;
    private boolean postMortem;
    private boolean keepFrameBinding;
    private boolean debug;
    private boolean trackFrameArgs;

    private IRubyObject lastContext;
    private IRubyObject lastThread ;

    private boolean started;
    private int startCount;
    private int bkp_count;

    private DebugContext lastDebugContext;

    IRubyObject start(IRubyObject recv, Block block) {
        Ruby runtime = recv.getRuntime();
        
        startCount++;
        IRubyObject result;
        if (started) {
            result = runtime.getFalse();
        } else {
            IRubyObject nil = runtime.getNil();
            lastThread  = nil;
            started = true;
            setLastContext(runtime, nil);
            tracing = false;
            postMortem = false;
            keepFrameBinding = false;
            debug = false;
            trackFrameArgs = false;
            catchpoint = nil;
            debugEventHook = new DebugEventHook(this, runtime);
            breakpoints = runtime.newArray();
            threadsTbl = RubyHash.newHash(runtime);
            threadsTbl.dataWrapStruct(new IdentityHashMap<RubyThread, IRubyObject>());
            runtime.addEventHook(debugEventHook);
            result = runtime.getTrue();
        }
        
        if (block.isGiven()) {
            try {
                return block.yield(runtime.getCurrentContext(), recv);
            } finally {
                stop(runtime);
            }
        }
        
        return result;
    }

    boolean stop(final Ruby runtime) {
        checkStarted(runtime);
        startCount--;
        if (startCount > 0) {
            return false;
        }
        runtime.removeEventHook(debugEventHook);
        breakpoints = null;
        debugEventHook = null;
        started = false;
        return true;
    }

    void load(IRubyObject recv, IRubyObject[] args) {
        Ruby rt = recv.getRuntime();
        Arity.checkArgumentCount(rt, args, 1, 2);
        IRubyObject file = args[0];
        IRubyObject stop;
        if (args.length == 1) {
            stop = rt.getFalse();
        } else {
            stop = args[1];
        }

        start(recv, Block.NULL_BLOCK);
        IRubyObject context = getCurrentContext(recv);
        DebugContext debugContext = (DebugContext) context.dataGetStruct();
        if (Util.toBoolean(stop)) {
            debugContext.setStopNext(1);
        }
        rt.getLoadService().load(((RubyString) file).toString());
        stop(rt);
    }
    
    IRubyObject getCurrentContext(IRubyObject recv) {
        checkStarted(recv.getRuntime());
        RubyThread thread = recv.getRuntime().getCurrentContext().getThread();
        return threadContextLookup(thread, false).context;
    }

    DebugContextPair threadContextLookup(final RubyThread thread, final boolean wantDebugContext) {
        Ruby rt = thread.getRuntime();
        checkStarted(rt);

        DebugContextPair ctxs = new DebugContextPair();
        if (lastThread == thread && !lastContext.isNil()) {
            ctxs.context = lastContext;
            if (wantDebugContext) {
                ctxs.debugContext = lastDebugContext;
            }
            return ctxs;
        }

        Map<RubyThread, IRubyObject> threadsTable = getThreadsTable();
        ctxs.context = threadsTable.get(thread);
        if (ctxs.context == null) {
            ctxs.context = debugContextCreate(thread);
            threadsTable.put(thread, ctxs.context);
        }

        DebugContext lDebugContext = (DebugContext) ctxs.context.dataGetStruct();
        if (wantDebugContext) {
            ctxs.debugContext = lDebugContext;
        }

        lastThread = thread;
        setLastContext(rt, ctxs.context);
        lastDebugContext = lDebugContext;
        return ctxs;
    }

    void checkStarted(final Ruby runtime) {
        if (!started) {
            throw runtime.newRuntimeError("Debugger.start is not called yet.");
        }
    }

    private IRubyObject debugContextCreate(RubyThread thread) {
        DebugContext debugContext = new DebugContext(thread);
        // if (thread.getType() == thread.getRuntime().getClass(DebuggerDef.DEBUG_THREAD_NAME)) {
        if (thread.getType().getName().equals("Debugger::" + DebuggerDef.DEBUG_THREAD_NAME)) {
            debugContext.setIgnored(true);
        }
        RubyClass cContext = thread.getRuntime().getModule("Debugger").getClass("Context");
        IRubyObject context = cContext.allocate();
        context.dataWrapStruct(debugContext);
        return context;
    }

    IRubyObject getDebugContexts(IRubyObject self) {
        Ruby rt = self.getRuntime();
        checkStarted(rt);
        RubyArray newList = rt.newArray();
        RubyArray list = RubyThread.list(self);

        for (int i = 0; i < list.size(); i++) {
            RubyThread thread = (RubyThread) list.entry(i);
            IRubyObject context = threadContextLookup(thread, false).context;
            newList.add(context);
        }
        ((RubyHash) threadsTbl).clear();
        Map<RubyThread, IRubyObject> threadsTable = getThreadsTable();
        for (int i = 0; i < newList.size(); i++) {
            IRubyObject context = newList.entry(i);
            DebugContext debugContext = (DebugContext) context.dataGetStruct();
            threadsTable.put(debugContext.getThread(), context);
        }

        return newList;
    }

    boolean isStarted() {
        return started;
    }

    void setTracing(boolean tracing) {
        this.tracing = tracing;
    }
    
    boolean isTracing() {
        return tracing;
    }

    void setKeepFrameBinding(boolean keepFrameBinding) {
        this.keepFrameBinding = keepFrameBinding;
    }

    boolean isKeepFrameBinding() {
        return keepFrameBinding;
    }

    boolean isTrackFrameArgs() {
        return trackFrameArgs;
    }

    IRubyObject getBreakpoints() {
        return breakpoints;
    }
    
    IRubyObject addBreakpoint(IRubyObject recv, IRubyObject[] args) {
        Ruby rt = recv.getRuntime();
        checkStarted(rt);
        IRubyObject result = createBreakpointFromArgs(recv, args, ++bkp_count);
        ((RubyArray) breakpoints).add(result);
        return result;
    }

    IRubyObject removeBreakpoint(IRubyObject recv, IRubyObject breakpointId) {
        int id = RubyFixnum.fix2int(breakpointId);
        RubyArray breakpointsA = ((RubyArray) breakpoints);
        for(int i = 0; i < breakpointsA.size(); i++) {
            IRubyObject breakpoint = breakpointsA.entry(i);
            DebugBreakpoint debugBreakpoint = (DebugBreakpoint) breakpoint.dataGetStruct();
            if(debugBreakpoint.getId() == id) {
                breakpointsA.remove(i);
                return breakpoint;
            }
        }
        return Util.nil(recv);
    }

    private IRubyObject createBreakpointFromArgs(IRubyObject recv, IRubyObject[] args, int id) {
        Ruby rt = recv.getRuntime();

        IRubyObject expr;
        if (Arity.checkArgumentCount(rt, args, 2, 3) == 3) {
            expr = args[2];
        } else {
            expr = rt.getNil();
        }
        IRubyObject source = args[0];
        IRubyObject pos = args[1];
        
        Type type = pos instanceof RubyFixnum ? DebugBreakpoint.Type.POS : DebugBreakpoint.Type.METHOD;
        if (type == DebugBreakpoint.Type.POS) {
            source = source.asString();
        } else {
            pos = pos.asString();
        }
        DebugBreakpoint debugBreakpoint = new DebugBreakpoint();
        debugBreakpoint.setId(id);
        debugBreakpoint.setSource(source);
        debugBreakpoint.setType(type);
        if (type == DebugBreakpoint.Type.POS) {
            debugBreakpoint.getPos().setLine(RubyFixnum.num2int(pos));
        } else {
            debugBreakpoint.getPos().setMethodName(((RubyString) pos).toString());
        }
        debugBreakpoint.setExpr(expr.isNil() ? expr : (RubyString) expr);
        debugBreakpoint.setHitCount(0);
        debugBreakpoint.setHitValue(0);
        debugBreakpoint.setHitCondition(DebugBreakpoint.HitCondition.NONE);
        RubyClass cBreakpoint = rt.getModule("Debugger").getClass("Breakpoint");
        IRubyObject breakpoint = cBreakpoint.allocate();
        breakpoint.dataWrapStruct(debugBreakpoint);
        return breakpoint;
    }

    IRubyObject lastInterrupted(IRubyObject recv) {
        checkStarted(recv.getRuntime());
        IRubyObject result = Util.nil(recv);
        Map<RubyThread, IRubyObject> threadsTable = getThreadsTable();
        for (Map.Entry<RubyThread, IRubyObject> entry : threadsTable.entrySet()) {
            IRubyObject context = entry.getValue();
            DebugContext debugContext = (DebugContext) context.dataGetStruct();
            if (debugContext.getThread() == lastThread) {
                result = context;
                break;
            }
        }
        return result;
    }

    IRubyObject getThreadsTbl() {
        return threadsTbl;
    }

    boolean isPostMortem() {
        return postMortem;
    }

    void setPostMortem(boolean postMortem) {
        this.postMortem = postMortem;
    }

    boolean isDebug() {
        return debug;
    }

    void setDebug(boolean debug) {
        this.debug = debug;
    }

    /** TODO: Get rid of me - here because of hard rewrite from C. */
    static final class DebugContextPair {
        IRubyObject context;
        DebugContext debugContext;
    }

    /** RubyThread -> IRubyObject(Debugger::Context) */
    @SuppressWarnings("unchecked")
    private Map<RubyThread, IRubyObject> getThreadsTable() {
        return (Map<RubyThread, IRubyObject>) threadsTbl.dataGetStruct();
    }

    private void setLastContext(Ruby runtime, IRubyObject value) {
        lastContext = value;
    }
}
