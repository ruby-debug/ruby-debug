package org.jruby.debug;

import java.io.IOException;
import org.jruby.Ruby;
import org.jruby.RubyClass;
import org.jruby.RubyThread;
import org.jruby.runtime.Block;
import org.jruby.runtime.builtin.IRubyObject;
import org.jruby.runtime.load.Library;

public final class RubyDebugBaseLibrary implements Library {
    
    public static final int CTX_FL_SUSPEND = (1<<1);
    public static final int CTX_FL_TRACING = (1<<2);
    public static final int CTX_FL_SKIPPED = (1<<3);
    public static final int CTX_FL_IGNORE = (1<<4);
    public static final int CTX_FL_DEAD = (1<<5);
    public static final int CTX_FL_WAS_RUNNING = (1<<6);
    public static final int CTX_FL_ENABLE_BKPT = (1<<7);
    public static final int CTX_FL_STEPPED = (1<<8);
    public static final int CTX_FL_FORCE_MOVE = (1<<9);

    public static final int CTX_STOP_NONE = 0;
    public static final int CTX_STOP_STEP = 1;
    public static final int CTX_STOP_BREAKPOINT = 2;
    public static final int CTX_STOP_CATCHPOINT = 3;
    
    public static final int BP_POS_TYPE = 0;
    public static final int BP_METHOD_TYPE = 1;
    
    public static final int HIT_COND_NONE = 0;
    public static final int HIT_COND_GE = 1;
    public static final int HIT_COND_EQ = 2;
    public static final int HIT_COND_MOD = 3;

    public void load(Ruby runtime) throws IOException {
        DebuggerDef.createDebuggerModule(runtime);
    }
    
    public static class DebugThread extends RubyThread {
        protected DebugThread(Ruby runtime, RubyClass type, Block block) {
            super(runtime, type);
        }
        
        public static IRubyObject inherited(IRubyObject recv, IRubyObject clazz, Block block) {
            throw new UnsupportedOperationException("not implemented yet");
        }
    }
    
}
