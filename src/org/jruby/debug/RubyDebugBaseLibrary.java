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

import java.io.IOException;
import org.jruby.Ruby;
import org.jruby.RubyClass;
import org.jruby.RubyThread;
import org.jruby.anno.JRubyMethod;
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
        RubyDebugger.createDebuggerModule(runtime);
    }
    
    public static class DebugThread extends RubyThread {
        protected DebugThread(Ruby runtime, RubyClass type, Block block) {
            super(runtime, type);
        }
        
        @JRubyMethod(name="inherited", required=1, meta=true)
        public static IRubyObject inherited(IRubyObject recv, IRubyObject clazz, Block block) {
            throw recv.getRuntime().newRuntimeError("Can't inherite Debugger::DebugThread class");
        }
    }
    
}
