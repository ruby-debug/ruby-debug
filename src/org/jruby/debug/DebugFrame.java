/*
 * header & license
 * Copyright (c) 2007 Martin Krauskopf
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

import org.jruby.parser.StaticScope;
import org.jruby.runtime.DynamicScope;
import org.jruby.runtime.Frame;
import org.jruby.runtime.builtin.IRubyObject;

final class DebugFrame {

    private IRubyObject binding;
    private String methodName;
    private String origMethodName;
    private String file;
    private int line;
    private boolean dead;
    private IRubyObject self;
    private IRubyObject argValues;
    private Info info;

    DebugFrame() {
        info = new Info();
    }

    IRubyObject getBinding() {
        return binding;
    }

    void setBinding(IRubyObject binding) {
        this.binding = binding;
    }

    boolean isDead() {
        return dead;
    }

    void setDead(boolean dead) {
        this.dead = dead;
    }

    String getFile() {
        return file;
    }

    void setFile(String file) {
        this.file = file;
    }

    String getMethodName() {
        return methodName;
    }

    void setMethodName(String methodName) {
        this.methodName = methodName;
    }

    Info getInfo() {
        return info;
    }

    void setInfo(Info info) {
        this.info = info;
    }

    int getLine() {
        return line;
    }

    void setLine(int line) {
        this.line = line;
    }

    String getOrigMethodName() {
        return origMethodName;
    }

    void setOrigMethodName(String origMethodName) {
        this.origMethodName = origMethodName;
    }

    IRubyObject getSelf() {
        return self;
    }

    void setSelf(IRubyObject self) {
        this.self = self;
    }

    public @Override String toString() {
        return "DebugFrame[" + file + ':' + line + "]";
    }

    static final class Info {

        private Frame frame;
        private StaticScope scope;
        private DynamicScope dynaVars;

        private IRubyObject copyArgs;
        private IRubyObject copyLocals;
        private IRubyObject copyArgAry;

        IRubyObject getCopyArgAry() {
            return copyArgAry;
        }

        void setCopyArgAry(IRubyObject copyArgAry) {
            this.copyArgAry = copyArgAry;
        }

        IRubyObject getCopyArgs() {
            return copyArgs;
        }

        void setCopyArgs(IRubyObject copyArgs) {
            this.copyArgs = copyArgs;
        }

        IRubyObject getCopyLocals() {
            return copyLocals;
        }

        void setCopyLocals(IRubyObject copyLocals) {
            this.copyLocals = copyLocals;
        }

        DynamicScope getDynaVars() {
            return dynaVars;
        }

        void setDynaVars(DynamicScope dynaVars) {
            this.dynaVars = dynaVars;
        }

        Frame getFrame() {
            return frame;
        }

        void setFrame(Frame frame) {
            this.frame = frame;
        }

        StaticScope getScope() {
            return scope;
        }

        void setScope(StaticScope scope) {
            this.scope = scope;
        }
    }

    public IRubyObject getArgValues() {
        return argValues;
    }

    public void setArgValues(IRubyObject argValues) {
        this.argValues = argValues;
    }
}
