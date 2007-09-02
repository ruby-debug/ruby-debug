package org.jruby.debug;

import org.jruby.runtime.DynamicScope;
import org.jruby.runtime.Frame;
import org.jruby.runtime.builtin.IRubyObject;

final class DebugFrame {

    private Object argc;
    private IRubyObject binding;
    private String id;
    private String origId;
    private String file;
    private int line;
    private boolean dead;
    private IRubyObject self;
    private IRubyObject arg_ary;
    private Info info;

    DebugFrame() {
        info = new Info();
    }

    Object getArgc() {
        return argc;
    }

    void setArgc(Object argc) {
        this.argc = argc;
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

    String getId() {
        return id;
    }

    void setId(String id) {
        this.id = id;
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

    String getOrigId() {
        return origId;
    }

    void setOrigId(String origId) {
        this.origId = origId;
    }

    IRubyObject getSelf() {
        return self;
    }

    void setSelf(IRubyObject self) {
        this.self = self;
    }

    static final class Info {

        private Frame frame;
        private Object scope;
        private DynamicScope dynaVars;

        private Object copyArgs;
        private Object copyLocals;
        private Object copyArgAry;

        Object getCopyArgAry() {
            return copyArgAry;
        }

        void setCopyArgAry(Object copyArgAry) {
            this.copyArgAry = copyArgAry;
        }

        Object getCopyArgs() {
            return copyArgs;
        }

        void setCopyArgs(Object copyArgs) {
            this.copyArgs = copyArgs;
        }

        Object getCopyLocals() {
            return copyLocals;
        }

        void setCopyLocals(Object copyLocals) {
            this.copyLocals = copyLocals;
        }

        Object getDynaVars() {
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

        Object getScope() {
            return scope;
        }

        void setScope(Object scope) {
            this.scope = scope;
        }
    }
}
