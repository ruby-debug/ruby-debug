package org.jruby.debug;

import org.jruby.runtime.builtin.IRubyObject;

final class DebugBreakpoint {

    enum Type {
        POS, METHOD
    }

    enum HitCondition {
        NONE, GE, EQ, MOD
    }

    private int id;
    private Type type;
    private IRubyObject source;
    private Pos pos;
    private IRubyObject expr;
    private int hitCount;
    private int hitValue;
    private HitCondition hitCondition;

    DebugBreakpoint() {
        this.pos = new Pos();
    }
    
    IRubyObject getExpr() {
        return expr;
    }

    void setExpr(IRubyObject expr) {
        this.expr = expr;
    }

    HitCondition getHitCondition() {
        return hitCondition;
    }

    void setHitCondition(HitCondition hitCondition) {
        this.hitCondition = hitCondition;
    }

    int getHitCount() {
        return hitCount;
    }

    void setHitCount(int hitCount) {
        this.hitCount = hitCount;
    }

    int getHitValue() {
        return hitValue;
    }

    void setHitValue(int hitValue) {
        this.hitValue = hitValue;
    }

    int getId() {
        return id;
    }

    void setId(int id) {
        this.id = id;
    }

    Pos getPos() {
        return pos;
    }

    void setPos(Pos pos) {
        this.pos = pos;
    }

    IRubyObject getSource() {
        return source;
    }

    void setSource(IRubyObject source) {
        this.source = source;
    }

    Type getType() {
        return type;
    }

    void setType(Type type) {
        this.type = type;
    }

    static class Pos {

        private int line;
        private String methodName;

        public int getLine() {
            return line;
        }

        public void setLine(int line) {
            this.line = line;
        }

        public String getMethodName() {
            return methodName;
        }

        public void setMethodName(String methodName) {
            this.methodName = methodName;
        }
    }
}
