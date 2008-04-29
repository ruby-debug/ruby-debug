/*
 * header & license
 * Copyright (c) 2007-2008 Martin Krauskopf
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
    private boolean enabled;
    private IRubyObject source;
    private Pos pos;
    private IRubyObject expr;
    private int hitCount;
    private int hitValue;
    private HitCondition hitCondition;

    DebugBreakpoint() {
        this.enabled = true;
        this.pos = new Pos();
    }
    
    boolean isEnabled() {
        return enabled;
    }

    void setEnabled(boolean enabled) {
        this.enabled = enabled;
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

        public @Override String toString() {
            return "DebugBreakpoint$Pos[line:" + getLine() + ", methodName:" + getMethodName() + ']';
        }
        
    }
}
