/*
 * header & license
 * Copyright (c) 2007-2008 Martin Krauskopf
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
import org.jruby.RubyFixnum;
import org.jruby.RubyNumeric;
import org.jruby.RubyObject;
import org.jruby.RubySymbol;
import org.jruby.anno.JRubyMethod;
import org.jruby.runtime.Block;
import org.jruby.runtime.builtin.IRubyObject;

public class Breakpoint extends RubyObject {
    
    private static final long serialVersionUID = 1L;

    protected Breakpoint(Ruby runtime, RubyClass type) {
        super(runtime, type);
    }

    DebugBreakpoint debuggerBreakpoint() {
        return (DebugBreakpoint)dataGetStruct();
    }

    @JRubyMethod(name="enabled=", required=1)
    public IRubyObject setEnabled(IRubyObject enabled, Block block) {
        debuggerBreakpoint().setEnabled(enabled.isTrue());
        return enabled;
    }

    @JRubyMethod(name="enabled?")
    public IRubyObject isEnabled(Block block) {
        return getRuntime().newBoolean(debuggerBreakpoint().isEnabled());
    }

    @JRubyMethod(name="id")
    public RubyFixnum id(Block block) {
        return getRuntime().newFixnum(debuggerBreakpoint().getId());
    }

    @JRubyMethod(name="source")
    public IRubyObject source(Block block) {
        return debuggerBreakpoint().getSource();
    }

    @JRubyMethod(name="source=", required=1)
    public IRubyObject source_set(IRubyObject source, Block block) {
        debuggerBreakpoint().setSource(source.convertToString());
        
        return source;
    }

    @JRubyMethod(name="pos")
    public IRubyObject pos(Block block) {
        DebugBreakpoint debugBreakpoint = debuggerBreakpoint();
        if (debugBreakpoint.getType() == DebugBreakpoint.Type.METHOD) {
            return getRuntime().newString(debuggerBreakpoint().getPos().getMethodName());
        } else {
            return getRuntime().newFixnum(debuggerBreakpoint().getPos().getLine());    
        }
    }

    @JRubyMethod(name="pos=", required=1)
    public IRubyObject pos_set(IRubyObject pos, Block block) {
        DebugBreakpoint debugBreakpoint = debuggerBreakpoint();
        if (debugBreakpoint.getType() == DebugBreakpoint.Type.METHOD) {
            debugBreakpoint.getPos().setMethodName(pos.convertToString().toString());
        } else {
            debugBreakpoint.getPos().setLine(RubyNumeric.fix2int(pos));
        }
        
        return pos;
    }

    @JRubyMethod(name="expr")
    public IRubyObject expr(Block block) {
        return debuggerBreakpoint().getExpr();
    }

    @JRubyMethod(name="expr=", required=1)
    public IRubyObject expr_set(IRubyObject expr, Block block) {
        debuggerBreakpoint().setExpr(expr.isNil() ? expr : expr.convertToString());
        return expr;
    }

    @JRubyMethod(name="hit_count")
    public IRubyObject hit_count(Block block) {
        return getRuntime().newFixnum(debuggerBreakpoint().getHitCount());
    }

    @JRubyMethod(name="hit_value")
    public IRubyObject hit_value(Block block) {
        return getRuntime().newFixnum(debuggerBreakpoint().getHitValue());
    }

    @JRubyMethod(name="hit_value=", required=1)
    public IRubyObject hit_value_set(IRubyObject hit_value, Block block) {
        debuggerBreakpoint().setHitValue(RubyNumeric.fix2int(hit_value));
        
        return hit_value;
    }

    @JRubyMethod(name="hit_condition")
    public IRubyObject hit_condition(Block block) {
        DebugBreakpoint.HitCondition cond = debuggerBreakpoint().getHitCondition();
        if (cond == null) {
            return getRuntime().getNil();
        } else {
            switch (cond) {
            case GE:
                return getRuntime().newSymbol("greater_or_equal");
            case EQ:
                return getRuntime().newSymbol("equal");
            case MOD:
                return getRuntime().newSymbol("modulo");
            case NONE:
            default:
                return getRuntime().getNil();
            }
        }
    }

    @JRubyMethod(name="hit_condition=", required=1)
    public IRubyObject hit_condition_set(IRubyObject hit_condition, Block block) {
        DebugBreakpoint debugBreakpoint = debuggerBreakpoint();
        
        if (! getRuntime().getSymbol().isInstance(hit_condition)) {
            throw getRuntime().newArgumentError("Invalid condition parameter");
        }
        
        String symbol = ((RubySymbol)hit_condition).asJavaString();
        if (symbol.equals("greater_or_equal") || symbol.equals("ge")) {
            debugBreakpoint.setHitCondition(DebugBreakpoint.HitCondition.GE);
        } else if (symbol.equals("equal") || symbol.equals("eq")) {
            debugBreakpoint.setHitCondition(DebugBreakpoint.HitCondition.EQ);
        } else if (symbol.equals("modulo") || symbol.equals("mod")) {
            debugBreakpoint.setHitCondition(DebugBreakpoint.HitCondition.MOD);
        } else {
            throw getRuntime().newArgumentError("Invalid condition parameter");
        }
        
        return hit_condition;
    }
}
