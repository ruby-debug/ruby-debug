package org.jruby.debug;

import org.jruby.Ruby;
import org.jruby.RubyClass;
import org.jruby.RubyFixnum;
import org.jruby.RubyObject;
import org.jruby.runtime.Block;
import org.jruby.runtime.builtin.IRubyObject;

public class Breakpoint extends RubyObject {

    protected Breakpoint(Ruby runtime, RubyClass type) {
        super(runtime, type);
    }

    DebugBreakpoint debuggerBreakpoint() {
        return (DebugBreakpoint)dataGetStruct();
    }

    public RubyFixnum id(Block block) {
        return getRuntime().newFixnum(debuggerBreakpoint().getId());
    }

    public IRubyObject source(Block block) {
        return debuggerBreakpoint().getSource();
    }

    public IRubyObject source_set(IRubyObject source, Block block) {
        throw new UnsupportedOperationException("not implemented yet");
    }

    public IRubyObject pos(Block block) {
        return getRuntime().newFixnum(debuggerBreakpoint().getPos().getLine());
    }

    public IRubyObject pos_set(IRubyObject pos, Block block) {
        throw new UnsupportedOperationException("not implemented yet");
    }

    public IRubyObject expr(Block block) {
        return debuggerBreakpoint().getExpr();
    }

    public IRubyObject expr_set(IRubyObject expr, Block block) {
        throw new UnsupportedOperationException("not implemented yet");
    }

    public IRubyObject hit_count(Block block) {
        return getRuntime().newFixnum(debuggerBreakpoint().getHitCount());
    }

    public IRubyObject hit_value(Block block) {
        throw new UnsupportedOperationException("not implemented yet");
    }

    public IRubyObject hit_value_set(IRubyObject hit_value, Block block) {
        throw new UnsupportedOperationException("not implemented yet");
    }

    public IRubyObject hit_condition(Block block) {
        throw new UnsupportedOperationException("not implemented yet");
    }

    public IRubyObject hit_condition_set(IRubyObject hit_condition, Block block) {
        throw new UnsupportedOperationException("not implemented yet");
    }
}
