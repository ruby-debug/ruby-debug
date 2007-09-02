package org.jruby.debug;

import org.jruby.RubyBoolean;
import org.jruby.RubyFixnum;
import org.jruby.runtime.builtin.IRubyObject;

final class Util {

    static RubyBoolean toRBoolean(IRubyObject recv, boolean value) {
        return RubyBoolean.newBoolean(recv.getRuntime(), value);
    }

    static boolean toBoolean(final IRubyObject bValue) {
        return ((RubyBoolean) bValue).isTrue();
    }
    
    static int toInt(final IRubyObject iValue) {
        return RubyFixnum.fix2int(iValue);
    }

    static IRubyObject nil(final IRubyObject ro) {
        return ro.getRuntime().getNil();
    }
}
