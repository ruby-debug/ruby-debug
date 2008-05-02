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

import java.io.File;
import java.io.IOException;
import org.jruby.RubyBoolean;
import org.jruby.runtime.EventHook;
import org.jruby.runtime.builtin.IRubyObject;

final class Util {
    
    private final static CharSequence JRUBY_BUILTIN_PATH_PART = "builtin" + File.separator + "javasupport";

    private Util() {/* forbid instances */}

    /**
     * Convenient delegate to {@link RubyBoolean#newBoolean} using <em>ro</em>'s
     * runtime.
     */
    static RubyBoolean toRBoolean(IRubyObject ro, boolean value) {
        return RubyBoolean.newBoolean(ro.getRuntime(), value);
    }

    /**
     * Convenient delegate to {@link org.jruby.Ruby#getNil} using
     * <em>recv</em>'s runtime.
     */
    static IRubyObject nil(final IRubyObject ro) {
        return ro.getRuntime().getNil();
    }

    static String relativizeToPWD(final String path) {
        return Util.relativizeFile(System.getProperty("user.dir"), path);
    }

    static String relativizeFile(final String base, final String filepath) {
        String result = filepath;
        if (filepath.startsWith(base)) {
            result = filepath.substring(base.length() + 1);
        }
        return result;
    }

    /**
     * Tests whether the give files, being in whatever form (relative, absolute,
     * containing '..', etc.) points to the same files, using canonical paths.
     *
     * @param first to be compared with second
     * @param second to be compared with first
     * @return if first and second are/points to the same files
     */
    static boolean areSameFiles(String first, String second) {
        try {
            String firstF = new File(first).getCanonicalPath();
            String secondF = new File(second).getCanonicalPath();
            return firstF.equals(secondF);
        } catch (IOException ioe) {
            throw new RuntimeException("Cannot resolve cannocical path", ioe);
        }
    }

    static void logEvent(int event, String file, int line, String methodName, IRubyObject klass) {
        System.err.printf("%s:%d [%s] %s#%s\n", file, line, EventHook.EVENT_NAMES[event], klass, methodName);
    }

    static boolean isJRubyCore(final String file) {
        return file.contains(JRUBY_BUILTIN_PATH_PART);
    }

}

