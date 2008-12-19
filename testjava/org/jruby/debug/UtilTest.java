/*
 * header & license
 * Copyright (c) 2008 Martin Krauskopf
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

import junit.framework.TestCase;

public class UtilTest extends TestCase {

    public UtilTest(String testName) {
        super(testName);
    }

    public void testAreSameFiles() {
        String first = "a";
        String second = "b/../a";
        assertTrue(Util.areSameFiles(first, second));
        assertFalse(Util.areSameFiles(
                "C:/some valid path/to some valid/file.rb",
                "generated code (C:/some path/to some/file.rb:36)"));
    }
    
    public void testRelativizeFile() {
        assertEquals("./tester.rb", Util.relativizeFile(
                "/a/b/c/d",
                "/a/b/c/d/./tester.rb"));
    }
    
    public void testIsJRubyCore() {
        assertTrue("jruby.jar handled", Util.isJRubyCore("/sources/jruby/lib/jruby.jar!/jruby/path_helper.rb"));
    }
}
