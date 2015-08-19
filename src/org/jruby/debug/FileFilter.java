package org.jruby.debug;

import org.jruby.Ruby;
import org.jruby.RubyClass;
import org.jruby.RubyObject;
import org.jruby.anno.JRubyMethod;
import org.jruby.runtime.Block;
import org.jruby.runtime.builtin.IRubyObject;

import java.util.HashSet;
import java.util.Set;

public class FileFilter extends RubyObject {
    private final Set<String> includedPaths = new HashSet<String>();
    private final Set<String> excludedPaths = new HashSet<String>();
    private boolean isEnabled = false;

    FileFilter(Ruby runtime, RubyClass type) {
        super(runtime, type);
    }

    @JRubyMethod(name={"include"}, required=1)
    public synchronized IRubyObject include(IRubyObject filePath, Block block) {
      if (filePath == null || filePath.isNil()) {
        return filePath;
      }
      final String path = filePath.convertToString().toString();
      if (!excludedPaths.remove(path)) {
        includedPaths.add(path);
      }
      return filePath;
    }

    @JRubyMethod(name={"exclude"}, required=1)
    public synchronized IRubyObject exclude(IRubyObject filePath, Block block) {
      if (filePath == null || filePath.isNil()) {
        return filePath;
      }
      final String path = filePath.convertToString().toString();
      if (!includedPaths.remove(path)) {
        excludedPaths.add(path);
      }
      return filePath;
    }

    @JRubyMethod(name={"enable"}, required=0)
    public synchronized IRubyObject enable(Block block) {
      isEnabled = true;
      return getRuntime().newBoolean(isEnabled);
    }

    @JRubyMethod(name={"disable"}, required=0)
    public synchronized IRubyObject disable(Block block) {
      isEnabled = false;
      return getRuntime().newBoolean(isEnabled);
    }

    public synchronized boolean isAccepted(final String filePath) {
      if (!isEnabled) return true;
      if (filePath == null) return false;
      return isUnder(includedPaths, filePath) && !isUnder(excludedPaths, filePath);
    }

    private static boolean isUnder(final Set<String> paths, final String filePath) {
        for (final String path : paths) {
            if (filePath.startsWith(path)) return true;
        }
        return false;
    }
}