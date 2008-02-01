#!/usr/bin/env ruby
require 'test/unit'

# Test of C extension ruby_debug.so
  $:.unshift File.join(File.dirname(__FILE__), '..', '..', 'ext')
  require 'ruby_debug'
  
class TestRubyDebugCatchpoint < Test::Unit::TestCase
  # test current_context
  def test_catchpoints
    assert_raise(RuntimeError) {Debugger.catchpoints}
    Debugger.start
    assert_equal({}, Debugger.catchpoints)
    Debugger.add_catchpoint('ZeroDivisionError')
    assert_equal({'ZeroDivisionError' => 0}, Debugger.catchpoints)
    Debugger.add_catchpoint('RuntimeError')
    assert_equal(['RuntimeError', 'ZeroDivisionError'], 
                 Debugger.catchpoints.keys.sort)
    Debugger.stop
  end

end

