#!/usr/bin/env ruby
require "test/unit"

$: << File.expand_path(File.dirname(__FILE__)) + '/../ext'
$: << File.expand_path(File.dirname(__FILE__)) + '/../cli/ruby-debug'
require "ruby_debug"

# Test of C extension ruby_debug.so
class TestRubyDebug < Test::Unit::TestCase
  include Debugger
  include Debugger

  # Test initial variables and setting/getting state.
  def test_basic
    assert_equal(false, Debugger.started?, 
                 "Debugger should not initially be started.")
    Debugger.start
    assert(Debugger.started?, 
           "Debugger should now be started.")
    assert_equal(false, Debugger.debug,
                 "Debug variable should not be set.")
    assert_equal(false, Debugger.post_mortem?,
                 "Post mortem debugging should not be set.")
    a = Debugger.contexts
    assert_equal(1, a.size, 
                 "There should only be one context.")
    assert_equal(Array, a.class, 
                 "Context should be an array.")
    Debugger.stop
    assert_equal(false, Debugger.started?, 
                 "Debugger should no longer be started.")
  end

  # Test breakpoint handling
  def test_breakpoints
    Debugger.start
    assert_equal(0, Debugger.breakpoints.size,
                 "There should not be any breakpoints set.")
    brk = Debugger.add_breakpoint(__FILE__, 1)
    assert_equal(Debugger::Breakpoint, brk.class,
                 "Breakpoint should have been set and returned.")
    assert_equal(1, Debugger.breakpoints.size,
                 "There should now be one breakpoint set.")
    Debugger.remove_breakpoint(0)
    assert_equal(1, Debugger.breakpoints.size,
                 "There should still be one breakpoint set.")
    Debugger.remove_breakpoint(1)
    assert_equal(0, Debugger.breakpoints.size,
                 "There should no longer be any breakpoints set.")
    Debugger.stop
  end

  require "command.rb"
  include ColumnizeFunctions
  def test_columize
    assert_equal("one  two  three\n", columnize(["one", "two", "three"]))
    assert_equal("oneitem\n", columnize(["oneitem"]))
    assert_equal(
"one    6hree  11o    16e    21ree  26o    31e    36ree  41o    46e    three\n" +
"two    7ne    12ree  17o    22e    27ree  32o    37e    42ree  47o  \n" +
"three  8wo    13e    18ree  23o    28e    33ree  38o    43e    48ree\n" +
"4ne    9hree  14o    19e    24ree  29o    34e    39ree  44o    one  \n" +
"5wo    10e    15ree  20o    25e    30ree  35o    40e    45ree  two  \n",
                 columnize([
                            "one", "two", "three",
                            "4ne", "5wo", "6hree",
                            "7ne", "8wo", "9hree",
                            "10e", "11o", "12ree",
                            "13e", "14o", "15ree",
                            "16e", "17o", "18ree",
                            "19e", "20o", "21ree",
                            "22e", "23o", "24ree",
                            "25e", "26o", "27ree",
                            "28e", "29o", "30ree",
                            "31e", "32o", "33ree",
                            "34e", "35o", "36ree",
                            "37e", "38o", "39ree",
                            "40e", "41o", "42ree",
                            "43e", "44o", "45ree",
                            "46e", "47o", "48ree",
                            "one", "two", "three"]))

  end
end

