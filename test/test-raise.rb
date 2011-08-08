#!/usr/bin/env ruby
require File.expand_path("../helper", __FILE__)

# Test Debugger.load handles uncaught exceptions in the debugged program.
class TestRaise < Test::Unit::TestCase
  include TestHelper

  def test_basic
    testname='raise'
    script = File.join('data', testname + '.cmd')
    assert(run_debugger(testname, "--script #{script} -- ./example/raise.rb"))
  end
end
