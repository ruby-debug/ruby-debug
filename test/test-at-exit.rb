#!/usr/bin/env ruby
require File.expand_path("../helper", __FILE__)

# Test that it's possible to stop at breakpoints in an at_exit block.
class TestAtExit < Test::Unit::TestCase
  include TestHelper

  def test_basic
    testname = 'at-exit'
    script = File.join('data', testname + '.cmd')
    assert(run_debugger(testname, "--script #{script} -- ./example/at-exit.rb"))
  end
end
