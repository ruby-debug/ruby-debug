#!/usr/bin/env ruby
require File.expand_path("../helper", __FILE__)

# Test (mostly) invalid breakpoint commands
class TestBadBreak < Test::Unit::TestCase
  include TestHelper

  def test_basic
    testname = 'break_bad'
    script = File.join('data', testname + '.cmd')
    assert(run_debugger(testname, "--script #{script} -- ./example/gcd.rb 3 5"))
  end

  def test_break_loop
    testname = 'break_loop_bug'
    script = File.join('data', testname + '.cmd')
    assert(run_debugger(testname, "--script #{script} -- ./example/bp_loop_issue.rb"))
  end
end
