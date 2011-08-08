#!/usr/bin/env ruby
require File.expand_path("../helper", __FILE__)

# Test that linetracing does something.
class TestStepping < Test::Unit::TestCase
  include TestHelper

  # Test commands in stepping.rb
  def test_basic
    testname='stepping'
    script = File.join('data', testname + '.cmd')
    assert(run_debugger(testname, "--script #{script} -- ./example/gcd.rb 3 5"))
  end
end
