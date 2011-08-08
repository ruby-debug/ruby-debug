#!/usr/bin/env ruby
require File.expand_path("../helper", __FILE__)

# Test Display commands
class TestDisplay < Test::Unit::TestCase
  include TestHelper

  # Test commands in display.rb
  def test_basic
    testname='display'
    script = File.join('data', testname + '.cmd')
    assert(run_debugger(testname, "--script #{script} -- ./example/gcd.rb 3 5"))
  end
end
