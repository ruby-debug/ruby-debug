#!/usr/bin/env ruby
require File.expand_path("../helper", __FILE__)

class TestSetShow < Test::Unit::TestCase
  include TestHelper

  # Test initial variables and setting/getting state.
  def test_basic
    testname='setshow'
    script = File.join('data', testname + '.cmd')
    assert(run_debugger(testname, "--script #{script} -- ./example/gcd.rb 3 5"))
  end
end
