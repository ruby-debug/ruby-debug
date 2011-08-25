#!/usr/bin/env ruby
require File.expand_path("../helper", __FILE__)

# Test Quit command
class TestQuit < Test::Unit::TestCase
  include TestHelper

  def test_basic
    testname='quit'
    script = File.join('data', testname + '.cmd')
    assert(run_debugger(testname, "--script #{script} -- ./example/gcd.rb"))
  end
end
