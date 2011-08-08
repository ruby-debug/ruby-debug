#!/usr/bin/env ruby
require File.expand_path("../helper", __FILE__)

# Test List commands
class TestList < Test::Unit::TestCase
  include TestHelper

  # Test commands in list.rb
  def test_basic
    testname='list'
    script = File.join('data', testname + '.cmd')
    assert(run_debugger(testname, "--script #{script} -- ./example/gcd.rb 3 5"))
  end
end
