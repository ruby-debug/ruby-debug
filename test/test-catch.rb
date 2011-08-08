#!/usr/bin/env ruby
require File.expand_path("../helper", __FILE__)

# Test condition command
class TestCatch < Test::Unit::TestCase
  include TestHelper

  def test_basic
    testname='catch'
    script = File.join('data', testname + '.cmd')
    assert(run_debugger(testname, "--script #{script} -- ./example/pm.rb"))
  end
end
