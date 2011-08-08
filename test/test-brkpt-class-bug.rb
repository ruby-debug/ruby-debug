#!/usr/bin/env ruby
require File.expand_path("../helper", __FILE__)

# Test (mostly) invalid breakpoint commands
class TestBrkptClassBug < Test::Unit::TestCase
  include TestHelper

  def test_basic
    testname='brkpt-class-bug'
    script = File.join('data', testname + '.cmd')
    assert(run_debugger(testname, "--script #{script} -- ./example/brkpt-class-bug.rb"))
  end
end
