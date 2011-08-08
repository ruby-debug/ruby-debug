#!/usr/bin/env ruby
require File.expand_path("../helper", __FILE__)

# Test 'starting' annotation.
class TestStartingAnnotate < Test::Unit::TestCase
  include TestHelper

  def test_basic
    testname='output'
    script = File.join('data', testname + '.cmd')
    assert(run_debugger(testname, "-A 3 --script #{script} -- ./example/output.rb"))
  end
end
