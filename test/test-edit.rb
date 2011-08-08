#!/usr/bin/env ruby
require File.expand_path("../helper", __FILE__)

# Test 'edit' command handling.
class TestEdit < Test::Unit::TestCase
  include TestHelper

  def test_basic
    testname='edit'
    script = File.join('data', testname + '.cmd')
    ENV['EDITOR']='echo FAKE-EDITOR '
    assert(run_debugger(testname, "--script #{script} -- ./example/gcd.rb 3 5"))
  end
end
