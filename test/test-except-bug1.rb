#!/usr/bin/env ruby
require File.expand_path("../helper", __FILE__)

# Bug in Post-mortem command was not being able to show
# variables on stack when stack stopped in a FIXNUM from 1/0.
class TestExceptBug1 < Test::Unit::TestCase
  include TestHelper

  # Test post-mortem handling
  def test_pm_except_bug
    ENV['COLUMNS'] = '80'
    testname='except-bug1'
    script = File.join('data', testname + '.cmd')
    assert(run_debugger(testname, "--script #{script} --post-mortem ./example/#{testname}.rb"))
  end
end unless defined?(JRUBY_VERSION) # post-mortem not yet supported on JRuby
