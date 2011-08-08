#!/usr/bin/env ruby
require File.expand_path("../helper", __FILE__)

class TestMethod < Test::Unit::TestCase
  include TestHelper

  def test_basic
    testname='method'
    script = File.join('data', testname + '.cmd')
    assert(run_debugger(testname, "--script #{script} -- ./example/classes.rb"))
    begin
      require 'methodsig'
      testname='methodsig'
      script = File.join('data', testname + '.cmd')
      assert(run_debugger(testname, "--script #{script} -- ./example/classes.rb"))
    rescue LoadError
      puts "Skipping method sig test"
    end
  end
end
