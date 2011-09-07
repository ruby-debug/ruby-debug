#!/usr/bin/env ruby
require File.expand_path("../helper", __FILE__)

# Test breakpoint commands
class TestBreakpoints < Test::Unit::TestCase
  include TestHelper

  def test_basic
    testname='breakpoints'
    script = File.join('data', testname + '.cmd')
    assert(run_debugger(testname, "--script #{script} -- ./example/gcd.rb 3 5"))
  end

  def test_breakpoint_in_files_with_same_basename
    filter = proc do |got_lines, correct_lines|
      got_lines.each {|s| s.gsub!(/[^\s]*\/example\/(a|b)\/example.rb/, 'example/\1/example.rb') }
    end

    testname='breakpoints-basename'
    script = File.join('data', testname + '.cmd')
    assert(run_debugger(testname, "--script #{script} -- ./example/#{testname}.rb",
                        :filter => filter))
  end
end
