#!/usr/bin/env ruby
require File.expand_path("../helper", __FILE__)

# Test frame commands
class TestFrame < Test::Unit::TestCase
  include TestHelper

  # Test commands in frame.rb
  def test_basic
    testname='frame'
    # Ruby 1.8.6 and earlier have a trace-line number bug for return
    # statements.
    filter = Proc.new{|got_lines, correct_lines|
      [got_lines[11], correct_lines[11]].flatten.each do |s|
        s.sub!(/in file ".*gcd.rb/, 'in file "gcd.rb')
      end
    }
    script = File.join('data', testname + '.cmd')
    assert(run_debugger(testname, "--script #{script} -- ./example/gcd.rb 3 5",
                        :filter => filter))
  end

  def test_bad_continue
    testname='continue_bad'
    script = File.join('data', testname + '.cmd')
    assert(run_debugger(testname, "--script #{script} -- ./example/gcd.rb 3 5"))
  end unless defined?(JRUBY_VERSION) # JRuby doesn't yet support tracelines
end
