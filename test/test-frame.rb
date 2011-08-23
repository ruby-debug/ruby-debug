#!/usr/bin/env ruby
require File.expand_path("../helper", __FILE__)

# Test frame commands
class TestFrame < Test::Unit::TestCase
  include TestHelper

  # Test commands in frame.rb
  def test_basic
    testname='frame'
    script = File.join('data', testname + '.cmd')
    assert(run_debugger(testname, "--script #{script} -- ./example/gcd.rb 3 5"))
  end

  def test_bad_continue
    # Remove absolute paths in output.
    filter = Proc.new{|got_lines, correct_lines|
      [got_lines, correct_lines].flatten.each do |s|
        s.sub!(/in file ".*gcd.rb/, 'in file "gcd.rb')
      end
    }

    testname='continue_bad'
    script = File.join('data', testname + '.cmd')
    assert(run_debugger(testname, "--script #{script} -- ./example/gcd.rb 3 5",
                        :filter => filter))
  end unless defined?(JRUBY_VERSION) # JRuby doesn't yet support tracelines
end
