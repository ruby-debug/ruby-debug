#!/usr/bin/env ruby
require File.expand_path("../helper", __FILE__)

# Test info commands
class TestInfo < Test::Unit::TestCase
  include TestHelper

  # Test commands in info.rb
  def test_basic
    testname='info'
    filter = Proc.new{|got_lines, correct_lines|
      got_lines.each do |s|
        s.gsub!(/Line 4 of ".*gcd.rb"/, 'Line 4 of "gcd.rb"')
      end
    }

    script = File.join('data', testname + '.cmd')
    assert(run_debugger(testname, "--script #{script} -- ./example/gcd.rb 3 5",
                        :filter => filter))
  end

  def test_file_break
    testname='info-file-break'
    script = File.join('data', testname + '.cmd')
    assert(run_debugger(testname, "--script #{script} -- ./example/gcd.rb 3 5"))
  end unless defined?(JRUBY_VERSION) # JRuby doesn't yet support tracelines
end
