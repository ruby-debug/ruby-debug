#!/usr/bin/env ruby
require 'test/unit'

# begin require 'rubygems' rescue LoadError end
# require 'ruby-debug'; Debugger.start

# Test info commands
class TestInfo < Test::Unit::TestCase

  @@SRC_DIR = File.dirname(__FILE__) unless 
    defined?(@@SRC_DIR)

  require File.join(@@SRC_DIR, 'helper')
  include TestHelper

  # Test commands in info.rb
  def test_basic
    testname='info'
    Dir.chdir(@@SRC_DIR) do 
       filter = Proc.new{|got_lines, correct_lines|
        got_lines.each do |s|
          s.gsub!(/Line 4 of ".*gcd.rb"/, 'Line 4 of "gcd.rb"')
        end
      }

      script = File.join('data', testname + '.cmd')
      assert_equal(true, 
                   run_debugger(testname,
                                "--script #{script} -- ./example/gcd.rb 3 5",
                                nil, filter))
    end
  end
end
