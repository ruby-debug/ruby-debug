#!/usr/bin/env ruby
require 'test/unit'

# begin require 'rubygems' rescue LoadError end
# require 'ruby-debug'; Debugger.start

# Test 'edit' command handling.
class TestEdit < Test::Unit::TestCase

  @@SRC_DIR = File.dirname(__FILE__) unless 
    defined?(@@SRC_DIR)

  require File.join(@@SRC_DIR, 'helper')
  include TestHelper

  def test_basic

    filter = Proc.new{|got_lines, correct_lines|
        got_lines.collect!{|l| l =~ /:gcd\.rb:/? l : nil}.compact!
      puts got_lines
      }

    testname='trace'
    Dir.chdir(@@SRC_DIR) do 
      assert_equal(true, 
                   run_debugger(testname,
                                "-nx --trace gcd.rb 3 5", nil, filter))
    end
  end
end
