#!/usr/bin/env ruby
require 'test/unit'

# begin require 'rubygems' rescue LoadError end
# require 'ruby-debug'; Debugger.start

# Test finish command
class TestFinish < Test::Unit::TestCase

  @@SRC_DIR = File.dirname(__FILE__) unless 
    defined?(@@SRC_DIR)

  require File.join(@@SRC_DIR, 'helper')
  include TestHelper

  def test_basic
    testname='finish'
    # Ruby 1.8.6 and earlier have a trace-line number bug for return
    # statements.
    filter = Proc.new{|got_lines, correct_lines|
      [got_lines[34], correct_lines[34]].each do |s|
        s.sub!(/gcd.rb:\d+/, 'gcd.rb:13')
      end
    }
    Dir.chdir(@@SRC_DIR) do 
      script = File.join('data', testname + '.cmd')
      assert_equal(true, 
                   run_debugger(testname,
                                "--script #{script} -- gcd.rb 3 5"))
    end
  end
end
