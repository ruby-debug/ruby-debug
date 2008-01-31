#!/usr/bin/env ruby
require 'test/unit'

# require 'rubygems'
# require 'ruby-debug'; Debugger.init

SRC_DIR = File.dirname(__FILE__) unless 
  defined?(SRC_DIR)

require File.join(SRC_DIR, 'helper.rb')

include TestHelper

# Test Quit command
class TestQuit < Test::Unit::TestCase

  # Test commands in stepping.rb
  def test_basic
    Dir.chdir(SRC_DIR) do 
#       filter = Proc.new{|got_lines, correct_lines|
#         [got_lines[0], correct_lines[0]].each do |s|
#           s.sub!(/tdebug.rb:\d+/, 'rdebug:999')
#         end
#       }
      assert_equal(true, 
                   run_debugger("quit", 
                                "--script quit.cmd -- null.rb"))
    end
  end
end
