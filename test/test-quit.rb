#!/usr/bin/env ruby
require "test/unit"
require "fileutils"

# require "rubygems" 
# require "ruby-debug"; Debugger.start

SRC_DIR = File.expand_path(File.dirname(__FILE__)) + "/" unless 
  defined?(SRC_DIR)

require File.join(SRC_DIR, "helper.rb")

include TestHelper

# Test frame commands
class TestQuit < Test::Unit::TestCase
  require 'stringio'

  # Test commands in stepping.rb
  def test_basic
    Dir.chdir(SRC_DIR) do 
      filter = Proc.new{|got_lines, correct_lines|
        [got_lines[0], correct_lines[0]].each do |s|
          s.sub!(/tdebug.rb:\d+/, 'rdebug:999')
        end
      }
      assert_equal(true, 
                   run_debugger("quit", 
                                "--script quit.cmd --no-quit -- null.rb",
                                nil, filter))
    end
  end
end
