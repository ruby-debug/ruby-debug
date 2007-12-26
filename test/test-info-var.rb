#!/usr/bin/env ruby
require "test/unit"
require "fileutils"

# require "rubygems"
# require "ruby-debug" ; Debugger.start

SRC_DIR = File.expand_path(File.dirname(__FILE__)) + "/" unless 
  defined?(SRC_DIR)

require File.join(SRC_DIR, "helper.rb")
include TestHelper

# Test info variables command
class TestInfoVar < Test::Unit::TestCase

  def test_info_variables

    Dir.chdir(SRC_DIR) do 

      filter = Proc.new{|got_lines, correct_lines|
        [got_lines[13-1], correct_lines[13-1]].each do |s|
          s.sub!(/Mine:0x[0-9,a-f]+/, 'Mine:')
        end
        [got_lines, correct_lines].each do |a|
          a.each do |s|
            s.sub!(/Lousy_inspect:0x[0-9,a-f]+/, 'Lousy_inspect:')
          end
        end
      }

      assert_equal(true, 
                   run_debugger("info-var", 
                                "--script info-var.cmd -- info-var-bug.rb",
                                nil, filter))
      assert_equal(true, 
                   run_debugger("info-var-bug2", 
                                "--script info-var-bug2.cmd -- info-var-bug2.rb",
                                nil))

    end
  end
end
