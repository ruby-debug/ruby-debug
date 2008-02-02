#!/usr/bin/env ruby
require 'test/unit'

# require 'rubygems'
# require 'ruby-debug'; Debugger.start

SRC_DIR = File.dirname(__FILE__) unless 
  defined?(SRC_DIR)

require File.join(SRC_DIR, 'helper.rb')

include TestHelper

# Test Post-mortem command
class TestPM < Test::Unit::TestCase

  # Test post-mortem handling
  def test_basic
    Dir.chdir(SRC_DIR) do 
#       filter = Proc.new{|got_lines, correct_lines|
#         [got_lines[0], correct_lines[0]].each do |s|
#           s.sub!(/tdebug.rb:\d+/, 'rdebug:999')
#         end
#       }
      ENV['COLUMNS'] = '80'
      assert_equal(true, 
                   run_debugger('post-mortem', 
                                '--script post-mortem.cmd --post-mortem pm.rb'))
    end
  end
end
