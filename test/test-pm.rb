#!/usr/bin/env ruby
require 'test/unit'

# begin require 'rubygems' rescue LoadError end
# require 'ruby-debug'; Debugger.start

# Test Post-mortem command
class TestPM < Test::Unit::TestCase

  @@SRC_DIR = File.dirname(__FILE__) unless 
    defined?(@@SRC_DIR)

  require File.join(@@SRC_DIR, 'helper')
  include TestHelper

  # Test post-mortem handling
  def test_basic
    Dir.chdir(@@SRC_DIR) do 
#       filter = Proc.new{|got_lines, correct_lines|
#         [got_lines[0], correct_lines[0]].each do |s|
#           s.sub!(/tdebug.rb:\d+/, 'rdebug:999')
#         end
#       }
      ENV['COLUMNS'] = '80'
      testname='post-mortem'
      script = File.join('data', testname + '.cmd')
      assert_equal(true, 
                   run_debugger(testname,
                                "--script #{script} --post-mortem pm.rb"))
    end
  end
end
