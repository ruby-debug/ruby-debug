#!/usr/bin/env ruby
require 'test/unit'

# require 'rubygems' 
# require 'ruby-debug'; Debugger.start(:post_mortem =>true)

class TestSave < Test::Unit::TestCase

  @@SRC_DIR = File.dirname(__FILE__) unless 
    defined?(@@SRC_DIR)

  require File.join(@@SRC_DIR, 'helper')
  include TestHelper

  # Test initial variables and setting/getting state.
  def test_basic
    testname='save'
    filter = Proc.new{|got_lines, correct_lines|
      got_lines.each do |s|
        s.gsub!(/(\d+) file .*gcd.rb/, '\1 file gcd.rb')
      end
      got_lines.each do |s|
        s.gsub!(/break .*gcd.rb:10/, "break gcd.rb:10")
      end
    }
    Dir.chdir(@@SRC_DIR) do 
      script = File.join('data', testname + '.cmd')
      assert_equal(true, 
                   run_debugger(testname,
                                "--script #{script} -- ./example/gcd.rb 3 5",
                                nil, filter))
    end
  end
end
