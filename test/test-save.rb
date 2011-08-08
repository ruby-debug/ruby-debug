#!/usr/bin/env ruby
require File.expand_path("../helper", __FILE__)

class TestSave < Test::Unit::TestCase
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
    script = File.join('data', testname + '.cmd')
    assert(run_debugger(testname, "--script #{script} -- ./example/gcd.rb 3 5",
                        :filter => filter))
  end
end
