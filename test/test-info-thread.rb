#!/usr/bin/env ruby
require File.expand_path("../helper", __FILE__)

# Test simple thread commands
class TestInfoThread < Test::Unit::TestCase
  include TestHelper

  def test_basic
    testname='info-thread'
    filter = Proc.new{|got_lines, correct_lines|
      [got_lines, correct_lines].each do |a|
        a.each do |s|
          s.gsub!(/Thread:0x[0-9a-f]+/, 'Thread:0x12345678')
        end
      end
      got_lines.each do |s|
        s.gsub!(/run>[ \t]+.*gcd.rb:4/, "run> gcd.rb:4")
      end
    }
    script = File.join('data', testname + '.cmd')
    assert(run_debugger(testname, "--script #{script} -- ./example/gcd.rb 3 5",
                        :filter => filter))
  end
end
