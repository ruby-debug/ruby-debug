#!/usr/bin/env ruby
require File.expand_path("../helper", __FILE__)

# Test info variables command
class TestInfoVar < Test::Unit::TestCase
  include TestHelper

  def test_info_variables
    filter = Proc.new{|got_lines, correct_lines|
      [got_lines[13-1], correct_lines[13-1]].each do |s|
        s.sub!(/Mine:0x[0-9,a-f]+/, 'Mine:')
      end
      [got_lines, correct_lines].each do |a|
        a.each do |s|
          s.sub!(/Lousy_inspect:0x[0-9,a-f]+/, 'Lousy_inspect:')
          s.sub!(/UnsuspectingClass:0x[0-9,a-f]+/, 'UnsuspectingClass:')
        end
      end
    }

    testname='info-var'
    script = File.join('data', testname + '.cmd')
    assert(run_debugger(testname, "--script #{script} -- ./example/info-var-bug.rb",
                        :filter => filter))
    
    testname='info-var-bug2'
    script = File.join('data', testname + '.cmd')
    assert(run_debugger(testname, "--script #{script} -- ./example/info-var-bug2.rb"))
  end
end
