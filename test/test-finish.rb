#!/usr/bin/env ruby
require File.expand_path("../helper", __FILE__)

# Test finish command
class TestFinish < Test::Unit::TestCase
  include TestHelper

  def test_basic
    # Ruby 1.8.6 and earlier have a trace-line number bug for return
    # statements.
    #     filter = Proc.new{|got_lines, correct_lines|
    #       [got_lines[31], got_lines[34]].flatten.each do |s|
    #         s.sub!(/gcd.rb:\d+/, 'gcd.rb:13')
    #       end
    #       got_lines[32] = 'return a'
    #     }

    testname = 'finish'
    script = File.join('data', testname + '.cmd')
    assert(run_debugger(testname, "--script #{script} -- ./example/gcd.rb 3 5"))
  end
end
