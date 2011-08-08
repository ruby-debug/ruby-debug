#!/usr/bin/env ruby
require File.expand_path("../helper", __FILE__)

# Test Quit command
class TestQuit < Test::Unit::TestCase
  include TestHelper

  def test_basic
    testname='quit'
    script = File.join('data', testname + '.cmd')
#       filter = Proc.new{|got_lines, correct_lines|
#         [got_lines[0], correct_lines[0]].each do |s|
#           s.sub!(/tdebug.rb:\d+/, 'rdebug:999')
#         end
#       }
    assert(run_debugger(testname, "--script #{script} -- ./example/null.rb"))
  end
end
