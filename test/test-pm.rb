#!/usr/bin/env ruby
require File.expand_path("../helper", __FILE__)

# Test Post-mortem command
class TestPM < Test::Unit::TestCase
  include TestHelper

  # Test post-mortem handling
  def test_basic
#       filter = Proc.new{|got_lines, correct_lines|
#         [got_lines[0], correct_lines[0]].each do |s|
#           s.sub!(/tdebug.rb:\d+/, 'rdebug:999')
#         end
#       }
    ENV['COLUMNS'] = '80'
    testname='post-mortem'
    script = File.join('data', testname + '.cmd')
    testname += '-osx' if Config::CONFIG['host_os'] =~ /^darwin/
    assert(run_debugger(testname, "--script #{script} --post-mortem ./example/pm.rb"))
  end

  # Test post-mortem handling
  def test_pm_next
    ENV['COLUMNS'] = '80'
    testname='post-mortem-next'
    script = File.join('data', testname + '.cmd')
    assert(run_debugger(testname, "--script #{script} --post-mortem ./example/pm.rb"))
  end

  # Test Tracker #22118 post-mortem giving an error in show internal variables
  def test_pm_iv_bug
    ENV['COLUMNS'] = '80'
    testname='pm-bug'
    script = File.join('data', testname + '.cmd')
    assert(run_debugger(testname, "--script #{script} --post-mortem example/pm-bug.rb"))
  end
end
