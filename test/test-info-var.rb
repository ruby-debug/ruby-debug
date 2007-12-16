#!/usr/bin/env ruby
require "test/unit"
require "fileutils"

# require "rubygems"
# require "ruby-debug" ; Debugger.start

SRC_DIR = File.expand_path(File.dirname(__FILE__)) + "/" unless 
  defined?(SRC_DIR)

# Test info commands
class TestInfo < Test::Unit::TestCase
  require 'stringio'

  ## FIXME: fix up helper module
  def run_debugger(testname, args='', outfile=nil)
    rightfile = File.join(SRC_DIR, "#{testname}.right")
    
    outfile = File.join(SRC_DIR, "#{testname}.out") unless outfile

    if File.exists?(outfile)
      FileUtils.rm(outfile)
    end
    
    ENV['RDEBUG'] = "#{SRC_DIR}tdebug.rb"
    cmd = "/bin/sh #{File.join(SRC_DIR, '../runner.sh')} #{args} >#{outfile}"
    output = `#{cmd}`
    
    if cheap_diff(File.read(outfile).split(/\n/),
                  File.read(rightfile).split(/\n/))
      FileUtils.rm(outfile)
      return true
    end
    return false
  end

  def cheap_diff(got_lines, correct_lines)
    puts got_lines if $DEBUG
    [got_lines[12], correct_lines[12]].each do |s|
      s.sub!(/Mine:0x[0-9,a-f]+/, 'Mine:')
    end
    correct_lines.each_with_index do |line, i|
      correct_lines[i].chomp!
      if got_lines[i] != correct_lines[i]
        puts "difference found at line #{i+1}"
        puts "got : #{got_lines[i]}"
        puts "need: #{correct_lines[i]}"
        return false
      end
    end
    if correct_lines.size != got_lines.size
      puts("difference in number of lines: " + 
           "#{correct_lines.size} vs. #{got_lines.size}")
      return false
    end
    return true
  end

  # Test commands in stepping.rb
  def test_basic

    Dir.chdir(SRC_DIR) do 
      assert_equal(true, 
                   run_debugger("info-var", 
                                "--script info-var.cmd -- info-var-bug.rb"))
    end
  end
end
