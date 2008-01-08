#!/usr/bin/env ruby

# Use this to run rdebug without installing it.  We assume that the
# library directories are stored at the same level the directory
# this program. 
module RDebugRunner
  def runner(stdout=nil)
    dirname=File.dirname(__FILE__)
    libs=%w(ext lib cli).map {|lib| File.join(dirname, lib) }
    # old_search=$:
    $:.unshift(*libs)
    rdebug=ENV['RDEBUG'] || File.join(dirname, 'bin', 'rdebug')
    if stdout
      old_stdout = $stdout
      $stdout.reopen(stdout)
    else
      old_stdout = nil
    end
    load(rdebug)
    $stdout.reopen(old_stdout) if old_stdout
    # $: = old_search
  end
  module_function :runner
end
if __FILE__ == $0 or
    ($DEBUG and ['rcov', 'rdebug'].include?(File.basename($0)))
  include RDebugRunner
  runner
end
