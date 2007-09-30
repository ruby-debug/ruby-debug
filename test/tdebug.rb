#!/usr/bin/env ruby
# -*- Ruby -*-
# This is a hacked down copy of rdebug which can be used for testing

require 'stringio'
require 'rubygems'
require 'optparse'
require "ostruct"
require 'ruby-debug'

options = OpenStruct.new(
  'wait'        => false,
  'nostop'      => false,
  'post_mortem' => false,
  'script'      => nil,
  'tracing'     => false,
  'frame_bind'  => false,
  'verbose_long'=> false
)

program = File.basename($0)
opts = OptionParser.new do |opts|
  opts.banner = <<EOB
#{program} #{Debugger::VERSION}
Usage: #{program} [options] <script.rb> -- <script.rb parameters>
EOB
  opts.separator ""
  opts.separator "Options:"
  opts.on("-d", "--debug", "Set $DEBUG=true") {$DEBUG = true}
  opts.on("-x", "--trace", "turn on line tracing") {options.tracing = true}
  opts.on("-n", "--nostop", "Do not stop when stript is loaded") {options.nostop = true}
  opts.on("-m", "--post-mortem", "Activate post-mortem mode") {options.post_mortem = true}
  opts.on("-I", "--include PATH", String, "Add PATH to $LOAD_PATH") do |path|
    $LOAD_PATH.unshift(path)
  end
  opts.on("--script FILE", String, "Name of the script file to run") do |options.script| 
    unless File.exists?(options.script)
      puts "Script file '#{options.script}' is not found"
      exit
    end
  end
  opts.on("-r", "--require SCRIPT", String,"Require the library, before executing your script") do |name|
    if name == 'debug'
      puts "ruby-debug is not compatible with Ruby's 'debug' library. This option is ignored."
    else
      require name
    end
  end
  opts.on("--keep-frame-binding", "Keep frame bindings") {options.frame_bind = true}
  ENV['EMACS'] = nil
  opts.separator ""
  opts.separator "Common options:"
  opts.on_tail("--help", "Show this message") do
    puts opts
    exit
  end
  opts.on_tail("--version", 
               "print the version") do
    puts "ruby-debug #{Debugger::VERSION}"
    exit
  end
  opts.on("--verbose", "turn on verbose mode") do
    $VERBOSE = true
    options.verbose_long = true
  end
  opts.on_tail("-v", 
               "print version number, then turn on verbose mode") do
    puts "ruby-debug #{Debugger::VERSION}"
    $VERBOSE = true
  end
end

begin
  if not defined? Debugger::ARGV
    Debugger::ARGV = ARGV.clone
  end
  rdebug_path = File.expand_path($0)
  if RUBY_PLATFORM =~ /mswin/
    rdebug_path += ".cmd" unless rdebug_path =~ /\.cmd$/i
  end
  Debugger::RDEBUG_SCRIPT = rdebug_path
  Debugger::INITIAL_DIR = Dir.pwd
  opts.parse! ARGV
rescue StandardError => e
  puts opts
  puts
  puts e.message
  exit(-1)
end

if ARGV.empty?
  exit if $VERBOSE and not options.verbose_long
  puts opts
  puts
  puts "Must specify a script to run"
  exit(-1)
end
  
# save script name
Debugger::PROG_SCRIPT = ARGV.shift

# install interruption handler
trap('INT') { Debugger.interrupt_last }

# set options
Debugger.wait_connection = false
Debugger.keep_frame_binding = options.frame_bind

# activate debugger
Debugger.start

# activate post-mortem
Debugger.post_mortem if options.post_mortem

# Set up an interface to read commands from a debugger script file.
Debugger.interface = Debugger::ScriptInterface.new(options.script, 
                                                   STDOUT, true)
Debugger.tracing = options.nostop = true if options.tracing

# Make sure Ruby script syntax checks okay.
# Otherwise we get a load message that looks like rdebug has 
# a problem. 
output = `ruby -c #{Debugger::PROG_SCRIPT} 2>&1`
if $?.exitstatus != 0 and RUBY_PLATFORM !~ /mswin/
  puts output
  exit $?.exitstatus 
end
Debugger.debug_load Debugger::PROG_SCRIPT, true

