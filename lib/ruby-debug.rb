require 'pp'
require 'stringio'
require "socket"
require 'thread'
require 'ruby_debug.so'
require 'ruby-debug/processor'

SCRIPT_LINES__ = {} unless defined? SCRIPT_LINES__
SCRIPT_TIMESTAMPS__ = {} unless defined? SCRIPT_TIMESTAMPS__

module Debugger
  PORT = 8989

  @processor = CommandProcessor.new
  @reload_source_on_change = false
  
  class Context
    def interrupt
      self.stop_next = 1
    end

    private

    def processor
      Debugger.processor
    end

    def at_breakpoint(breakpoint)
      processor.at_breakpoint(self, breakpoint)
    end

    def at_catchpoint(excpt)
      processor.at_catchpoint(self, excpt)
    end

    def at_tracing(file, line)
      processor.at_tracing(self, file, line)
    end

    def at_line(file, line)
      processor.at_line(self, file, line)
    end
  end

  class << self
    attr_accessor :processor
    
    # stop main thread when remote connection established
    attr_accessor :stop_on_connect
    
    # in remote mode, wait for the remote connection 
    attr_accessor :wait_connection
    
    # if <tt>true</tt>, checks the modification time of source files and reloads if it was modified
    attr_accessor :reload_source_on_change
    
    attr_reader :thread, :control_thread
    
    #
    # Interrupts the current thread
    #
    def interrupt
      current_context.interrupt
    end
    
    #
    # Interrupts the last debugged thread
    #
    def interrupt_last
      if context = last_context
        return nil unless context.thread.alive?
        context.interrupt
      end
      context
    end
    
    def interface=(value) # :nodoc:
      processor.interface = value
    end

    #
    # Starts a remote debugger.
    #
    def start_remote(host = nil, port = PORT, post_mortem = false)
      return if @thread
      return if started?

      self.interface = nil
      start
      self.post_mortem if post_mortem

      if port.kind_of?(Array)
        cmd_port, ctrl_port = port
      else
        cmd_port, ctrl_port = port, port + 1
      end

      start_control(host, ctrl_port)
      
      mutex = Mutex.new
      proceed = ConditionVariable.new
      
      @thread = DebugThread.new do
        server = TCPServer.new(host, cmd_port)
        while (session = server.accept)
          self.interface = RemoteInterface.new(session)
          if wait_connection
            mutex.synchronize do
              proceed.signal
            end
          else
            stop_main_thread
          end
        end
      end
      if wait_connection
        mutex.synchronize do
          proceed.wait(mutex)
        end 
        stop_main_thread
      end
    end
    alias start_server start_remote
    
    def start_control(host = nil, ctrl_port = PORT + 1)
      raise "Debugger is not started" unless started?
      return if @control_thread
      @control_thread = DebugThread.new do
        server = TCPServer.new(host, ctrl_port)
        while (session = server.accept)
          interface = RemoteInterface.new(session)
          processor = ControlCommandProcessor.new(interface)
          processor.process_commands
        end
      end
    end
    
    #
    # Connects to the remote debugger
    #
    def start_client(host = 'localhost', port = PORT)
      require "socket"
      interface = Debugger::LocalInterface.new
      socket = TCPSocket.new(host, port)
      puts "Connected."

      catch(:exit) do
        while (line = socket.gets)
          case line 
          when /^PROMPT (.*)$/
            input = interface.read_command($1)
            throw :exit unless input
            socket.puts input
          when /^CONFIRM (.*)$/
            input = interface.confirm($1)
            throw :exit unless input
            socket.puts input
          else
            print line
          end
        end
      end
      socket.close
    end
    
    def stop_main_thread # :nodoc:
      return unless stop_on_connect
      
      context = thread_context(Thread.main)
      context.stop_next = 2
    end
    private :stop_main_thread

    def source_for(file) # :nodoc:
      finder = lambda do
        unless File.exists?(file)
          return (SCRIPT_LINES__[file] == true ? nil : SCRIPT_LINES__[file])
        end

        if SCRIPT_LINES__[file].nil? || SCRIPT_LINES__[file] == true
          SCRIPT_LINES__[file] = File.readlines(file)
        end

        change_time = test(?M, file)
        SCRIPT_TIMESTAMPS__[file] ||= change_time
        if @reload_source_on_change && SCRIPT_TIMESTAMPS__[file] < change_time
          SCRIPT_LINES__[file] = File.readlines(file)
        end

        SCRIPT_LINES__[file]
      end
      Dir.chdir(File.dirname($0)){finder.call} || finder.call
    end
    
    def source_reload
      SCRIPT_LINES__.keys.each do |file|
        next unless File.exists?(file)
        SCRIPT_LINES__[file] = nil
      end
    end
    
    def line_at(file, line) # :nodoc:
      lines = source_for(file)
      if lines
        line = lines[line-1]
        return "\n" unless line
        return "#{line.gsub(/^\s+/, '').chomp}\n"
      end
      return "\n"
    end

    #
    # Runs a script file
    #
    def run_script(file, out = processor.interface)
      interface = ScriptInterface.new(file, out)
      processor = ControlCommandProcessor.new(interface)
      processor.process_commands
    end

    #
    # Activates the post-mortem mode. There are two ways of using it:
    # 
    # == Global post-mortem mode
    # By calling Debugger.post_mortem method without a block, you install
    # at_exit hook that intercepts any unhandled by your script exceptions
    # and enables post-mortem mode.
    #
    # == Local post-mortem mode
    #
    # If you know that a particular block of code raises an exception you can
    # enable post-mortem mode by wrapping this block with Debugger.post_mortem, e.g.
    #
    #   def offender
    #      raise 'error'
    #   end
    #   Debugger.post_mortem do
    #      ...
    #      offender
    #      ...
    #   end
    def post_mortem
      raise "Post-mortem is already activated" if self.post_mortem?
      self.post_mortem = true
      if block_given?
        begin
          yield
        rescue Exception => exp
          handle_post_mortem(exp)
          raise
        ensure
          self.post_mortem = false
        end
      else
        debug_at_exit do
          handle_post_mortem($!) if $! && post_mortem?
        end
      end
    end
    
    def handle_post_mortem(exp)
      return if exp.__debug_context.stack_size == 0
      Debugger.suspend
      orig_tracing = Debugger.tracing, Debugger.current_context.tracing
      Debugger.tracing = Debugger.current_context.tracing = false
      processor.at_line(exp.__debug_context, exp.__debug_file, exp.__debug_line)
    ensure
      Debugger.tracing, Debugger.current_context.tracing = orig_tracing
      Debugger.resume
    end
    private :handle_post_mortem
  end
end

class Exception # :nodoc:
  attr_reader :__debug_file, :__debug_line, :__debug_binding, :__debug_context
end

module Kernel
  #
  # Stops the current thread after a number of _steps_ made.
  #
  def debugger(steps = 1)
    Debugger.current_context.stop_next = steps
  end
  
  #
  # Returns a binding of n-th call frame
  #
  def binding_n(n = 0)
    Debugger.current_context.frame_binding[n+1]
  end
end

class Module
  #
  # Wraps the +meth+ method with Debugger.start {...} block.
  #
  def debug_method(meth)
    old_meth = "__debugee_#{meth}"
    old_meth = "#{$1}_set" if old_meth =~ /^(.+)=$/
    alias_method old_meth.to_sym, meth
    class_eval <<-EOD
    def #{meth}(*args, &block)
      Debugger.start do
        debugger 2
        #{old_meth}(*args, &block)
      end
    end
    EOD
  end
  
  #
  # Wraps the +meth+ method with Debugger.post_mortem {...} block.
  #
  def post_mortem_method(meth)
    old_meth = "__postmortem_#{meth}"
    old_meth = "#{$1}_set" if old_meth =~ /^(.+)=$/
    alias_method old_meth.to_sym, meth
    class_eval <<-EOD
    def #{meth}(*args, &block)
      Debugger.start do |dbg|
        dbg.post_mortem do
          #{old_meth}(*args, &block)
        end
      end
    end
    EOD
  end
end
