require 'pp'
require 'stringio'
require 'socket'
require 'thread'
require 'ruby-debug-base'
require 'ruby-debug/processor'

module Debugger
  self.handler = CommandProcessor.new
  
  # the port number used for remote debugging
  PORT = 8989 unless defined?(PORT)

  # File we look for debugger startup commands.
  INITFILE = ".rdebugrc"  unless defined?(INITFILE)
  
  class << self
    # in remote mode, wait for the remote connection 
    attr_accessor :wait_connection
    attr_accessor :annotate
    
    attr_reader :thread, :control_thread

    def interface=(value) # :nodoc:
      handler.interface = value
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
      
      yield if block_given?
      
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
          end
        end
      end
      if wait_connection
        mutex.synchronize do
          proceed.wait(mutex)
        end 
      end
    end
    alias start_server start_remote
    
    def start_control(host = nil, ctrl_port = PORT + 1) # :nodoc:
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
    
    # Runs normal debugger initialization scripts
    # Reads and executes the commands from init file (if any) in the
    # current working directory.  This is only done if the current
    # directory is different from your home directory.  Thus, you can
    # have more than one init file, one generic in your home directory,
    #  and another, specific to the program you are debugging, in the
    # directory where you invoke ruby-debug.
    def run_init_script(out = handler.interface)
      script_file = "#{ENV["HOME"] || ENV["HOMEPATH"]}/#{INITFILE}"
      home_script_file = File.expand_path(script_file)
      cwd_script_file  = File.expand_path("./#{INITFILE}")
      if File.exists?(cwd_script_file)
        run_script(cwd_script_file, out)
      elsif File.exists?(home_script_file)
        run_script(home_script_file, out)
      end
    end

    #
    # Runs a script file
    #
    def run_script(file, out = handler.interface)
      interface = ScriptInterface.new(File.expand_path(file), out)
      processor = ControlCommandProcessor.new(interface)
      processor.process_commands
    end
  end
end
