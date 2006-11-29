require 'pp'
require 'stringio'
require 'thread'
require 'ruby_debug.so'
require 'ruby-debug/printers/plain_printer'
require 'ruby-debug/printers/xml_printer'
require 'ruby-debug/processor'

SCRIPT_LINES__ = {} unless defined? SCRIPT_LINES__

module Debugger
  PORT = 8989

  @printer_class = PlainPrinter
  @processor = CommandProcessor.new(LocalInterface.new, @printer_class)
  
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

    def at_line(file, line, binding)
      processor.at_line(self, file, line, binding)
    end
  end

  class << self
    attr_accessor :processor
    
    attr_accessor :printer_class
    
    attr_accessor :is_debug
    
    def use_plain_printer
      @printer_class = PlainPrinter
      @processor.printer_class = @printer_class
    end
    
    def use_xml_printer
      @printer_class = XmlPrinter
      @processor.printer_class = @printer_class
    end
    
    # stop main thread when remote connection established
    attr_accessor :stop_on_connect
    
    # in remote mode, wait for the remote connection 
    attr_accessor :wait_connection
    
    attr_reader :thread, :control_thread
    
    #
    # Interrupts the main thread
    #
    def interrupt
      context = contexts.find{|c| c.thread == Thread.current }
      context.interrupt
    end
    
    #
    # Interrupts the last debugged thread
    #
    def interrupt_last
      skip do
        if context = last_context
          return nil unless context.thread.alive?
          context.interrupt
        end
        context
      end
    end
    
    def interface=(value) # :nodoc:
      processor.interface = value
    end
    
    #
    # Starts a remote debugger.
    #
    def start_remote(host = nil, port = PORT)
      return if @thread
      return if started?

      self.interface = nil
      start

      require "socket"
      
      if port.kind_of?(Array)
        cmd_port, ctrl_port = port
      else
        cmd_port, ctrl_port = port, port + 1
      end

      @control_thread = Thread.start do
        current_context.ignore = true
        server = TCPServer.new(host, ctrl_port)
        while (session = server.accept)
          interface = RemoteInterface.new(session)
          processor = ControlCommandProcessor.new(interface)
          processor.process_commands
        end
      end
      
      mutex = Mutex.new
      proceed = ConditionVariable.new
      
      @thread = Thread.start do
        current_context.ignore = true
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
      
      context = contexts.find{ |c| c.thread == Thread.main }
      context.stop_next = 2
    end
    private :stop_main_thread

    def source_for(file) # :nodoc:
      if source = SCRIPT_LINES__[file]
        return source unless source == true
      end
      if File.exists?(file)
        SCRIPT_LINES__[file] = File.readlines(file)
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
  end
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
    frame = Debugger.current_context.frames[n+1]
    raise "Unknown frame #{n}" unless frame
    frame.binding 
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
end