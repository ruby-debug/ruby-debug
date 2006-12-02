require 'ruby-debug/interface'
require 'ruby-debug/command'

module Debugger
  class CommandProcessor # :nodoc:
    attr_accessor :interface
    attr_reader   :display
    
    def initialize(interface = LocalInterface.new)
      @interface = interface
      @display = []
      @mutex = Mutex.new
      @last_cmd = nil
    end
    
    def interface=(interface)
      @mutex.synchronize do
        @interface.close if @interface
        @interface = interface
      end
    end
    
    def self.protect(mname)
      alias_method "__#{mname}", mname
      module_eval %{
        def #{mname}(*args)
          @mutex.synchronize do
            return unless @interface
            __#{mname}(*args)
          end
        rescue IOError, Errno::EPIPE
          self.interface = nil
        rescue Exception
          print "INTERNAL ERROR!!! #\{$!\}\n" rescue nil
          print $!.backtrace.map{|l| "\t#\{l\}"}.join("\n") rescue nil
        end
      }
    end
    
    def at_breakpoint(context, breakpoint)
      n = Debugger.breakpoints.index(breakpoint) + 1
      print "Breakpoint %d at %s:%s\n", n, breakpoint.source, breakpoint.pos
    end
    protect :at_breakpoint
    
    def at_catchpoint(context, excpt)
      frames = context.frames
      print "Catchpoint at %s:%d: `%s' (%s)\n", frames[0].file, frames[0].line, excpt, excpt.class
      fs = frames.size
      tb = caller(0)[-fs..-1]
      if tb
        for i in tb
          print "\tfrom %s\n", i
        end
      end
    end
    protect :at_catchpoint
    
    def at_tracing(context, file, line)
      print "Tracing(%d):%s:%s %s", context.thnum, file, line, Debugger.line_at(file, line)
    end
    protect :at_tracing

    def at_line(context, file, line, binding)
      print "%s:%d: %s", file, line, Debugger.line_at(file, line)
      process_commands(context, file, line, binding)
    end
    protect :at_line

    private
    
    def print(*args)
      @interface.print(*args)
    end
    
    def process_commands(context, file, line, binding)
      event_cmds = Command.commands.select{|cmd| cmd.event }
      state = State.new do |s|
        s.context = context
        s.file    = file
        s.line    = line
        s.binding = binding
        s.display = display
        s.interface = interface
        s.commands = event_cmds
      end
      commands = event_cmds.map{|cmd| cmd.new(state) }
      commands.select{|cmd| cmd.class.always_run }.each{|cmd| cmd.execute }
      
      while !state.proceed? and input = @interface.read_command("(rdb:%d) " % context.thnum)
        catch(:debug_error) do
          if input == ""
            next unless @last_cmd
            input = @last_cmd
          else
            @last_cmd = input
          end
          
          if cmd = commands.find{ |c| c.match(input) }
            cmd.execute
          else
            unknown_cmd = commands.find{|cmd| cmd.class.unknown }
            if unknown_cmd
              unknown_cmd.execute
            else
              print "Unknown command\n"
            end
          end
        end
      end
    end

    class State # :nodoc:
      attr_accessor :context, :file, :line, :binding
      attr_accessor :frame_pos, :previous_line, :display
      attr_accessor :interface, :commands

      def initialize
        @frame_pos = 0
        @previous_line = nil
        @proceed = false
        yield self
      end

      def print(*args)
        @interface.print(*args)
      end

      def confirm(*args)
        @interface.confirm(*args)
      end

      def proceed?
        @proceed
      end

      def proceed
        @proceed = true
      end
    end
  end
  
  class ControlCommandProcessor # :nodoc:
    def initialize(interface)
      @interface = interface
    end
    
    def print(*args)
      @interface.print(*args)
    end
    
    def process_commands
      control_cmds = Command.commands.select{|cmd| cmd.control }
      state = State.new(@interface, control_cmds)
      commands = control_cmds.map{|cmd| cmd.new(state) }
      
      while input = @interface.read_command("(rdb:ctrl) ")
        catch(:debug_error) do
          if cmd = commands.find{|c| c.match(input) }
            cmd.execute
          else
            print "Unknown command\n"
          end
        end
      end
    rescue IOError, Errno::EPIPE
    rescue Exception
      print "INTERNAL ERROR!!! #{$!}\n" rescue nil
      print $!.backtrace.map{|l| "\t#{l}"}.join("\n") rescue nil
    ensure
      @interface.close
    end
    
    class State # :nodoc:
      attr_reader :commands
      def initialize(interface, commands)
        @interface = interface
        @commands = commands
      end
      
      def proceed
      end
      
      def print(*args)
        @interface.print(*args)
      end
      
      def confirm(*args)
        'y'
      end
      
      def context
        nil
      end

      def file
        print "No filename given.\n"
        throw :debug_error
      end
    end
  end
end