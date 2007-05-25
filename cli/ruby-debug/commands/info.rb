module Debugger
  class InfoCommand < Command # :nodoc:
    include FrameFunctions
    include ParseFunctions
    include ThreadFunctions
    include VarFunctions

    SubcmdStruct=Struct.new(:name, :min, :short_help)
    Subcommands = 
      [
       ['args', 1, "Argument variables of current stack frame"],
       ['breakpoints', 1, "Status of user-settable breakpoints"],
       ['file', 1, "File names and timestamps of files read in"],
       ['line', 2, "Current source position"],
       ['locals', 2, "Local variables of the current stack frame"],
       ['stack', 2, "Backtrace of the stack"],
       ['threads', 1, "IDs of currently known threads"],
       ['variables', 1, "global variables"]
      ].map do |name, min, short_help| 
      SubcmdStruct.new(name, min, short_help)
    end

    def regexp
      /^\s* i(?:nfo)? (?:\s+(.*))?$/ix
    end
    def execute
      if not @match[1]
        print "\"info\" must be followed by the name of an info command:\n"
        print "List of info subcommands:\n\n"
        for subcmd in Subcommands do
          print "info #{subcmd.name} -- #{subcmd.short_help}\n"
        end
      else
        subcmd, args = @match[1].split(/[ \t]+/)
        subcmd.downcase!
        for try_subcmd in Subcommands do
          if (subcmd.size >= try_subcmd.min) and
              (try_subcmd.name[0..subcmd.size-1] == subcmd)
            send("info_#{try_subcmd.name}", args)
            return
          end
        end
        print "Unknown info command #{subcmd}\n"
      end
    end
    
    def info_args(*args)
      args = @state.context.frame_args(@state.frame_pos)
      args.each do |name, value|
        s = "#{name} = #{value.inspect}"
        if s.size > self.class.settings[:width]
          s[self.class.settings[:width]-3 .. -1] = "..."
        end
        print "#{s}\n"
      end
    end
    
    def info_breakpoints(*args)
      unless Debugger.breakpoints.empty?
        print "Breakpoints:\n"
        Debugger.breakpoints.sort_by{|b| b.id }.each do |b|
          if b.expr.nil?
            print "  %d at %s:%s\n", b.id, b.source, b.pos
          else
            print "  %d at %s:%s if %s\n", b.id, b.source, b.pos, b.expr
          end
        end
      else
        print "No breakpoints.\n"
      end
    end
    
    def info_file(*args)
      SCRIPT_LINES__.each do |file, value|
        print "File %s %s\n", file, SCRIPT_TIMESTAMPS__[file]
      end
    end
    
    def info_line(*args)
      print "Line %d of \"%s\"\n",  @state.line, @state.file
    end
    
    def info_locals(*args)
      locals = @state.context.frame_locals(@state.frame_pos)
      locals.keys.sort.each do |name|
        print "%s = %p\n", name, locals[name]
      end
    end
    
    def info_stack(*args)
      if help
        return "Backtrace of the stack\n"
      end
      (0...@state.context.stack_size).each do |idx|
        if idx == @state.frame_pos
          print "--> "
        else
          print "    "
        end
        print_frame(idx)
      end
    end
    
    def info_threads(*args)
      if help
        return "IDs of currently known threads\n"
      end
      threads = Debugger.contexts.sort_by{|c| c.thnum}.each do |c|
        display_context(c)
      end
    end
    
    def info_variables(*args)
      var_list(global_variables)
    end
    
    class << self
      def help_command
        'info'
      end

      def help(cmd)
        s = "
Generic command for showing things about the program being debugged.
-- 
List of info subcommands:
--  
"
        for subcmd in Subcommands do
          s += "info #{subcmd.name} -- #{subcmd.short_help}\n"
        end
        return s
      end
    end
  end
end
