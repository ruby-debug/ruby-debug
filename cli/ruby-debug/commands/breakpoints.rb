module Debugger
  class AddBreakpoint < Command # :nodoc:
    self.control = true
    
    def regexp
      / ^\s*
        b(?:reak)?
        \s+
        (?:
          (\d+) |
          (.+?)[:.#]([^.:\s]+)
        )
        (?:\s+
          if\s+(.+)
        )?
        $
      /x
    end

    def execute
      if @match[1]
        line, _, _, expr = @match.captures
      else
        _, file, line, expr = @match.captures
      end

      full_file = nil
      if file.nil?
        full_file = @state.file
        file = File.basename(@state.file)
      else
        if line !~ /^\d+$/
          klass = debug_silent_eval(file)
          if klass && !klass.kind_of?(Module)
            print "Unknown class #{file}\n"
            throw :debug_error
          end
          class_name = klass.name if klass
        else
          file = File.expand_path(file) if file.index(File::SEPARATOR) || \
            File::ALT_SEPARATOR && file.index(File::ALT_SEPARATOR)
          full_file = file
        end
      end
      
      if line =~ /^\d+$/
        line = line.to_i
        lines = Debugger.source_for(full_file)
        if not lines 
          print "No source file named %s\n", file
        elsif lines.size < line
          print "No line %d in file \"%s\"\n", line, file
        else
          b = Debugger.add_breakpoint file, line, expr
          print "Breakpoint %d file %s, line %s\n", b.id, file, line.to_s
        end
      else
        method = line.intern.id2name
        b = Debugger.add_breakpoint class_name, method, expr
        print "Breakpoint %d at %s::%s\n", b.id, class_name, method.to_s
      end
    end

    class << self
      def help_command
        'break'
      end

      def help(cmd)
        %{
          b[reak] file:line [if expr]
          b[reak] class(.|#)method [if expr]
          \tset breakpoint to some position, (optionally) if expr == true
        }
      end
    end
  end

  class BreakpointsCommand < Command # :nodoc:
    self.control = true

    def regexp
      /^\s*b(?:reak)?$/
    end

    def execute
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

    class << self
      def help_command
        'break'
      end

      def help(cmd)
        %{
          b[reak]\tlist breakpoints
        }
      end
    end
  end

  class DeleteBreakpointCommand < Command # :nodoc:
    include ParseFunctions
    self.control = true

    def regexp
      /^\s*del(?:ete)?(?:\s+(.*))?$/
    end

    def execute
      brkpts = @match[1]
      unless brkpts
        if confirm("Delete all breakpoints? (y or n) ")
          Debugger.breakpoints.clear
        end
      else
        brkpts.split(/[ \t]+/).each do |pos|
          pos = get_int(pos, "Delete", 1)
          return unless pos
          unless Debugger.remove_breakpoint(pos)
            print "No breakpoint number %d\n", pos
          end
        end
      end
    end

    class << self
      def help_command
        'delete'
      end

      def help(cmd)
        %{
          del[ete][ nnn...]\tdelete some or all breakpoints
        }
      end
    end
  end
end
