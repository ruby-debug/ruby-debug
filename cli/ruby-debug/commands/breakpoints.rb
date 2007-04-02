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
        pos, _, _, expr = @match.captures
      else
        _, file, pos, expr = @match.captures
      end
      
      if file.nil?
        file = File.basename(@state.file)
      else
        if pos !~ /^\d+$/
          klass = debug_silent_eval(file)
          if klass && !klass.kind_of?(Module)
            print "Unknown class #{file}\n"
            throw :debug_error
          end
          file = klass.name if klass
        else
          file = File.expand_path(file) if file.index(File::SEPARATOR) || \
            File::ALT_SEPARATOR && file.index(File::ALT_SEPARATOR)
        end
      end
      
      if pos =~ /^\d+$/
        pos = pos.to_i
      else
        pos = pos.intern.id2name
      end
      
      b = Debugger.add_breakpoint file, pos, expr
      print "Set breakpoint %d at %s:%s\n", b.id, file, pos.to_s
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
            print "  %d %s:%s\n", b.id, b.source, b.pos
          else
            print "  %d %s:%s if %s\n", b.id, b.source, b.pos, b.expr
          end
        end
      else
        print "No breakpoints\n"
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
    self.control = true

    def regexp
      /^\s*del(?:ete)?(?:\s+(\d+))?$/
    end

    def execute
      pos = @match[1]
      unless pos
        if confirm("Clear all breakpoints? (y/n) ")
          Debugger.breakpoints.clear
        end
      else
        pos = pos.to_i
        unless Debugger.remove_breakpoint(pos)
          print "Breakpoint %d is not defined\n", pos
        end
      end
    end

    class << self
      def help_command
        'delete'
      end

      def help(cmd)
        %{
          del[ete][ nnn]\tdelete some or all breakpoints
        }
      end
    end
  end
end
