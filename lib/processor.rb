class Debugger
  class State # :nodoc:
    attr_accessor :context, :file, :line, :binding
    attr_accessor :frame_pos, :previous_line, :display
    attr_accessor :interface, :commands
    
    def initialize(file, line, binding, id=nil)
      super()
      @file    = file
      @line    = line
      @binding = binding
      @id      = id
      @frame_pos = 0
      @previous_line = nil
      @proceed = false
    end
    
#     # FIXME: use delegate? 
#     def errmsg(*args)
#       @interface.errmsg(*args)
#     end
    
#     def print(*args)
#       @interface.print(*args)
#     end
    
#     def confirm(*args)
#       @interface.confirm(*args)
#     end
    
    def proceed?
      @proceed
    end
    
    def proceed
      @proceed = true
    end
  end # State

  class Context
    def one_cmd(commands, state, input)
      file = state.file
      line = state.line
      binding = state.binding
      if cmd = commands.find{ |c| c.match(input) }
        ## puts "++ found command #{input} is #{cmd}"
        cmd.execute
        return
      end
      case input
      when /^\s*tr(?:ace)?(?:\s+(on|off))?(?:\s+(all))?$/
        if defined?( $2 )
          if $1 == 'on'
            set_trace_all true
          else
            set_trace_all false
          end
        elsif defined?( $1 )
          if $1 == 'on'
            set_trace true
          else
            set_trace false
          end
        end
        if trace?
          stdout.print "Trace on.\n"
        else
          stdout.print "Trace off.\n"
        end

      when /^\s*b(?:reak)?\s+(?:(.+):)?([^.:]+)$/
        pos = $2
        if $1
          klass = debug_silent_eval($1, binding)
          file = $1
        end
        if pos =~ /^\d+$/
          pname = pos
          pos = pos.to_i
        else
          pname = pos = pos.intern.id2name
        end
        break_points.push [true, 0, klass || file, pos]
        stdout.printf "Set breakpoint %d at %s:%s\n", break_points.size, klass || file, pname

      when /^\s*b(?:reak)?\s+(.+)[#.]([^.:]+)$/
        pos = $2.intern.id2name
        klass = debug_eval($1, binding)
        break_points.push [true, 0, klass, pos]
        stdout.printf "Set breakpoint %d at %s.%s\n", break_points.size, klass, pos

      when /^\s*wat(?:ch)?\s+(.+)$/
        exp = $1
        break_points.push [true, 1, exp]
        stdout.printf "Set watchpoint %d:%s\n", break_points.size, exp

      when /^\s*b(?:reak)?$/
        if break_points.find{|b| b[1] == 0}
          n = 1
          stdout.print "Breakpoints:\n"
          for b in break_points
            if b[0] and b[1] == 0
              stdout.printf "  %d %s:%s\n", n, b[2], b[3] 
            end
            n += 1
          end
        end
        if break_points.find{|b| b[1] == 1}
          n = 1
          stdout.print "\n"
          stdout.print "Watchpoints:\n"
          for b in break_points
            if b[0] and b[1] == 1
              stdout.printf "  %d %s\n", n, b[2]
            end
            n += 1
          end
        end
        if break_points.size == 0
          stdout.print "No breakpoints\n"
        else
          stdout.print "\n"
        end

      when /^\s*del(?:ete)?(?:\s+(\d+))?$/
        pos = $1
        unless pos
          input = readline("Clear all breakpoints? (y/n) ", false)
          if input == "y"
            for b in break_points
              b[0] = false
            end
          end
        else
          pos = pos.to_i
          if break_points[pos-1]
            break_points[pos-1][0] = false
          else
            stdout.printf "Breakpoint %d is not defined\n", pos
          end
        end

      when /^\s*disp(?:lay)?\s+(.+)$/
        exp = $1
        display.push [true, exp]
        stdout.printf "%d: ", display.size
        display_expression(exp, binding)

      when /^\s*disp(?:lay)?$/
        display_expressions(binding)

      when /^\s*undisp(?:lay)?(?:\s+(\d+))?$/
        pos = $1
        unless pos
          input = readline("Clear all expressions? (y/n) ", false)
          if input == "y"
            for d in display
              d[0] = false
            end
          end
        else
          pos = pos.to_i
          if display[pos-1]
            display[pos-1][0] = false
          else
            stdout.printf "Display expression %d is not defined\n", pos
          end
        end

      when /^\s*c(?:ont)?$/
        @prompt = false

      when /^\s*s(?:tep)?(?:\s+(\d+))?$/
        if $1
          lev = $1.to_i
        else
          lev = 1
        end
        @stop_next = lev
        @prompt = false

      when /^\s*n(?:ext)?(?:\s+(\d+))?$/
        if $1
          lev = $1.to_i
        else
          lev = 1
        end
        @stop_next = lev
        @no_step = @frames.size - @frame_pos
        @prompt = false

      when /^\s*w(?:here)?$/, /^\s*f(?:rame)?$/
        display_frames(@frame_pos)

      when /^\s*l(?:ist)?(?:\s+(.+))?$/
        if not $1
          b = previous_line ? previous_line + 10 : binding_line - 5
          e = b + 9
        elsif $1 == '-'
          b = previous_line ? previous_line - 10 : binding_line - 5
          e = b + 9
        else
          b, e = $1.split(/[-,]/)
          if e
            b = b.to_i
            e = e.to_i
          else
            b = b.to_i - 5
            e = b + 9
          end
        end
        previous_line = b
        display_list(b, e, binding_file, binding_line)

      when /^\s*up(?:\s+(\d+))?$/
        previous_line = nil
        if $1
          lev = $1.to_i
        else
          lev = 1
        end
        @frame_pos += lev
        if @frame_pos >= @frames.size
          @frame_pos = @frames.size - 1
          stdout.print "At toplevel\n"
        end
        binding, binding_file, binding_line = @frames[@frame_pos]
        stdout.print format_frame(@frame_pos)

      when /^\s*down(?:\s+(\d+))?$/
        previous_line = nil
        if $1
          lev = $1.to_i
        else
          lev = 1
        end
        @frame_pos -= lev
        if @frame_pos < 0
          @frame_pos = 0
          stdout.print "At stack bottom\n"
        end
        binding, binding_file, binding_line = @frames[@frame_pos]
        stdout.print format_frame(@frame_pos)

      when /^\s*fin(?:ish)?$/
        if @frame_pos == @frames.size
          stdout.print "\"finish\" not meaningful in the outermost frame.\n"
        else
          @finish_pos = @frames.size - @frame_pos
          @frame_pos = 0
          @prompt = false
        end

      when /^\s*cat(?:ch)?(?:\s+(.+))?$/
        if $1
          excn = $1
          if excn == 'off'
            @catch = nil
            stdout.print "Clear catchpoint.\n"
          else
            @catch = excn
            stdout.printf "Set catchpoint %s.\n", @catch
          end
        else
          if @catch
            stdout.printf "Catchpoint %s.\n", @catch
          else
            stdout.print "No catchpoint.\n"
          end
        end

      when /^\s*v(?:ar)?\s+/
        debug_variable_info($', binding)

      when /^\s*m(?:ethod)?\s+/
        debug_method_info($', binding)

      when /^\s*th(?:read)?\s+/
        if Debugger.debug_thread_info($', binding) == :cont
          @prompt = false
        end

      when /^\s*pp\s+/
        PP.pp(debug_eval($', binding), stdout)

      when /^\s*p\s+/
        stdout.printf "%s\n", debug_eval($', binding).inspect

      when /^\s*r(?:estart)?$/
        $debugger_restart.call

      when /^\s*h(?:elp)?$/
        debug_print_help()

      else
        v = debug_eval(input, binding)
        stdout.printf "%s\n", v.inspect
      end
    end
  end
end
