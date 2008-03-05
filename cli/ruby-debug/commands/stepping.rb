module Debugger
  # Mix-in module to assist in command parsing.
  module SteppingFunctions # :nodoc:
    def parse_stepping_args(command_name, match)
      if match[1].nil? 
          force = Command.settings[:force_stepping]
      elsif match[1] == '+' 
        force = true
      elsif match[1] == '-' 
        force = false
      end
      steps = get_int(match[2], command_name, 1)
      return [steps, force]
    end
  end
  class NextCommand < Command # :nodoc:
    self.allow_in_post_mortem  = false
    self.need_context          = true
    
    def regexp
      /^\s* n(?:ext)? 
        ([+-])?(?:\s+(\S+))? 
        \s*$/x
    end

    def execute
      steps, force = parse_stepping_args("Next", @match)
      return unless steps
      @state.context.step_over steps, @state.frame_pos, force
      @state.proceed
    end

    class << self
      def help_command
        'next'
      end

      def help(cmd)
        %{
          n[ext][+-]?[ nnn]\tstep over once or nnn times, 
          \t\t'+' forces to move to another line.
          \t\t'-' is the opposite of '+' and disables the force_stepping setting.
        }
      end
    end
  end

  class StepCommand < Command # :nodoc:
    self.allow_in_post_mortem = false
    self.need_context         = true
    
    def regexp
      /^\s* s(?:tep)?
        ([+-])?(?:\s+(\S+))?
        \s*$/x
    end

    def execute
      steps, force = parse_stepping_args("Step", @match)
      @state.context.step(steps, force)
      @state.proceed
    end

    class << self
      def help_command
        'step'
      end

      def help(cmd)
        %{
          s[tep][+-]?[ nnn]\tstep (into methods) once or nnn times
          \t\t'+' forces to move to another line.
          \t\t'-' is the opposite of '+' and disables the force_stepping setting.
        }
      end
    end
  end

  class FinishCommand < Command # :nodoc:
    self.allow_in_post_mortem = false
    self.need_context         = true
    
    def regexp
      /^\s*fin(?:ish)?$/
    end

    def execute
      if @state.frame_pos == @state.context.stack_size - 1
        print "\"finish\" not meaningful in the outermost frame.\n"
      else
        @state.context.stop_frame = @state.frame_pos
        @state.frame_pos = 0
        @state.proceed
      end
    end

    class << self
      def help_command
        'finish'
      end

      def help(cmd)
        %{
          fin[ish]\treturn to outer frame
        }
      end
    end
  end

  class ContinueCommand < Command # :nodoc:
    self.allow_in_post_mortem = false
    self.need_context         = true
    def regexp
      /^\s*c(?:ont(?:inue)?)?(?:\s+(.*))?$/
    end

    def execute
      if @match[1] && !@state.context.dead?
        file = File.expand_path(@state.file)
        line = get_int(@match[1], "Continue", 0, nil, 0)
        return unless line
        @state.context.set_breakpoint(file, line)
      end
      @state.proceed
    end

    class << self
      def help_command
        'continue'
      end

      def help(cmd)
        %{
          c[ont[inue]][ nnn]\trun until program ends or hits breakpoint or reaches line nnn 
        }
      end
    end
  end
end
