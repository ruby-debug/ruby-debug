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
      step_count = get_int(match[2], command_name, 1)
      return [step_count, force]
    end
  end
  # Implements debugger "next" command.
  class NextCommand < Command
    self.allow_in_post_mortem  = false
    self.need_context          = true
    
    def regexp
      /^\s* n(?:ext)? 
        ([+-])?(?:\s+(\S+))? 
        \s*$/x
    end

    def execute
      steps, @state.processor.different = parse_stepping_args("Next", @match)
      return unless steps
      @state.processor.next_level = @state.context.stack_size - @state.frame_pos
      @state.context.step_over steps, @state.frame_pos
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

  # Implements debugger "step" command.
  class StepCommand < Command
    self.allow_in_post_mortem = false
    self.need_context         = true
    
    def regexp
      /^\s* s(?:tep)?
        ([+-])?(?:\s+(\S+))?
        \s*$/x
    end

    def execute
      steps, @state.processor.different = parse_stepping_args("Step", @match)
      return unless steps
      @state.processor.next_level = 10000
      @state.context.step(steps-1)
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
end
