module Debugger
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
end
