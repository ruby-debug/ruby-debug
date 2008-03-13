module Debugger
  class FinishCommand < Command # :nodoc:
    self.allow_in_post_mortem = false
    self.need_context         = true
    
    def regexp
      /^\s*fin(?:ish)? (?:\s+(.*))?$/x
    end

    def execute
      max_frame = @state.context.stack_size
      frame_pos = get_int(@match[1], "Finish", 0, max_frame-1, 0)
      return nil unless frame_pos
      @state.context.stop_frame = frame_pos
      @state.frame_pos = 0
      @state.proceed
    end

    class << self
      def help_command
        'finish'
      end

      def help(cmd)
        %{
          fin[ish] [frame-number]\tExecute until selected stack frame returns.
If no frame number is given, we run until most-recent frame returns. This is the same as
value 1. If a frame number is given we run until frames returns.
        }
      end
    end
  end
end
