module Debugger
  class WhereCommand < Command # :nodoc:
    include FrameFunctions
    include ParseFunctions

    def regexp
      /^\s*(?:w(?:here)?|bt|backtrace)$/
    end

    def execute
      (0...@state.context.stack_size).each do |idx|
        if idx == @state.frame_pos
          print "--> "
        else
          print "    "
        end
        print_frame(idx)
      end
    end

    class << self
      def help_command
        %w|where backtrace|
      end

      def help(cmd)
        if cmd == 'where'
          %{
            w[here]\tdisplay frames
          }
        else
          %{
            bt|backtrace\t\talias for where
          }
        end
      end
    end
  end

  class UpCommand < Command # :nodoc:
    include FrameFunctions
    include ParseFunctions

    def regexp
      /^\s* u(?:p)? (?:\s+(.*))?$/x
    end

    def execute
      pos = get_int(@match[1], "Up")
      return unless pos
      adjust_frame(pos, false)
    end

    class << self
      def help_command
        'up'
      end

      def help(cmd)
        %{
          up[count]\tmove to higher frame
        }
      end
    end
  end

  class DownCommand < Command # :nodoc:
    include FrameFunctions
    include ParseFunctions

    def regexp
      /^\s* down (?:\s+(.*))? .*$/x
    end

    def execute
      pos = get_int(@match[1], "Down")
      return unless pos
      adjust_frame(-pos, false)
    end

    class << self
      def help_command
        'down'
      end

      def help(cmd)
        %{
          down[count]\tmove to lower frame
        }
      end
    end
  end
  
  class FrameCommand < Command # :nodoc:
    include FrameFunctions
    include ParseFunctions
    def regexp
      /^\s* f(?:rame)? (?:\s+ (.*))? \s*$/x
    end

    def execute
      if not @match[1]
        print "Missing a frame number argument.\n"
        return
      else
        pos = get_int(@match[1], "Frame")
        return unless pos
      end
      adjust_frame(pos, true)
    end

    class << self
      def help_command
        'frame'
      end

      def help(cmd)
        %{
          f[rame] frame-number
          Move the current frame to the specified frame number.

          A negative number indicates position from the other end.  So
          'frame -1' moves to the oldest frame, and 'frame 0' moves to
          the newest frame.
        }
      end
    end
  end
end
