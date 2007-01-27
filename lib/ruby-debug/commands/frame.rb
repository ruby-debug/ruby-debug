module Debugger
  class WhereCommand < Command # :nodoc:
    def regexp
      /^\s*(?:w(?:here)?|f(?:rame)?)$/
    end

    def execute
      print_frames(@state.frames, @state.frame_pos)
    end

    class << self
      def help_command
        %w|where frame|
      end

      def help(cmd)
        if cmd == 'where'
          %{
            w[here]\tdisplay frames
          }
        else
          %{
            f[rame]\t\talias for where
          }
        end
      end
    end
  end

  class UpCommand < Command # :nodoc:
    def regexp
      /^\s*(?:(up)(?:\s+(\d+))?|(f)(?:rame)?(?:\s+(\d+)))\s*$/
    end

    def execute
      if @match[1]
        cmd, arg = @match.captures
      else
        cmd, arg = @match.captures[2..-1]
      end
      @state.previous_line = nil
      if cmd == 'f'
        @state.frame_pos = arg.to_i - 1
      else
        @state.frame_pos += (arg ? arg.to_i : 1)
      end
      @state.frame_pos = 0 if @state.frame_pos < 0
      if @state.frame_pos >= @state.frames.size
        @state.frame_pos = @state.frames.size - 1
        print_msg "At toplevel"
      end
      frame = @state.frames[-1 - @state.frame_pos]
      @state.binding, @state.file, @state.line = frame.binding, frame.file, frame.line
      print_current_frame(frame, @state.frame_pos)
    end

    class << self
      def help_command
        %w|up frame|
      end

      def help(cmd)
        if cmd == 'up'
          %{
            up[ nn]\tmove to higher frame
          }
        else
          %{
            f[rame] n\tselect nth frame
          }
        end
      end
    end
  end

  class DownCommand < Command # :nodoc:
    def regexp
      /^\s*down(?:\s+(\d+))?$/
    end

    def execute
      @state.previous_line = nil
      @state.frame_pos -= @match[1] ? @match[1].to_i : 1
      if @state.frame_pos < 0
        @state.frame_pos = 0
        print_msg "At stack bottom"
      end
      frame = @state.frames[-1 - @state.frame_pos]
      @state.binding, @state.file, @state.line = frame.binding, frame.file, frame.line
      print_current_frame(frame, @state.frame_pos)
    end

    class << self
      def help_command
        'down'
      end

      def help(cmd)
        %{
          down[ nn]\tmove to lower frame
        }
      end
    end
  end
end