module Debugger
  module FrameFunctions # :nodoc:
    def adjust_frame(frame_pos, absolute)
      if absolute
        if frame_pos < 0
          abs_frame_pos = @state.context.stack_size + frame_pos
        else
          abs_frame_pos = frame_pos
        end
      else
        abs_frame_pos = @state.frame_pos + frame_pos
      end

      if abs_frame_pos >= @state.context.stack_size then
        print "Adjusting would put us beyond the oldest (initial) frame.\n"
        return
      elsif abs_frame_pos < 0 then
        print "Adjusting would put us beyond the newest (innermost) frame.\n"
        return
      end
      if @state.frame_pos != abs_frame_pos then
        @state.previous_line = nil
        @state.frame_pos = abs_frame_pos
      end
      
      @state.file = @state.context.frame_file(@state.frame_pos)
      @state.line = @state.context.frame_line(@state.frame_pos)
      
      print format_frame(@state.frame_pos)
    end

    def get_int(str, cmd)
      begin
        return Integer(@match[1])
      rescue
        print "%s argument needs to be a number.\n" % cmd
        return nil
      end
    end

    def format_frame(pos)
      printf "\032\032" if ENV['EMACS']
      file, line, id = @state.context.frame_file(pos), @state.context.frame_line(pos), @state.context.frame_id(pos)
      "#%d %s:%s%s\n" % [pos, file, line, (id ? ":in `#{id.id2name}'" : "")]
    end
  end

  class WhereCommand < Command # :nodoc:
    include FrameFunctions

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
        print format_frame(idx)
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

    def regexp
      /^\s* u(?:p)? (?:\s+(.*))? .*$/x
    end

    def execute
      unless @match[1]
        pos = 1
      else
        pos = get_int(@match[1], "Up")
        return unless pos
      end
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

    def regexp
      /^\s* down (?:\s+(.*))? .*$/x
    end

    def execute
      if not @match[1]
        pos = 1
      else
        pos = get_int(@match[1], "Down")
        return unless pos
      end
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
