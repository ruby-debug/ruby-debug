module Debugger
  class ThreadListCommand < Command # :nodoc:
    self.control = true
    include ThreadFunctions

    def regexp
      /^\s*th(?:read)?\s+l(?:ist)?\s*$/
    end

    def execute
      threads = Debugger.contexts.sort_by{|c| c.thnum}.each do |c|
        display_context(c)
      end
    end

    class << self
      def help_command
        'thread'
      end

      def help(cmd)
        %{
          th[read] l[ist]\t\t\tlist all threads
        }
      end
    end
  end

  class ThreadStopCommand < Command # :nodoc:
    self.control = true
    self.need_context = true
    
    include ThreadFunctions

    def regexp
      /^\s*th(?:read)?\s+stop\s*(\S*)\s*$/
    end

    def execute
      c = parse_thread_num("stop", @match[1])
      return unless c 
      c.suspend
      display_context(c)
    end

    class << self
      def help_command
        'thread'
      end

      def help(cmd)
        %{
          th[read] stop <nnn>\t\tstop thread nnn
        }
      end
    end
  end

  class ThreadResumeCommand < Command # :nodoc:
    self.control = true
    self.need_context = true
    
    include ThreadFunctions

    def regexp
      /^\s*th(?:read)?\s+resume\s*(\S*)\s*$/
    end

    def execute
      c = parse_thread_num("resume", @match[1])
      return unless c 
      c.resume
      display_context(c)
    end

    class << self
      def help_command
        'thread'
      end

      def help(cmd)
        %{
          th[read] resume <nnn>\t\tresume thread nnn
        }
      end
    end
  end

  # Thread switch Must come after "Thread resume" because "switch" is
  # optional

  class ThreadSwitchCommand < Command # :nodoc:
    self.control = true
    self.need_context = true
    
    include ThreadFunctions

    def regexp
      /^\s*th(?:read)?\s*(?:sw(?:itch)?)?\s+(\S+)\s*$/
    end

    def execute
      c = parse_thread_num("switch", @match[1])
      return unless c 
      display_context(c)
      c.stop_next = 1
      c.thread.run
      @state.proceed
    end

    class << self
      def help_command
        'thread'
      end

      def help(cmd)
        %{
          th[read] [sw[itch]] <nnn>\tswitch thread context to nnn
        }
      end
    end
  end

  class ThreadCurrentCommand < Command # :nodoc:
    self.need_context = true
    
    include ThreadFunctions

    def regexp
      /^\s*th(?:read)?\s*(?:cur(?:rent)?)?\s*$/
    end

    def execute
      display_context(@state.context)
    end

    class << self
      def help_command
        'thread'
      end

      def help(cmd)
        %{
          th[read] [cur[rent]]\t\tshow current thread
        }
      end
    end
  end
end
