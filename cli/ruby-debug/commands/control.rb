module Debugger
  class QuitCommand < Command # :nodoc:
    self.control = true

    def regexp
      /^\s*(?:q(?:uit)?|exit)\s*$/
    end

    def execute
      if confirm("Really quit? (y/n) ")
        Debugger.save_history if Debugger.respond_to? :save_history
        exit! # exit -> exit!: No graceful way to stop threads...
      end
    end

    class << self
      def help_command
        %w[quit exit]
      end

      def help(cmd)
        %{
          q[uit]\texit from debugger, 
          exit\talias to quit
        }
      end
    end
  end
  
  class RestartCommand < Command # :nodoc:
    self.control = true

    def regexp
      / ^\s*
      (?:restart|R)
      (\s+ \S+ .*)?
      $
      /ix
    end
    
    def execute
      if not defined? Debugger::RDEBUG_SCRIPT
        # FIXME? Should ask for confirmation? 
        print "Debugger was not called from the outset...\n"
        if not File.exists?($0)
          print "  and $0 #{$0} doesn't exist\n"
          return
        else
          print "  we'll hope $0 hasn't been modified.\n"
        end
        if not File.executable?($0)
          print "$0 doesn't seem to be executable; we'll add a call to Ruby.\n"
          prog_script = "ruby #{$0}"
        else
          prog_script = $0
        end
      else
        prog_script = Debugger::PROG_SCRIPT
      end
      if @match[1]
        args = prog_script + " " + @match[1]
      else
        if not defined? Debugger::ARGV
          # FIXME? Should ask for confirmation? 
          print "We'll also hope ARGV hasn't been modified too badly\n"
          argv = ARGV
        elsif Debugger::ARGV[0] == Debugger::PROG_SCRIPT
          argv = Debugger::ARGV[1..-1]
        else
          argv = Debugger::ARGV
        end
        args = argv.join(" ")
      end

      # An execv would be preferable to the "exec" below.
      cmd = prog_script + " " + args
      print "Re exec'ing:\n\t#{cmd}\n"
      exec cmd
    rescue Errno::EOPNOTSUPP
      print "Restart command is not available at this time.\n"
    end

    class << self
      def help_command
        'restart'
      end

      def help(cmd)
        %{
          restart|R [args] 
          Restart the program. This is is a re-exec - all debugger state
          is lost. If command arguments are passed those are used.
        }
      end
    end
  end

  class InterruptCommand < Command # :nodoc:
    self.event = false
    self.control = true
    self.need_context = true
    
    def regexp
      /^\s*i(?:nterrupt)?\s*$/
    end
    
    def execute
      unless Debugger.interrupt_last
        context = Debugger.thread_context(Thread.main)
        context.interrupt
      end
    end
    
    class << self
      def help_command
        'interrupt'
      end
      
      def help(cmd)
        %{
          i[nterrupt]\tinterrupt the program
        }
      end
    end
  end
end
