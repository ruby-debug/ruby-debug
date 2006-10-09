module Debugger
  class QuitCommand < Command # :nodoc:
    self.control = true

    def regexp
      /^\s*q(?:uit)?\s*$/
    end

    def execute
      if confirm("Really quit? (y/n) ")
        Debugger.save_history if Debugger.respond_to? :save_history
        exit! # exit -> exit!: No graceful way to stop threads...
      end
    end

    class << self
      def help_command
        'quit'
      end

      def help(cmd)
        %{
          q[uit]\texit from debugger
        }
      end
    end
  end
  
  class InterruptCommand < Command # :nodoc:
    self.event = false
    self.control = true
    
    def regexp
      /^\s*i(?:nterrupt)?\s*$/
    end
    
    def execute
      unless Debugger.interrupt_last
        context = Debugger.contexts.find{ |c| c.thread == Thread.main }
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