module Debugger
  # Implements debugger "source" command.
  class SourceCommand < Command
    self.allow_in_control = true
    
    def regexp
      /^\s*so(?:urce)?\s+(.+)$/
    end
    
    def execute
      file = File.expand_path(@match[1]).strip
      unless File.exists?(file)
        print "Command file '#{file}' is not found\n"
        return
      end
      if @state and @state.interface
        @state.interface.command_queue += File.open(file).readlines
      else
        Debugger.run_script(file, @state)
      end
    end
    
    class << self
      def help_command
        'source'
      end
      
      def help(cmd)
        %{
          source FILE\texecutes a file containing debugger commands
        }
      end
    end
  end
  
end
