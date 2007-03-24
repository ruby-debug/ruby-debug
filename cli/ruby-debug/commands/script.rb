module Debugger
  class ScriptCommand < Command # :nodoc:
    self.control = true
    
    def regexp
      /^\s*sc(?:ript)?\s+(.+)$/
    end
    
    def execute
      unless File.exists?(@match[1])
        print "Script file '#{@match[1]}' is not found\n"
        return
      end
      Debugger.run_script(@match[1], @state)
    end
    
    class << self
      def help_command
        'script'
      end
      
      def help(cmd)
        %{
          script FILE\texecutes a script file
        }
      end
    end
  end
  
  class SaveCommand < Command # :nodoc:
    self.control = true
    
    def regexp
      /^\s*sa(?:ve)?\s+(.+)$/
    end
    
    def execute
      open(@match[1], 'w') do |file|
        Debugger.breakpoints.each do |b|
          file.puts "break #{b.source}:#{b.pos}#{" if #{b.expr}" if b.expr}"
        end
        file.puts "catch #{Debugger.catchpoint}" if Debugger.catchpoint
      end
      print "Saved to '#{@match[1]}'\n"
    end

    class << self
      def help_command
        'save'
      end
      
      def help(cmd)
        %{
          save FILE\tsaves current breakpoints and catchpoint as a script file
        }
      end
    end
  end
end