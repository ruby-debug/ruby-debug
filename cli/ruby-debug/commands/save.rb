module Debugger
  module SaveFunctions # :nodoc:

    # Create a temporary file to write in if file is nil
    def open_save
      require "tempfile"
      file = Tempfile.new("rdebug-save")
      # We want close to not unlink, so redefine.
      def file.close
        @tmpfile.close if @tmpfile
      end
      return file
    end
  end

  class SaveCommand < Command # :nodoc:
    self.control = true
    
    def save_breakpoints(file)
      Debugger.breakpoints.each do |b|
        file.puts "break #{b.source}:#{b.pos}#{" if #{b.expr}" if b.expr}"
      end
      file.puts "catch #{Debugger.catchpoint}" if Debugger.catchpoint
    end
    
    def save_displays(file)
      for d in @state.display
        if d[0]
          file.puts "display #{d[1]}"
        end
      end
    end
    
    def save_settings(file)
      %w(autolist autoeval autoirb basename debuggertesting full_path 
         reload_source_on_change 
         stack_trace_on_error).each do |setting|
        file.puts "set #{setting} = #{Command.settings[setting.to_sym]}"
      end
    end
    
    def regexp
      /^\s*sa(?:ve)?(?:\s+(.+))?$/
    end
    
    def execute
      if not @match[1] or @match[1].strip.empty?
        file = open_save()
      else
        file = open(@match[1], 'w')
      end
      save_breakpoints(file)
      save_displays(file)
      save_settings(file)
      print "Saved to '#{file.path}'\n"
      if @state and @state.interface
        @state.interface.restart_file = file.path
      end
      file.close
    end

    class << self
      def help_command
        'save'
      end
      
      def help(cmd)
        %{
          save FILE\tsaves current debugger state to FILE as a script file.
          This includes breakpoints, catchpoint, display expressions and some
           settings.
        }
      end
    end
  end
end
