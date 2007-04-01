module Debugger  
  class LocalInterface # :nodoc:
    def read_command(prompt)
      readline(prompt, true)
    end
    
    def confirm(prompt)
      readline(prompt, false)
    end
    
    def print(*args)
      STDOUT.printf(*args)
    end
    
    def close
    end
    
    private
    
    begin
      require 'readline'
      class << Debugger
        FILE_HISTORY = ".rdebug_hist"
        save_file = File.join(ENV["HOME"]||ENV["HOMEPATH"], FILE_HISTORY)
        open(save_file, 'r') do |file|
          file.each do |line|
            line.chomp!
            Readline::HISTORY << line
          end
        end if File.exists?(save_file)
        
        define_method(:save_history) do
          open(save_file, 'w') do |file|
            Readline::HISTORY.to_a.last(500).each do |line|
              file.puts line unless line.strip.empty?
            end
          end rescue nil
        end
        public :save_history 
      end
      Debugger.debug_at_exit { Debugger.save_history }
      
      def readline(prompt, hist)
        Readline::readline(prompt, hist)
      end
    rescue LoadError
      def readline(prompt, hist)
        STDOUT.print prompt
        STDOUT.flush
        line = STDIN.gets
        exit unless line
        line.chomp!
        line
      end
    end
  end

  class RemoteInterface # :nodoc:
    def initialize(socket)
      @socket = socket
    end
    
    def read_command(prompt)
      send_command "PROMPT #{prompt}"
    end
    
    def confirm(prompt)
      send_command "CONFIRM #{prompt}"
    end

    def print(*args)
      @socket.printf(*args)
    end
    
    def close
      @socket.close
    rescue Exception
    end
    
    private
    
    def send_command(msg)
      @socket.puts msg
      result = @socket.gets
      raise IOError unless result
      result.chomp
    end
  end
  
  class ScriptInterface # :nodoc:
    def initialize(file, out)
      @file = file.respond_to?(:gets) ? file : open(file)
      @out = out
    end
    
    def read_command(prompt)
      while result = @file.gets
        next if result =~ /^\s*#/
        next if result.strip.empty?
        break
      end
      raise IOError unless result
      result
    end
    
    def confirm(prompt)
      'y'
    end
    
    def print(*args)
      @out.print(*args)
    end
    
    def close
      @file.close
    end
  end
end
