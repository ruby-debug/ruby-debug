module Debugger
  class Command # :nodoc:
    class << self
      def commands
        @commands ||= []
      end
      
      DEF_OPTIONS = {
        :event => true, 
        :control => false, 
        :always_run => false,
        :unknown => false,
        :need_context => false,
      }
      
      def inherited(klass)
        DEF_OPTIONS.each do |o, v|
          klass.options[o] = v if klass.options[o].nil?
        end
        commands << klass
      end

      def load_commands
        dir = File.dirname(__FILE__)
        Dir[File.join(dir, 'commands', '*')].each do |file|
          require file if file =~ /\.rb$/
        end
      end
      
      def method_missing(meth, *args, &block)
        if meth.to_s =~ /^(.+?)=$/
          @options[$1.intern] = args.first
        else
          if @options.has_key?(meth)
            @options[meth]
          else
            super
          end
        end
      end
      
      def options
        @options ||= {}
      end
    end
    
    def initialize(state)
      @state = state
    end

    def match(input)
      @match = regexp.match(input)
    end

    protected

    def print(*args)
      @state.print(*args)
    end

    def confirm(msg)
      @state.confirm(msg) == 'y'
    end

    def debug_eval(str, b = @state.binding)
      unless b
        print "Can't evaluate in the current context.\nUse rdebug with -f option, or set Debugger.keep_frame_info = true.\n"
        throw :debug_error
      end
      begin
        val = eval(str, b)
      rescue StandardError, ScriptError => e
        at = eval("caller(1)", b)
        print "%s:%s\n", at.shift, e.to_s.sub(/\(eval\):1:(in `.*?':)?/, '')
        for i in at
          print "\tfrom %s\n", i
        end
        throw :debug_error
      end
    end

    def debug_silent_eval(str)
      return nil unless @state.binding
      begin
        eval(str, @state.binding)
      rescue StandardError, ScriptError
        nil
      end
    end

    def line_at(file, line)
      Debugger.line_at(file, line)
    end

    def get_context(thnum)
      Debugger.contexts.find{|c| c.thnum == thnum}
    end  
  end
  
  Command.load_commands
end