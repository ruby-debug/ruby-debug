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
    
    def initialize(state, printer)
      @state, @printer = state, printer
    end

    def match(input)
      @match = regexp.match(input)
    end

    protected
    
    def method_missing(meth, *args, &block)
      if @printer.respond_to? meth
        @printer.send meth, *args, &block
      else
        super
      end
    end

    def print(*args)
      @state.print(*args)
    end

    def confirm(msg)
      @state.confirm(msg) == 'y'
    end

    def debug_eval(str, b = @state.binding)
      unless b
        print "Can't evaluate in the current context.\n"
        throw :debug_error
      end
      begin
        val = eval(str, b)
      rescue StandardError, ScriptError => e
        @printer.print_exception(e, @state.binding)
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
    
    def get_binding(pos)
      # returns frame binding of frame pos, if pos is within bound,  @state.binding otherwise
      return @state.binding unless pos
      pos = pos.to_i
      pos -= 1
      if pos >= @state.context.frames.size || pos < 0 then
        @printer.print_error("stack frame number must be between 1 and %i, was: %i, using 1.", @state.context.frames.size, pos+1)
        return @state.binding
      end
      @printer.print_msg("Using frame %s for evaluation of variable.", pos)
      return @state.context.frames[pos].binding
    end
  end
  
  Command.load_commands
end