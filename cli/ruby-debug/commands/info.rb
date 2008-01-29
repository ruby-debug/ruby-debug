module Debugger
  class InfoCommand < Command # :nodoc:
    self.control = true
    Subcommands = 
      [
       ['args', 1, 'Argument variables of current stack frame'],
       ['breakpoints', 1, 'Status of user-settable breakpoints',
        'Without argument, list info about all breakpoints.  With an
integer argument, list info on that breakpoint.'],
       ['display', 2, 'Expressions to display when program stops'],
       ['files', 5, 'File names and timestamps of files read in'],
       ['file', 4, 'Info about a particular file read in'],
       ['global_variables', 2, 'Global variables'],
       ['instance_variables', 2, 
        'Instance variables of the current stack frame'],
       ['line', 2, 
        'Line number and file name of current position in source file'],
       ['locals', 2, 'Local variables of the current stack frame'],
       ['program', 2, 'Execution status of the program'],
       ['stack', 2, 'Backtrace of the stack'],
       ['threads', 1, 'IDs of currently known threads'],
       ['variables', 1, 
        'Local and instance variables of the current stack frame']
      ].map do |name, min, short_help, long_help| 
      SubcmdStruct.new(name, min, short_help, long_help)
    end unless defined?(Subcommands)

    def regexp
      /^\s* i(?:nfo)? (?:\s+(.*))?$/ix
    end
    
    def execute
      if @match[1].empty?
        errmsg "\"info\" must be followed by the name of an info command:\n"
        print "List of info subcommands:\n\n"
        for subcmd in Subcommands do
          print "info #{subcmd.name} -- #{subcmd.short_help}\n"
        end
      else
        args = @match[1].split(/[ \t]+/)
        subcmd = args.shift
        subcmd.downcase!
        for try_subcmd in Subcommands do
          if (subcmd.size >= try_subcmd.min) and
              (try_subcmd.name[0..subcmd.size-1] == subcmd)
            send("info_#{try_subcmd.name}", *args)
            return
          end
        end
        errmsg "Unknown info command #{subcmd}\n"
      end
    end
    
    def info_args(*args)
      unless @state.context
        print "No frame selected.\n"
        return 
      end
      locals = @state.context.frame_locals(@state.frame_pos)
      args = @state.context.frame_args(@state.frame_pos)
      args.each do |name|
        s = "#{name} = #{locals[name].inspect}"
        if s.size > self.class.settings[:width]
          s[self.class.settings[:width]-3 .. -1] = "..."
        end
        print "#{s}\n"
      end
    end
    
    def info_breakpoints(*args)
      unless @state.context
        print "info breakpoints not available here.\n"
        return 
      end
      unless Debugger.breakpoints.empty?
        brkpts = Debugger.breakpoints.sort_by{|b| b.id}
        unless args.empty?
          a = args.map{|a| a.to_i}
          brkpts = brkpts.select{|b| a.member?(b.id)}
          if brkpts.empty?
            errmsg "No breakpoints found among list given\n"
            return
          end
        end
        print "Num Enb What\n"
        brkpts.each do |b|
          if b.expr.nil?
            print "%3d %s   at %s:%s\n", 
            b.id, (b.enabled? ? 'y' : 'n'), b.source, b.pos
          else
            print "%3d %s   at %s:%s if %s\n", 
            b.id, (b.enabled? ? 'y' : 'n'), b.source, b.pos, b.expr
          end
        end
      else
        print "No breakpoints.\n"
      end
    end
    
    def info_display(*args)
      unless @state.context
        print "info display not available here.\n"
        return 
      end
      if @state.display.find{|d| d[0]}
        print "Auto-display expressions now in effect:\n"
        print "Num Enb Expression\n"
        n = 1
        for d in @state.display
          print "%3d: %s  %s\n", n, (d[0] ? 'y' : 'n'), d[1] if
            d[0] != nil
          n += 1
        end
      else
        print "There are no auto-display expressions now.\n"
      end
    end
    
    def info_file(*args)
      unless args[0] 
        info_files
        return
      end
      file = args[0]
      param =  args[1]
      if param and not %w(all breakpoints lines mtime path sha1).member?(param)
        errmsg "Invalid parameter #{param}\n"
        return
      end
      param = 'basic' unless param
      
      unless LineCache::cached?(file)
        unless LineCache::cached_script?(file)
          print "File #{file} is not cached\n"
          return
        end
        LineCache::cache(file, Command.settings[:reload_source_on_change])
      end
      
      print "File %s", file
      path = LineCache.path(file)
      if %w(all basic path).member?(param) and path != file
        print " - %s\n", path 
      else
        print "\n"
      end

      if %w(all basic lines).member?(param)
        lines = LineCache.size(file)
        print "\t %d lines\n", lines if lines
      end

      if %w(breakpoints).member?(param)
        breakpoints = LineCache.trace_line_numbers(file)
        if breakpoints
          print "\tbreakpoint line numbers:\n" 
          print columnize(breakpoints, self.class.settings[:width])
        end
      end

      if %w(all mtime).member?(param)
        stat = LineCache.stat(file)
        print "\t%s\n", stat.mtime if stat
      end
      if %w(all sha1).member?(param)
        print "\t%s\n", LineCache.sha1(file)
      end
    end
    
    def info_files(*args)
      files = LineCache::cached_files
      files += SCRIPT_LINES__.keys unless 'stat' == args[0] 
      files.uniq.sort.each do |file|
        stat = LineCache::stat(file)
        path = LineCache::path(file)
        print "File %s", file
        if path and path != file
          print " - %s\n", path 
        else
          print "\n"
        end
        print "\t%s\n", stat.mtime if stat
      end
    end
    
    def info_instance_variables(*args)
      unless @state.context
        print "info instance_variables not available here.\n"
        return 
      end
      obj = debug_eval('self')
      var_list(obj.instance_variables)
    end
    
    def info_line(*args)
      unless @state.context
        errmsg "info line not available here.\n"
        return 
      end
      print "Line %d of \"%s\"\n",  @state.line, @state.file
    end
    
    def info_locals(*args)
      unless @state.context
        errmsg "info line not available here.\n"
        return 
      end
      locals = @state.context.frame_locals(@state.frame_pos)
      locals.keys.sort.each do |name|
        ### FIXME: make a common routine
        begin
          s = "#{name} = #{locals[name].inspect}"
        rescue
          begin
          s = "#{name} = #{locals[name].to_s}"
          rescue
            s = "*Error in evaluation*"
          end
        end  
        if s.size > self.class.settings[:width]
          s[self.class.settings[:width]-3 .. -1] = "..."
        end
        print "#{s}\n"
      end
    end
    
    def info_program(*args)
      if not @state.context or @state.context.dead? 
        print "The program being debugged is not being run.\n"
        return
      end
      print "Program stopped. "
      case @state.context.stop_reason
      when :step
        print "It stopped after stepping, next'ing or initial start.\n"
      when :breakpoint
        print("It stopped at a breakpoint.\n")
      when :catchpoint
        print("It stopped at a catchpoint.\n")
      when :catchpoint
        print("It stopped at a catchpoint.\n")
      else
        print "unknown reason: %s\n" % @state.context.stop_reason.to_s
      end
    end
    
    def info_stack(*args)
      if not @state.context
        errmsg "info stack not available here.\n"
        return
      end
      (0...@state.context.stack_size).each do |idx|
        if idx == @state.frame_pos
          print "--> "
        else
          print "    "
        end
        print_frame(idx)
      end
    end
    
    def info_threads(*args)
      if not @state.context
        print "info threads not available here.\n"
        return
      end
      threads = Debugger.contexts.sort_by{|c| c.thnum}.each do |c|
        display_context(c)
      end
    end
    
    def info_global_variables(*args)
      unless @state.context
        errmsg "info global_variables not available here.\n"
        return 
      end
      var_list(global_variables)
    end
    
    def info_variables(*args)
      if not @state.context
        errmsg "info variables not available here.\n"
        return
      end
      obj = debug_eval('self')
      locals = @state.context.frame_locals(@state.frame_pos)
      locals['self'] = @state.context.frame_self(@state.frame_pos)
      locals.keys.sort.each do |name|
        next if name =~ /^__dbg_/ # skip debugger pollution
        ### FIXME: make a common routine
        begin
          s = "#{name} = #{locals[name].inspect}"
        rescue
          begin
            s = "#{name} = #{locals[name].to_s}"
          rescue
            s = "#{name} = *Error in evaluation*"
          end
        end
        if s.size > self.class.settings[:width]
          s[self.class.settings[:width]-3 .. -1] = "..."
        end
        s.gsub!('%', '%%')  # protect against printf format strings
        print "#{s}\n"
      end
      var_list(obj.instance_variables, obj.instance_eval{binding()})
      var_class_self
    end
    
    class << self
      def help_command
        'info'
      end

      def help(args)
        if args[1] 
          s = args[1]
          subcmd = Subcommands.find do |try_subcmd| 
            (s.size >= try_subcmd.min) and
              (try_subcmd.name[0..s.size-1] == s)
          end
          if subcmd
            str = subcmd.short_help + '.'
            str += "\n" + subcmd.long_help if subcmd.long_help
            return str
          else
            return "Invalid 'info' subcommand '#{args[1]}'."
          end
        end
        s = %{
          Generic command for showing things about the program being debugged.
          -- 
          List of info subcommands:
          --  
        }
        for subcmd in Subcommands do
          s += "info #{subcmd.name} -- #{subcmd.short_help}\n"
        end
        return s
      end
    end
  end
end
