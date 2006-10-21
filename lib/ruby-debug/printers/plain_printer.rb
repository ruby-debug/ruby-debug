module Debugger
  class PlainPrinter # :nodoc:
    attr_accessor :interface
    
    def initialize(interface)
      @interface = interface
    end
    
    def print_msg(*args)
      print *args
      print "\n"
    end
    
    def print_frames(frames, cur_idx)
      frames.each_with_index do |frame, idx|
        print_frame(frame, idx, cur_idx)
      end
    end

    def print_frame(frame, idx, cur_idx)
      if idx == cur_idx
        print "--> "
      else
        print "    "
      end
      file, line, id = frame.file, frame.line, frame.id
      print "#%d %s:%s%s\n", idx + 1, file, line, (id ? ":in `#{id.id2name}'" : "")
    end
    
    def print_contexts(contexts)
      contexts.each do |c|
        print_context(c)
      end
    end
    
    def print_context(c)
      c_flag = c.thread == Thread.current ? '+' : ' '
      d_flag = c.ignore? ? '!' : ' '
      print "%s%s", c_flag, d_flag
      print "%d ", c.thnum
      print "%s\t", c.thread.inspect
      last_frame = c.frames.first
      print "%s:%d", last_frame.file, last_frame.line if last_frame
      print "\n"
    end
    
    def print_variables(vars, binding, kind)
      vars.sort.each do |v|
        print_variable(v, binding, kind)
      end
    end

    def print_variable(name, binding, kind)
      print "  %s => %s\n", name, eval(name, binding).inspect
    end

    def print_breakpoints(breakpoints)
      unless breakpoints.empty?
        print "Breakpoints:\n"
        breakpoints.each_with_index do |b, n|
          if b.expr.nil?
            print "  %d %s:%s\n", n+1, b.source, b.pos
          else
            print "  %d %s:%s if %s\n", n+1, b.source, b.pos, b.expr
          end
        end
        print "\n"
      else
        print "No breakpoints\n"
      end
    end
    
    def print_expressions(exps)
      exps.each_with_index do |(exp, value), idx|
        print_expression(exp, value, idx+1)
      end
    end
    
    def print_expression(exp, value, idx)
      print "%d: %s = %s\n", idx, exp, value
    end
    
    def print_eval(exp, value)
      print "%s\n", value
    end
    
    def print_pp(exp, value)
      print value
    end
    
    def print_list(b, e, file, line)
      print "[%d, %d] in %s\n", b, e, file
      if lines = Debugger.source_for(file)
        n = 0
        b.upto(e) do |n|
          if n > 0 && lines[n-1]
            if n == line
              print "=> %d  %s\n", n, lines[n-1].chomp
            else
              print "   %d  %s\n", n, lines[n-1].chomp
            end
          end
        end
      else
        print "No sourcefile available for %s\n", file
      end
    end
    
    def print_methods(methods)
      len = 0
      for v in methods.sort
        len += v.size + 1
        if len > 70
          len = v.size + 1
          print "\n"
        end
        print "%s ", v
      end
      print "\n"
    end
    
    # Events
    
    def print_breakpoint(n, breakpoint)
      print("Breakpoint %d at %s:%s\n", n, breakpoint.source, breakpoint.pos)
    end
    
    def print_catchpoint(excpt)
      frames = Debugger.current_context.frames
      print "Catchpoint at %s:%d: `%s' (%s)\n", frames[0].file, frames[0].line, excpt, excpt.class
      fs = frames.size
      tb = caller(0)[-fs..-1]
      if tb
        for i in tb
          print "\tfrom %s\n", i
        end
      end
    end
    
    def print_trace(context, file, line)
      print "Tracing(%d):%s:%s %s", context.thnum, file, line, Debugger.line_at(file, line)
    end
    
    def print_at_line(file, line)
      print "%s:%d: %s", file, line, Debugger.line_at(file, line)
    end
    
    def print_exception(excpt, binding)
      at = eval("caller(1)", binding)
      print "%s:%s\n", at.shift, excpt.to_s.sub(/\(eval\):1:(in `.*?':)?/, '')
      for i in at
        print "\tfrom %s\n", i
      end
    end

    private
    
    def print(*params)
      @interface.print(*params)
    end

    def print_element(name)
      yield
    end

  end
end