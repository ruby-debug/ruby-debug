module Debugger
  class Lock  # :nodoc:
    def initialize
      @locker = nil
      @waiting = []
      @locked = false;
    end

    def locked?
      @locked
    end

    def lock
      return if Thread.critical
      return if @locker == Thread.current
      while (Thread.critical = true; @locked)
        @waiting.push Thread.current
        Thread.stop
      end
      @locked = true
      @locker = Thread.current
      Thread.critical = false
      self
    end

    def unlock
      return if Thread.critical
      return unless @locked
      unless @locker == Thread.current
        raise RuntimeError, "unlocked by other"
      end
      Thread.critical = true
      t = @waiting.shift
      @locked = false
      @locker = nil
      Thread.critical = false
      t.run if t
      self
    end
  end
end