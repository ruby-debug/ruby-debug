/*
 * header & license
 * Copyright (c) 2007 Martin Krauskopf
 * 
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 * 
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
package org.jruby.debug;

import java.util.LinkedList;
import java.util.List;

import org.jruby.RubyThread;
import org.jruby.runtime.builtin.IRubyObject;

final class DebugContext {

    static final String AT_BREAKPOINT = "at_breakpoint";
    static final String AT_CATCHPOINT = "at_catchpoint";
    static final String AT_LINE = "at_line";
    static final String AT_TRACING = "at_tracing";
    static final String LIST = "list";

    private static int thnumMax = 0;

    enum StopReason {
        NONE, STEP, BREAKPOINT, CATCHPOINT
    }

    private final RubyThread thread;
    private IRubyObject breakpoint;
    private final List<DebugFrame> frames;
    private int lastLine;
    private String lastFile;
    private int destFrame;
    private int stopFrame;
    private int stopNext;
    private int stopLine;
    private int stackLen;
    private StopReason stopReason;
    private int thnum;
    private boolean dead;

    // flags
    private boolean suspended;
    private boolean wasRunning;
    private boolean ignored;
    private boolean skipped;
    private boolean enableBreakpoint;
    private boolean stepped;
    private boolean tracing;
    private boolean forceMove;

    DebugContext(final RubyThread thread) {
        thnum = ++thnumMax;
        lastFile = null;
        lastLine = 0;
        stopNext = -1;
        destFrame = -1;
        stopLine = -1;
        stopFrame = -1;
        stopReason = StopReason.NONE;
        frames = new LinkedList<DebugFrame>();
        breakpoint = thread.getRuntime().getNil();
        this.thread = thread;
    }
    
    void addFrame(final DebugFrame debugFrame) {
        frames.add(debugFrame);
    }

    RubyThread getThread() {
        return thread;
    }

    DebugFrame getTopFrame() {
        return frames.get(getStackSize() - 1);
    }
    
    DebugFrame getFrame(int index) {
        return frames.get(getStackSize() - index - 1);
    }

    DebugFrame popFrame() {
        return frames.remove(getStackSize() - 1);
    }

    void clearFrames() {
        frames.clear();
    }
    
    IRubyObject getBreakpoint() {
        return breakpoint;
    }

    void setBreakpoint(IRubyObject breakpoint) {
        this.breakpoint = breakpoint;
    }

    int getDestFrame() {
        return destFrame;
    }

    void setDestFrame(int destFrame) {
        this.destFrame = destFrame;
    }

    boolean isEnableBreakpoint() {
        return enableBreakpoint;
    }

    void setEnableBreakpoint(boolean enableBreakpoint) {
        this.enableBreakpoint = enableBreakpoint;
    }

    boolean isForceMove() {
        return forceMove;
    }

    void setForceMove(boolean forceMove) {
        this.forceMove = forceMove;
    }

    boolean isIgnored() {
        return ignored;
    }

    void setIgnored(boolean ignored) {
        this.ignored = ignored;
    }

    String getLastFile() {
        return lastFile;
    }

    void setLastFile(String lastFile) {
        this.lastFile = lastFile;
    }

    int getLastLine() {
        return lastLine;
    }

    void setLastLine(int lastLine) {
        this.lastLine = lastLine;
    }

    boolean isSkipped() {
        return skipped;
    }

    void setSkipped(boolean skipped) {
        this.skipped = skipped;
    }

    int getStackLen() {
        return stackLen;
    }

    void setStackLen(int stackLen) {
        this.stackLen = stackLen;
    }

    int getStackSize() {
        return frames.size();
    }

    boolean isStepped() {
        return stepped;
    }

    void setStepped(boolean stepped) {
        this.stepped = stepped;
    }

    int getStopFrame() {
        return stopFrame;
    }

    void setStopFrame(int stopFrame) {
        this.stopFrame = stopFrame;
    }

    int getStopLine() {
        return stopLine;
    }

    void setStopLine(int stopLine) {
        this.stopLine = stopLine;
    }

    int getStopNext() {
        return stopNext;
    }

    void setStopNext(int stopNext) {
        this.stopNext = stopNext;
    }

    StopReason getStopReason() {
        return stopReason;
    }

    void setStopReason(StopReason stopReason) {
        this.stopReason = stopReason;
    }

    boolean isSuspended() {
        return suspended;
    }

    void setSuspended(boolean suspended) {
        this.suspended = suspended;
    }

    int getThnum() {
        return thnum;
    }

    void setThnum(int thnum) {
        this.thnum = thnum;
    }

    boolean isTracing() {
        return tracing;
    }

    void setTracing(boolean tracing) {
        this.tracing = tracing;
    }

    boolean isWasRunning() {
        return wasRunning;
    }

    void setWasRunning(boolean wasRunning) {
        this.wasRunning = wasRunning;
    }

    boolean isDead() {
        return dead;
    }

    void setDead(boolean dead) {
        this.dead = dead;
    }

    /* "Step", "Next" and "Finish" do their work by saving information
       about where to stop next. resetSteppingStopPoints removes/resets this
       information. */
    void resetSteppingStopPoints() {
        this.destFrame = -1;
        this.stopLine = -1;
        this.stopNext = -1;
    }
}
