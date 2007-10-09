

import java.io.IOException;
import org.jruby.Ruby;
import org.jruby.RubyClass;
import org.jruby.RubyThread;
import org.jruby.debug.DebuggerDef;
import org.jruby.runtime.Block;
import org.jruby.runtime.builtin.IRubyObject;
import org.jruby.runtime.load.BasicLibraryService;
import org.jruby.runtime.load.Library;

public final class RubyDebugBaseService implements BasicLibraryService {

    public boolean basicLoad(Ruby runtime) throws IOException {
//        System.out.println("MK> " + new Exception().getStackTrace()[0] + " called...." + ", " + System.currentTimeMillis());
//        java.util.logging.Logger.getLogger("MK>").info(" " + new Exception().getStackTrace()[0] + " called...." + ", " + System.currentTimeMillis());
//        java.util.logging.Logger.getLogger("MK>").info("   HIT_COND_NONE: \"" + HIT_COND_NONE + "\"");
        DebuggerDef.createDebuggerModule(runtime);
        return true;
    }
    
}
