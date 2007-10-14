import java.io.IOException;
import org.jruby.Ruby;
import org.jruby.debug.DebuggerDef;
import org.jruby.runtime.load.BasicLibraryService;

public final class RubyDebugBaseService implements BasicLibraryService {

    public boolean basicLoad(Ruby runtime) throws IOException {
        DebuggerDef.createDebuggerModule(runtime);
        return true;
    }
    
}
