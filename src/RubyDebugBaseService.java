import java.io.IOException;
import org.jruby.Ruby;
import org.jruby.debug.RubyDebugger;
import org.jruby.runtime.load.BasicLibraryService;

public final class RubyDebugBaseService implements BasicLibraryService {

    public boolean basicLoad(Ruby runtime) throws IOException {
        RubyDebugger.createDebuggerModule(runtime);
        return true;
    }
    
}
