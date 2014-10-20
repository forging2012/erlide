package org.erlide.engine.model.erlang;

import java.util.List;

import org.eclipse.handly.junit.WorkspaceTest;
import org.erlide.engine.model.root.ErlElementKind;
import org.erlide.engine.model.root.IErlElement;
import org.junit.Before;

public class ErlModelTestBase2 extends WorkspaceTest {

    protected IErlModule module;
    protected IErlModule module2;
    protected IErlFunction functionA;
    protected IErlFunction functionB;
    protected IErlFunction functionC;

    @Before
    public void set_up() throws Exception {
        setUpProject("testproject1");

        module = getErlModule("xx.erl");
        module2 = getErlModule("zz.erl");
        module2.open(null);
        final List<IErlElement> functions = module2
                .getChildrenOfKind(ErlElementKind.FUNCTION);
        functionA = (IErlFunction) functions.get(0);
        functionB = (IErlFunction) functions.get(1);
        functionC = (IErlFunction) functions.get(2);
    }

}
