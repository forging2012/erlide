package org.erlide.engine.model.erlang;

import static org.junit.Assert.assertEquals;

import org.erlide.engine.model.root.IErlProject;
import org.erlide.testing.utils.WorkspaceTest;
import org.junit.Before;
import org.junit.Test;

public class ErlFunctionCommentTests extends WorkspaceTest {

    private IErlProject project;

    @Before
    public void setup() throws Exception {
        setUpProject("testproject1");
        project = getErlProject("testproject1");
    }

    /**
     * http://www.assembla.com/spaces/erlide/tickets/891-wrong-function-comment-
     * in-edoc-view-and-hover
     */
    @Test
    public void functionCommentsOnlyTopLevel() throws Exception {
        final String s = "f1()->\n    %some comment here \n    foo:bar().\n"
                + "f2() ->\n    ok.";
        final IErlModule myModule = createModule(project, "w.erl", s);
        myModule.open(null);
        final IErlFunction function = (IErlFunction) myModule.getChildNamed("f2");
        assertEquals(0, function.getComments().size());
    }
}
