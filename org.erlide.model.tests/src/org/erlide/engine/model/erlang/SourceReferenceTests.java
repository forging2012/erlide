package org.erlide.engine.model.erlang;

import static org.junit.Assert.assertEquals;

import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.testing.utils.WorkspaceTest;
import org.junit.Before;
import org.junit.Test;

public class SourceReferenceTests extends WorkspaceTest {

    private IErlModule module;
    private IErlProject project;

    @Before
    public void setup() throws Exception {
        setUpProject("testproject1");
        project = getErlProject("testproject1");
        module = getErlModule("xx.erl");
    }

    // ISourceRange getSourceRange() throws ErlModelException;
    @Test
    public void getSourceRange() throws Exception {
        module = createModule(project, "sourcerange.erl",
                "-module(sourcerange). -include(\"yy.hrl\"). f(A) ->  lists:reverse(A). ");
        module.open(null);

        final IErlElement element = module.getElementAt(0);
        final ISourceReference sourceReference = (ISourceReference) element;
        final ISourceRange sourceRange = sourceReference.getSourceRange();
        assertEquals(0, sourceRange.getOffset());

        final IErlElement element2 = module.getElementAt(25);
        final ISourceReference sourceReference2 = (ISourceReference) element2;
        final ISourceRange sourceRange2 = sourceReference2.getSourceRange();
        assertEquals(22, sourceRange2.getOffset());
    }

    // public int getLineStart();
    // public int getLineEnd();
    @Test
    public void getLineX() throws Exception {
        module.open(null);

        final IErlElement element = module.getElementAtLine(0);
        final ISourceReference sourceReference = (ISourceReference) element;
        assertEquals(0, sourceReference.getLineStart());
        assertEquals(0, sourceReference.getLineEnd());

        final IErlElement element2 = module.getElementAtLine(1);
        final ISourceReference sourceReference2 = (ISourceReference) element2;
        assertEquals(1, sourceReference2.getLineStart());
        assertEquals(1, sourceReference2.getLineEnd());
    }

}
