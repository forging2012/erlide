package org.erlide.engine.model.erlang;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.erlide.engine.model.root.IErlElement;
import org.junit.Before;
import org.junit.Test;

public class SourceRangeTests extends ErlModelTestBase {

    private ISourceRange sourceRange;
    private ISourceRange sourceRange2;

    @Before
    public void set_up() throws Exception {
        final IErlModule module2 = createModule(project, "zz6.erl",
                "-module(zz6). f() -> ok.\n");
        module2.open(null);
        final IErlElement element = module2.getElementAt(1);
        final IErlAttribute attribute = (IErlAttribute) element;
        sourceRange = attribute.getSourceRange();
        final IErlElement element2 = module2.getElementAt(18);
        final IErlFunction function = (IErlFunction) element2;
        sourceRange2 = function.getSourceRange();
    }

    // int getLength();
    @Test
    public void getLength() throws Exception {
        assertEquals("-module(zz6).".length(), sourceRange.getLength());
        assertEquals("f() -> ok.".length(), sourceRange2.getLength());
    }

    // int getOffset();
    @Test
    public void getOffset() throws Exception {
        assertEquals(0, sourceRange.getOffset());
        assertEquals("-module(zz6). ".length(), sourceRange2.getOffset());
    }

    // boolean hasPosition(int position);
    @Test
    public void hasPosition() throws Exception {
        assertTrue(sourceRange.hasPosition(0));
        final int offset = sourceRange.getOffset() + sourceRange.getLength() - 1;
        final int offset2 = sourceRange.getOffset() + sourceRange.getLength();
        final int offset3 = sourceRange.getOffset() + sourceRange.getLength() + 1;
        assertTrue(sourceRange.hasPosition(offset));
        assertTrue(sourceRange.hasPosition(offset2));
        assertFalse(sourceRange.hasPosition(offset3));
    }

}
