package org.erlide.engine.model.erlang;

import static org.junit.Assert.assertEquals;

import org.erlide.engine.model.root.IErlElement;
import org.erlide.testing.utils.WorkspaceTest;
import org.junit.Before;
import org.junit.Test;

public class ErlMemberTests extends WorkspaceTest {

    private IErlModule module;

    @Before
    public void setup() throws Exception {
        setUpProject("testproject1");
        module = getErlModule("xx.erl");
    }

    // void setNameRange(int offset, int length);
    @Test
    public void setNameRange() throws Exception {
        module.open(null);
        final IErlElement element = module.getElementAt(1);
        final IErlAttribute attribute = (IErlAttribute) element;
        final int offset = 1, length = 2;
        attribute.setNameRange(offset, length);
        final ISourceRange nameRange = attribute.getNameRange();
        assertEquals(offset, nameRange.getOffset());
        assertEquals(length, nameRange.getLength());
    }

    // ISourceRange getNameRange();
    @Test
    public void getNameRange() throws Exception {
        module.open(null);
        final IErlElement element = module.getElementAt(1);
        final IErlAttribute attribute = (IErlAttribute) element;
        final ISourceRange nameRange = attribute.getNameRange();
        assertEquals(0, nameRange.getOffset());
        assertEquals(12, nameRange.getLength());
    }
}
