package org.erlide.engine.model.erlang;

import org.eclipse.handly.junit.WorkspaceTest;
import org.erlide.engine.model.root.IErlProject;
import org.junit.Before;

public class ErlModelTestBase extends WorkspaceTest {

    protected IErlProject[] projects = new IErlProject[0];
    protected IErlModule module;
    protected IErlProject project;

    @Before
    public void setup() throws Exception {
        setUpProject("testproject1");
        setUpProject("testproject2");

        projects = new IErlProject[] { getErlProject("testproject1"),
                getErlProject("testproject2") };
        project = getErlProject("testproject1");
        module = getErlModule("xx.erl");
    }

}
