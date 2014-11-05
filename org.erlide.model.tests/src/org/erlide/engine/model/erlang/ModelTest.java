package org.erlide.engine.model.erlang;

import static org.junit.Assert.assertNotNull;

import org.erlide.engine.model.root.IErlProject;
import org.erlide.testing.utils.WorkspaceTest;
import org.junit.Test;

public class ModelTest extends WorkspaceTest {

    @Test
    public void moduleWithNonErlangExtension() throws Exception {
        // given an erlang project
        final String projectName = "testproject";
        final IErlProject erlProject = createTmpErlProject(projectName);
        assertNotNull(erlProject);
        // when creating a module with non-erlang extension, e.g. erlx
        final IErlModule a = createModule(erlProject, "a.erlx",
                        "-module(a).\n-export([t/0]).\nt() ->\n    p(a).\np(L) ->\n    lists:reverse(L).\n");
        // then it should be created
        assertNotNull(a);
    }
}
