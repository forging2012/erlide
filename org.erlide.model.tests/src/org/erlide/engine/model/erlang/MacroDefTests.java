package org.erlide.engine.model.erlang;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import java.util.List;

import org.erlide.engine.model.root.ErlElementKind;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.testing.utils.WorkspaceTest;
import org.junit.Before;
import org.junit.Test;

public class MacroDefTests extends WorkspaceTest {

    private IErlProject project;

    @Before
    public void setup() throws Exception {
        setUpProject("testproject1");
        project = getErlProject("testproject1");
    }

    @Test
    public void detectMacroName() throws Exception {
        final IErlModule module2 = createModule(project, "yyw.erl", "-module(yyw).\n"
                + "-define(X, x).\n" + "-define(X , x).\n" + "f()->?X.\n");
        module2.open(null);
        final List<IErlElement> childrenOfKind = module2
                .getChildrenOfKind(ErlElementKind.MACRO_DEF);

        IErlMacroDef def = (IErlMacroDef) childrenOfKind.get(0);
        assertThat(def.getDefinedName(), is("X"));

        def = (IErlMacroDef) childrenOfKind.get(1);
        assertThat(def.getDefinedName(), is("X"));
    }

}
