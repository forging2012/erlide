package org.erlide.engine.new_model

import org.eclipse.core.runtime.Path
import org.erlide.testing.utils.WorkspaceTest
import org.junit.Before
import org.junit.Test

import static org.hamcrest.MatcherAssert.*
import static org.hamcrest.Matchers.*

class ErlModelTest extends WorkspaceTest {

    @Before
    def void setup() {
        setUpProject("Test001"); // a predefined project with erlang nature
        setUpProject("SimpleProject"); // another one without erlang nature
    }

    @Test
    def testErlModel() {
        val erlModel = ErlModelCore.getErlModel();
        var erlProjects = erlModel.projects;
        assertThat(erlProjects.length, is(1));
        val erlProject = erlProjects.head;
        assertThat(erlProject.name, is("Test001"));

        val erlProject2 = erlModel.getProject("Test002")
        assertThat(erlProject2.exists, is(false))
        setUpProject("Test002") // a second project with erlang nature
        assertThat(erlProject2.exists, is(true))
        erlProjects = erlModel.getProjects()
        assertThat(erlProjects.length, is(2))
        assertThat(erlProjects.exists[it == erlProject], is(true))
        assertThat(erlProjects.exists[it == erlProject2], is(true))

        val erlFiles = erlProject.modules;
        assertThat(erlFiles.length, is(1));
        val erlFile = erlFiles.head;
        assertThat(erlFile.getName(), is("nop.erl"));

        val erlFiles2 = erlProject2.modules;
        assertThat(erlFiles2.length, is(1));
        val erlFile2 = erlFiles2.head;
        assertThat(erlFile2.getName(), is("nop.erl"));

        erlFile.getFile().delete(true, null);
        assertThat(erlFile.exists(), is(false));
        assertThat(erlFile.getParent().getChildren().length, is(0));

        erlFile2.getFile().move(new Path("/Test001/nop.erl"), true, null);
        assertThat(erlFile2.exists(), is(false));
        assertThat(erlProject2.modules.length, is(0));
        assertThat(erlProject.modules.length, is(1));
    }

}
