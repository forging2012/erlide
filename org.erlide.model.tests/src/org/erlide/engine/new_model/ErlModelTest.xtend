package org.erlide.engine.new_model

import org.eclipse.handly.junit.WorkspaceTest
import org.junit.Assert
import org.junit.Before
import org.junit.Test
import org.eclipse.handly.model.IHandle

class ErlModelTest extends WorkspaceTest {

	@Before
	def void setup() {
		setUpProject("Test001"); // a predefined project with erlang nature
		setUpProject("SimpleProject"); // another one without erlang nature
	}

	@Test
	def testErlModel() {
		val erlModel = ErlModelCore.getErlModel();
		var erlProjects = erlModel.erlProjects;
		Assert.assertEquals(1, erlProjects.length);
		val erlProject = erlProjects.head;
		Assert.assertEquals("Test001", (erlProject as IHandle).name);

		val erlProject2 = erlModel.getErlProject("Test002")
		Assert.assertFalse((erlProject2 as IHandle).exists)
		setUpProject("Test002") // a second project with erlang nature
		Assert.assertTrue((erlProject2 as IHandle).exists)
		erlProjects = erlModel.getErlProjects()
		Assert.assertEquals(2, erlProjects.length)
		Assert.assertTrue(erlProjects.exists[it==erlProject])
		Assert.assertTrue(erlProjects.exists[it==erlProject2])
	}
}
