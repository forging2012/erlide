package org.erlide.engine.new_model

import org.eclipse.core.resources.IProjectDescription
import org.eclipse.core.runtime.Path
import org.eclipse.handly.model.IElementChangeEvent
import org.eclipse.handly.model.IElementChangeListener
import org.eclipse.handly.model.IHandleDelta
import org.eclipse.handly.model.impl.HandleDelta
import org.erlide.testing.utils.WorkspaceTest
import org.junit.After
import org.junit.Before
import org.junit.Test

import static org.hamcrest.MatcherAssert.*
import static org.hamcrest.Matchers.*

class ErlNotificationTest extends WorkspaceTest {
    val ErlModelListener listener = new ErlModelListener()

    @Before
    def void setup() {
        setUpProject("Test001")

        val erlModel = ErlModelCore.erlModel
        erlModel.addElementChangeListener(listener)
    }

    @After
    def void teardown() {
        val erlModel = ErlModelCore.erlModel
        erlModel.removeElementChangeListener(listener)
    }

    @Test
    def void testErlModelNotification() {
        val erlModel = ErlModelCore.erlModel
        val IErlProject erlProject1 = erlModel.getProject("Test001")
        val IErlProject erlProject2 = erlModel.getProject("Test002")

        setUpProject("Test002")
        assertEquality(newDelta().insertAdded(erlProject2), listener.delta)

        val IErlSource erlFile1 = erlProject1.getSourceFile("src/nop.erl")
        erlFile1.getFile().touch(null)
        assertEquality(newDelta().insertChanged(erlFile1, HandleDelta.F_CONTENT), listener.delta)

        erlFile1.getFile().copy(new Path("/Test002/test1.erl"), true, null)
        assertEquality(newDelta().insertAdded(erlProject2.getSourceFile("test1.erl")), listener.delta)

        erlFile1.getFile().delete(true, null)
        assertEquality(newDelta().insertRemoved(erlFile1), listener.delta)

        val IErlSource erlFile2 = erlProject2.getSourceFile("nop.erl")
        val IErlSource movedErlFile2 = erlProject1.getSourceFile("test1.erl")
        erlFile2.getFile().move(new Path("/Test001/test1.erl"), true, null)
        assertEquality(newDelta().insertMovedTo(movedErlFile2, erlFile2).insertMovedFrom(erlFile2, movedErlFile2),
            listener.delta)

        erlProject2.workspaceProject.close(null)
        assertEquality(newDelta().insertRemoved(erlProject2, HandleDelta.F_OPEN), listener.delta)

        erlProject2.workspaceProject.open(null)
        assertEquality(newDelta().insertAdded(erlProject2, HandleDelta.F_OPEN), listener.delta)

        erlProject2.workspaceProject.delete(true, null)
        assertEquality(newDelta().insertRemoved(erlProject2), listener.delta)

        val IProjectDescription description = erlProject1.workspaceProject.getDescription()
        val String[] oldNatures = description.getNatureIds()
        description.setNatureIds(#[])
        erlProject1.workspaceProject.setDescription(description, null)
        assertEquality(newDelta().insertRemoved(erlProject1, HandleDelta.F_DESCRIPTION), listener.delta)

        description.setNatureIds(oldNatures)
        erlProject1.workspaceProject.setDescription(description, null)
        assertEquality(newDelta().insertChanged(erlProject1, HandleDelta.F_DESCRIPTION.bitwiseOr(HandleDelta.F_CONTENT)),
            listener.delta)

        val IErlProject movedErlProject1 = erlModel.getProject("Test")
        erlProject1.workspaceProject.move(new Path("Test"), true, null)
        assertEquality(
            newDelta().insertMovedTo(movedErlProject1, erlProject1).insertMovedFrom(erlProject1, movedErlProject1),
            listener.delta)
    }

    def private HandleDelta newDelta() {
        val erlModel = ErlModelCore.erlModel
        return new HandleDelta(erlModel)
    }

    def private static void assertEquality(IHandleDelta expected, IHandleDelta actual) {
        if (expected === null) {
            assertThat(actual, is(nullValue))
            return
        }
        assertThat(actual, is(not(nullValue)))
        assertThat(expected.element, is(actual.element))
        assertThat(expected.kind, is(actual.kind))
        assertThat(expected.getFlags(), is(actual.getFlags()))
        assertThat(expected.getMovedToElement(), is(actual.getMovedToElement()))
        assertThat(expected.getMovedFromElement(), is(actual.getMovedFromElement()))
        val expectedChildren = expected.getAffectedChildren()
        val actualChildren = actual.getAffectedChildren()
        assertThat(expectedChildren.length, is(actualChildren.length))
        for (var i = 0; i < expectedChildren.length; i++) {
            assertEquality(expectedChildren.get(i), actualChildren.get(i))
        }
    }

    private static class ErlModelListener implements IElementChangeListener {
        public HandleDelta delta

        override void elementChanged(IElementChangeEvent event) {
            delta = event.getDelta() as HandleDelta
        }
    }
}
