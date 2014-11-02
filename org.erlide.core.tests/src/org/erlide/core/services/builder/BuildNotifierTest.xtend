package org.erlide.core.services.builder

import java.util.List
import org.eclipse.core.runtime.NullProgressMonitor
import org.eclipse.core.runtime.ProgressMonitorWrapper
import org.erlide.core.internal.builder.BuildNotifier
import org.erlide.core.internal.builder.BuildPhase
import org.hamcrest.MatcherAssert
import org.hamcrest.Matchers
import org.junit.Before
import org.junit.Test

class BuildNotifierTest {

    val mon = new ProgressMonitorWrapper(new NullProgressMonitor()) {
        override void worked(int work) {
            trace(work)
        }
    };

    val List<Integer> crtTrace = newArrayList()
    BuildNotifier notifier

    def void trace(int text) {
        crtTrace.add(text)
    }

    @Before
    def void before() {
        crtTrace.clear
        notifier = new BuildNotifier(mon, null)
    }

    @Test
    def void clean_phase() {
        notifier.begin
        notifier.newPhase("clean")
        notifier.done

        val w = BuildPhase.CLEAN.work * 10
        MatcherAssert.assertThat(
            crtTrace,
            Matchers.contains(w, 1000 - w)
        )
    }

    @Test
    def void compile_phase() {
        notifier.begin
        notifier.newPhase("compile")
        notifier.done

        val w = BuildPhase.COMPILE.work * 10
        MatcherAssert.assertThat(
            crtTrace,
            Matchers.contains(w, 1000 - w)
        )
    }

    @Test
    def void two_phases() {
        notifier.begin
        notifier.newPhase("clean")
        notifier.newPhase("compile")
        notifier.done

        val w = BuildPhase.CLEAN.work * 10
        val w1 = BuildPhase.COMPILE.work * 10
        MatcherAssert.assertThat(
            crtTrace,
            Matchers.contains(w, w1, 1000 - w - w1)
        )
    }

    @Test
    def void compile_steps() {
        notifier.begin
        notifier.newPhase("compile")
        notifier.newStep(".foo", 0)
        notifier.newStep(".zax", 2)
        notifier.compiled("foo1")
        notifier.compiled("foo2")
        notifier.newStep(".erl", 4)
        notifier.compiled("erl1")
        notifier.compiled("erl2")
        notifier.compiled("erl3")
        notifier.compiled("erl4")
        notifier.newStep(".bar", 0)
        notifier.done

        val w = BuildPhase.COMPILE.work * 10
        val s1 = w / 100
        val s1c = s1 / 2
        val s2 = w * 91 / 100
        val s2c = s2 / 4

        // matching this is tricky because of rounding
        MatcherAssert.assertThat(
            crtTrace,
            Matchers.contains(s1, s1c, s1c + 1, s2c, s2c + 1, s2c + 1, s2 - 3 * s2c - 2, s1 + 1, 39, 1000 - w)
        )
    }

}
