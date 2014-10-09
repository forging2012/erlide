/*******************************************************************************
 * Copyright (c) 2000, 2004 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse def License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.core.internal.builder

import org.eclipse.core.resources.IProject
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.core.runtime.OperationCanceledException
import org.eclipse.core.runtime.SubMonitor
import org.eclipse.osgi.util.NLS
import org.erlide.core.builder.BuilderHelper
import org.erlide.util.ErlLogger

/**
 * Wrapper for a progress monitor.
 *
 * We this is matching the return from the rebar builder, assuming the following:
 * - 5 phases: clean, compile, eunit_compile*, eunit*, xref* (the last three are optional)
 * - phases work: clean 5%, compile 65%, eunit_compile 5%, eunit 20%, xref 5%
 * - compile phase has 10 steps
 * - '.erl' step takes 91% of the progress, the rest 1% each
 */
class BuildNotifier {

    val static ERL_STEP_WORK = 91
    val static OTHER_STEP_WORK = 1

    boolean cancelling
    val SubMonitor monitor
    SubMonitor phaseMonitor
    SubMonitor stepMonitor

    /**
     * @param monitor the progress monitor to use for reporting progress to the user.
     *  It is the caller's responsibility to call done() on the given monitor.
     *  Accepts null, indicating that no progress should be reported and that the
     *  operation cannot be cancelled.
     */
    new(IProgressMonitor monitor, IProject project) {
        this.monitor = SubMonitor.convert(monitor, 100)
        cancelling = false
    }

    def void begin() {
    }

    def void newPhase(String name) {
        val phase = BuildPhase.valueOf(name.toUpperCase)
        phaseMonitor = monitor.newChild(phase.work)
        phaseMonitor.workRemaining = 100
    }

    def void newStep(String name, int items) {
        val work = if (name == ".erl") ERL_STEP_WORK else OTHER_STEP_WORK
        stepMonitor = phaseMonitor.newChild(work)
        stepMonitor.workRemaining = items
    }

    def void compiled(String path) {
        val message = NLS.bind(BuilderMessages.build_compiled, path)
        stepMonitor.subTask(message)
        if (BuilderHelper.isDebugging()) {
            ErlLogger.debug("<< " + message)
        }
        stepMonitor.worked(1)
        checkCancelWithinCompiler()
    }

    def void checkCancel() {
        if (monitor.isCanceled()) {
            throw new OperationCanceledException()
        }
    }

    /**
     * Check whether the build has been canceled. Must use this call instead of
     * checkCancel() when within the compiler.
     */
    def void checkCancelWithinCompiler() {
        if (monitor.isCanceled() && !cancelling) {
            // Once the compiler has been canceled, don't check again.
            setCancelling(true)

            throw new BuilderCanceledException()
        }
    }

    def void done() {
        monitor.done()
    }

    def void setCancelling(boolean cancelling) {
        this.cancelling = cancelling
    }

    def boolean isCanceled() {
        monitor.isCanceled()
    }

    def worked(int work) {
        monitor.worked(work)
    }

}
