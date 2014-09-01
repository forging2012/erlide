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

import java.text.NumberFormat
import org.eclipse.core.resources.IProject
import org.eclipse.core.resources.IResource
import org.eclipse.core.runtime.IPath
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.core.runtime.NullProgressMonitor
import org.eclipse.core.runtime.OperationCanceledException
import org.eclipse.osgi.util.NLS
import org.erlide.core.builder.BuilderHelper
import org.erlide.util.ErlLogger

class BuildNotifier {

    val IProgressMonitor monitor
    boolean cancelling
    float percentComplete
    float progressPerCompilationUnit
    int workDone
    int totalWork
    String previousSubtask

    new(IProgressMonitor monitor, IProject project) {
        this.monitor = monitor ?: new NullProgressMonitor()
        cancelling = false
    }

    def void begin() {
        workDone = 0
        totalWork = 100000
        monitor.beginTask("building", totalWork)
        previousSubtask = null
    }

    def void newPhase(String string) {
    }

    def void newStep(String string, int i) {
    }

    /**
     * Notification before a compile that a unit is about to be compiled.
     */
    def void aboutToCompile(IResource unit) {
        checkCancel()
        val message = NLS.bind(BuilderMessages.build_compiling, unit.getFullPath())
        subTask(message)
        if (BuilderHelper.isDebugging()) {
            ErlLogger.debug(">>" + message)
        }
    }

    /**
     * Check whether the build has been canceled.
     */
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
        // stop compiler
        }
    }

    def void compiled(IResource unit) {
        compiled(unit.getFullPath())
    }

    def void compiled(IPath path) {
        compiled(path.toPortableString())
    }

    def void compiled(String path) {
        val message = NLS.bind(BuilderMessages.build_compiled, path)
        subTask(message)
        if (BuilderHelper.isDebugging()) {
            ErlLogger.debug("<< " + message)
        }
        updateProgressDelta(progressPerCompilationUnit)
        checkCancelWithinCompiler()
    }

    def void done() {
        updateProgress(1.0f)
        subTask(BuilderMessages.build_done)
        monitor.done()
        previousSubtask = null
    }

    def void setCancelling(boolean cancelling) {
        this.cancelling = cancelling
    }

    def void subTask(String message) {
        if (message.equals(previousSubtask)) {
            return // avoid refreshing with same one
        }
        monitor.subTask(message)
        previousSubtask = message
    }

    def void updateProgress(float newPercentComplete) {
        if (newPercentComplete > percentComplete) {
            percentComplete = Math.min(newPercentComplete, 1.0f)
            val work = Math.round(percentComplete * totalWork)
            if (work > workDone) {
                monitor.worked(work - workDone)
                if (BuilderHelper.isDebugging()) {
                    ErlLogger.debug(NumberFormat.getPercentInstance().format(percentComplete))
                }
                workDone = work
            }
        }
    }

    def void updateProgressDelta(float percentWorked) {
        updateProgress(percentComplete + percentWorked)
    }

    def boolean isCanceled() {
        monitor.isCanceled()
        return false
    }

    def void worked(int i) {
        monitor.worked(i)
    }

    def void beginTask(String name, int length) {
        monitor.beginTask(name, length)
    }

    def void doneTask() {
        monitor.done()
    }

}
