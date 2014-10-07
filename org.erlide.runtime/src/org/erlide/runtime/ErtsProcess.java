/*******************************************************************************
 * Copyright (c) 2010 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.runtime;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.Map;

import org.erlide.runtime.api.RuntimeData;
import org.erlide.runtime.internal.ErtsException;
import org.erlide.util.ErlLogger;
import org.erlide.util.SystemConfiguration;

public class ErtsProcess {

    private Process process;
    private volatile int exitCode;
    private final RuntimeData data;

    private static final long EXIT_POLL_INTERVAL = 500;

    public ErtsProcess(final RuntimeData data) {
        this.data = data;
    }

    public synchronized void startUp() {
        if (process != null) {
            return;
        }
        exitCode = -1;
        process = startRuntimeProcess();
        if (process == null) {
            throw new IllegalStateException("Could not start runtime process " + data);
        }
    }

    public synchronized void shutDown() {
        process.destroy();
        process = null;
    }

    public Process getProcess() {
        return process;
    }

    private Process startRuntimeProcess() {
        final String[] cmds = data.getCmdLine();
        final File workingDirectory = new File(data.getWorkingDir());

        try {
            ErlLogger.debug("START node :> " + Arrays.toString(cmds) + " *** "
                    + workingDirectory.getCanonicalPath());
        } catch (final IOException e1) {
            // ignore
        }

        final ProcessBuilder builder = new ProcessBuilder(cmds);
        builder.directory(workingDirectory);
        setEnvironment(data, builder);
        try {
            final Process aProcess = builder.start();
            return aProcess;
        } catch (final IOException e) {
            ErlLogger.error("Could not create runtime: %s", Arrays.toString(cmds));
            ErlLogger.error(e);
            return null;
        }
    }

    private void setEnvironment(final RuntimeData data, final ProcessBuilder builder) {
        final Map<String, String> env = builder.environment();
        if (!SystemConfiguration.getInstance().isOnWindows()
                && SystemConfiguration.getInstance().hasSpecialTclLib()) {
            env.put("TCL_LIBRARY", "/usr/share/tcl/tcl8.4/");
        }
        if (data.getEnv() != null) {
            env.putAll(data.getEnv());
        }
    }

    /**
     * To be called only when we want to make sure the process has exited.
     *
     * @throws ErtsException
     */
    public void waitForExit() throws ErtsException {
        if (process != null) {
            // give up after 4 minutes
            int i = 500;
            // may have to wait for crash dump to be written
            while (i-- > 0 && exitCode < 0) {
                exitCode = -1;
                try {
                    Thread.sleep(EXIT_POLL_INTERVAL);
                    exitCode = process.exitValue();
                } catch (final IllegalThreadStateException e) {
                    // keep trying
                } catch (final InterruptedException e) {
                    // keep trying
                }
            }
            if (i <= 0) {
                throw new ErtsException("Process " + data.getNodeName()
                        + " didn't exit in time... giving up");
            }
        }
    }

    public int getExitCode() {
        return exitCode;
    }

    public boolean isRunning() {
        if (process == null) {
            return false;
        }
        try {
            exitCode = process.exitValue();
            return false;
        } catch (final IllegalThreadStateException e) {
            return true;
        }
    }

}
