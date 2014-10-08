/*******************************************************************************
 * Copyright (c) 2005 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 * Vlad Dumitrescu
 * Jakob Cederlund
 *******************************************************************************/
package org.erlide.backend.launch;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationType;
import org.eclipse.debug.core.model.LaunchConfigurationDelegate;
import org.erlide.backend.BackendCore;
import org.erlide.backend.api.BackendData;
import org.erlide.backend.api.ErlRuntimeAttributes;
import org.erlide.backend.api.IBackend;
import org.erlide.backend.api.ICodeBundle.CodeContext;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.IBeamLocator;
import org.erlide.runtime.epmd.EpmdWatcher;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;
import org.erlide.util.ErlLogger;
import org.erlide.util.HostnameUtils;

public class ErlangLaunchDelegate extends LaunchConfigurationDelegate {

    protected IBackend backend;

    @Override
    public void launch(final ILaunchConfiguration config, final String mode,
            final ILaunch launch, final IProgressMonitor monitor) throws CoreException {
        final BackendData data = getBackendData(config, mode, launch);
        if (data == null) {
            return;
        }

        if (data.isManaged()) {
            setCaptureOutput(launch);
        }
        data.setLaunch(launch);
        if (!isErlangInternalLaunch(launch)) {
            backend = BackendCore.getBackendManager().createExecutionBackend(data);
        } else {
            backend = BackendCore.getBackendManager().getFactory().createBackend(data);
        }
    }

    private BackendData getBackendData(final ILaunchConfiguration config,
            final String mode, final ILaunch launch) throws CoreException {
        if (config == null) {
            throw new IllegalArgumentException();
        }

        RuntimeInfo runtimeInfo = BackendCore.getRuntimeInfoCatalog().getRuntime(
                config.getAttribute(ErlRuntimeAttributes.RUNTIME_NAME, ""));
        if (runtimeInfo == null) {
            runtimeInfo = BackendCore.getRuntimeInfoCatalog().getDefaultRuntime();
        }
        if (runtimeInfo == null) {
            ErlLogger.error("Can't create backend without a runtime defined!");
            return null;
        }
        final String nodeName = config.getAttribute(ErlRuntimeAttributes.NODE_NAME, "");
        final boolean managed = shouldManageNode(nodeName, BackendCore.getEpmdWatcher());
        BackendData data = new BackendData(runtimeInfo, config, mode, managed);
        data = configureBackend(data, mode, launch);
        return data;
    }

    /*
     * Child classes override this to set specific information
     */
    protected BackendData configureBackend(final BackendData data, final String mode,
            final ILaunch launch) {
        data.setLaunch(launch);
        data.setBeamLocator(ErlangEngine.getInstance().getService(IBeamLocator.class));
        if (mode.equals("debug")) {
            data.setContext(CodeContext.DEBUGGER);
        }
        return data;
    }

    private void setCaptureOutput(final ILaunch launch) {
        // important, we don't want the "normal" console for the erlide backend
        final String captureOutput = System.getProperty("erlide.console.stdout", "false");
        launch.setAttribute(DebugPlugin.ATTR_CAPTURE_OUTPUT, captureOutput);
    }

    public static boolean isErlangLaunch(final ILaunch aLaunch) {
        try {
            final ILaunchConfiguration cfg = aLaunch.getLaunchConfiguration();
            final ILaunchConfigurationType type = cfg.getType();
            final String id = type.getIdentifier();
            return IErlangLaunchDelegateConstants.CONFIGURATION_TYPE.equals(id)
                    || IErlangLaunchDelegateConstants.CONFIGURATION_TYPE_INTERNAL
                            .equals(id);
        } catch (final CoreException e) {
            ErlLogger.warn(e);
            return false;
        }
    }

    public static boolean isErlangInternalLaunch(final ILaunch aLaunch) {
        try {
            final ILaunchConfiguration cfg = aLaunch.getLaunchConfiguration();
            final ILaunchConfigurationType type = cfg.getType();
            final String id = type.getIdentifier();
            return IErlangLaunchDelegateConstants.CONFIGURATION_TYPE_INTERNAL.equals(id);
        } catch (final CoreException e) {
            ErlLogger.warn(e);
            return false;
        }
    }

    private static boolean shouldManageNode(final String name,
            final EpmdWatcher epmdWatcher) {
        final int atSignIndex = name.indexOf('@');
        String shortName = name;
        if (atSignIndex > 0) {
            shortName = name.substring(0, atSignIndex);
        }

        boolean isLocal = atSignIndex < 0;
        if (atSignIndex > 0) {
            final String hostname = name.substring(atSignIndex + 1);
            if (HostnameUtils.isThisHost(hostname)) {
                isLocal = true;
            }
        }

        final boolean isRunning = epmdWatcher.hasLocalNode(shortName);
        final boolean result = isLocal && !isRunning;
        return result;
    }

}
