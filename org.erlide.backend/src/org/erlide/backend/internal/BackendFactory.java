/*******************************************************************************
 * Copyright (c) 2005-2011 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.backend.internal;

import java.io.File;
import java.util.Map;

import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.model.IProcess;
import org.eclipse.jdt.annotation.NonNull;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.erlide.backend.BackendCore;
import org.erlide.backend.BackendUtils;
import org.erlide.backend.api.BackendData;
import org.erlide.backend.api.IBackend;
import org.erlide.backend.api.IBackendFactory;
import org.erlide.backend.api.IBackendManager;
import org.erlide.backend.api.ICodeBundle.CodeContext;
import org.erlide.runtime.ErtsProcess;
import org.erlide.runtime.OtpNodeProxy;
import org.erlide.runtime.api.IOtpNodeProxy;
import org.erlide.runtime.runtimeinfo.IRuntimeInfoCatalog;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;
import org.erlide.util.ErlLogger;
import org.erlide.util.HostnameUtils;
import org.erlide.util.SystemConfiguration;

import com.google.common.collect.Maps;

public class BackendFactory implements IBackendFactory {

    final IRuntimeInfoCatalog runtimeInfoCatalog;

    public BackendFactory(final IRuntimeInfoCatalog runtimeInfoManager) {
        runtimeInfoCatalog = runtimeInfoManager;
    }

    @Override
    public IBackend createIdeBackend() {
        final BackendData data = getIdeBackendData();
        ErlLogger.debug("Create ide backend " + data.getRuntimeInfo().getVersion());
        final IBackend backend = createBackend(data);
        return backend;
    }

    @Override
    public synchronized IBackend createBuildBackend(final RuntimeInfo info) {
        ErlLogger.debug("Create build backend " + info.getVersion().asMajor().toString());
        final IBackend backend = createBackend(getBuildBackendData(info));
        return backend;
    }

    @Override
    public synchronized IBackend createBackend(final BackendData data) {
        ErlLogger.debug("Create backend " + data.getNodeName());

        final ErtsProcess erts;
        if (data.isManaged()) {
            erts = new ErtsProcess(data);
            erts.startUp();

            if (data.getLaunch() != null) {
                final Map<String, String> map = Maps.newHashMap();
                final IProcess proc = DebugPlugin.newProcess(data.getLaunch(),
                        erts.getProcess(), data.getNodeName(), map);

                ErlLogger.debug("@@@ Started erts: %s >> %s", proc.getLabel(),
                        data.getNodeName());
            }
        } else {
            erts = null;
        }
        final IOtpNodeProxy nodeProxy = new OtpNodeProxy(data);
        nodeProxy.startAndWait();

        final IBackendManager backendManager = BackendCore.getBackendManager();
        final Backend b = data.isInternal() ? new InternalBackend(data, nodeProxy, erts)
                : new ExternalBackend(data, nodeProxy, erts);

        b.initialize(data.getContext(), backendManager);
        nodeProxy.addStatusHandler(new Procedure1<Boolean>() {
            @Override
            public void apply(final Boolean up) {
                if (!up) {
                    b.handleCrash(backendManager);
                }
            }
        });

        return b;
    }

    private BackendData getIdeBackendData() {
        final RuntimeInfo info = getIdeRuntimeInfo();
        final BackendData result = new BackendData(info);
        result.setNodeName(getIdeNodeName());
        result.setDebug(false);
        result.setConsole(SystemConfiguration.getInstance().isDeveloper());
        result.setManaged(true);
        result.setRestartable(true);
        result.setLongName(SystemConfiguration.hasFeatureEnabled("erlide.shortname") ? false
                : HostnameUtils.canUseLongNames());
        result.setInternal(true);
        result.setReportErrors(true);
        result.setContext(CodeContext.IDE);
        return result;
    }

    private BackendData getBuildBackendData(final @NonNull RuntimeInfo info) {
        final RuntimeInfo myinfo = new RuntimeInfo(info);

        final BackendData result = new BackendData(myinfo);
        result.setNodeName(info.getVersion().asMajor().toString() + "_"
                + BackendUtils.getErlideNodeNameTag());
        result.setCookie("erlide");
        result.setRestartable(true);
        result.setDebug(false);
        result.setManaged(true);
        result.setConsole(false);
        result.setLongName(HostnameUtils.canUseLongNames());
        result.setInternal(true);
        result.setReportErrors(true);
        result.setContext(CodeContext.IDE);
        return result;
    }

    private RuntimeInfo getIdeRuntimeInfo() {
        final RuntimeInfo runtime = runtimeInfoCatalog.getErlideRuntime();
        if (runtime != null && runtimeHomeDirExists(runtime)) {
            return new RuntimeInfo(runtime);
        }
        for (final RuntimeInfo aruntime : runtimeInfoCatalog.getRuntimes()) {
            if (aruntime != null && runtimeHomeDirExists(aruntime)) {
                return new RuntimeInfo(aruntime);
            }
        }
        return null;
    }

    private boolean runtimeHomeDirExists(final RuntimeInfo runtime) {
        if (runtime == null) {
            return false;
        }
        final String otpHome = runtime.getOtpHome();
        return otpHome != null && new File(otpHome).exists();
    }

    private String getIdeNodeName() {
        final String dflt = BackendUtils.getErlideNodeNameTag() + "_erlide";
        return getLabelProperty(dflt);
    }

    private static String getLabelProperty(final String dflt) {
        return System.getProperty("erlide.label", dflt);
    }

}
