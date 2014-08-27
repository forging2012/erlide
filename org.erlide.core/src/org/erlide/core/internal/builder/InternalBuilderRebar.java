/*******************************************************************************
 * Copyright (c) 2013 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available
 * at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.core.internal.builder;

import java.util.Date;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.osgi.util.NLS;
import org.erlide.backend.BackendCore;
import org.erlide.backend.api.BackendException;
import org.erlide.backend.api.IBackend;
import org.erlide.core.builder.BuilderHelper;
import org.erlide.core.builder.BuilderHelper.SearchVisitor;
import org.erlide.engine.model.builder.BuilderProperties;
import org.erlide.engine.model.builder.MarkerUtils;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.runtime.events.ErlangEventHandler;
import org.erlide.runtime.rpc.RpcException;
import org.erlide.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangObject;

public class InternalBuilderRebar extends ErlangBuilder {

    private final static OtpErlangAtom FULL = new OtpErlangAtom("full");
    private final static OtpErlangAtom AUTO = new OtpErlangAtom("auto");
    private final static OtpErlangAtom INCREMENTAL = new OtpErlangAtom("incremental");

    private final BuilderHelper helper = new BuilderHelper();

    @Override
    public IProject[] build(final BuildKind kind, final IErlProject erlProject,
            final BuildNotifier notifier) throws CoreException {

        if (erlProject == null) {
            return null;
        }
        final long time = System.currentTimeMillis();
        final IProject project = erlProject.getWorkspaceProject();
        if (project == null || !project.isAccessible()) {
            return null;
        }

        if (BuilderHelper.isDebugging()) {
            ErlLogger.trace("build", "Start " + project.getName() + ": " + kind);
        }

        try {
            initializeBuilder(notifier);
            do_build(kind, project, erlProject, notifier);
            project.refreshLocal(IResource.DEPTH_INFINITE, null);
        } catch (final OperationCanceledException e) {
            if (BuilderHelper.isDebugging()) {
                ErlLogger.debug("Build of " + project.getName() + " was canceled.");
            }
        } catch (final Exception e) {
            ErlLogger.error(e);
            final String msg = NLS.bind(BuilderMessages.build_inconsistentProject,
                    e.getLocalizedMessage(), e.getClass().getName());
            MarkerUtils
                    .createProblemMarker(project, null, msg, 0, IMarker.SEVERITY_ERROR);
        } finally {
            cleanup(notifier);
            if (BuilderHelper.isDebugging()) {
                ErlLogger.trace(
                        "build",
                        " Done " + project.getName() + " took "
                                + Long.toString(System.currentTimeMillis() - time));
            }
        }
        return null;
    }

    private void do_build(final BuildKind kind, final IProject project,
            final IErlProject erlProject, final BuildNotifier notifier)
            throws BackendException {

        final ErlangProjectProperties properties = erlProject.getProperties();
        final IPath out = properties.getOutputDir();
        final IResource outr = project.findMember(out);
        if (outr != null) {
            try {
                outr.setDerived(true, null);
                outr.refreshLocal(IResource.DEPTH_ZERO, null);
            } catch (final CoreException e) {
                // ignore it
            }
        }

        final IBackend backend = BackendCore.getBackendManager().getBuildBackend(
                erlProject);
        if (backend == null) {
            final String message = "No backend with the required "
                    + "version could be found. Can't build.";
            MarkerUtils.createProblemMarker(project, null, message, 0,
                    IMarker.SEVERITY_ERROR);
            throw new BackendException(message);
        }

        final ErlangEventHandler handler = new BuilderEventHandler(backend.getName(),
                notifier, project.getLocation().lastSegment());
        backend.getRuntime().registerEventListener(handler);
        try {
            try {

                final OtpErlangAtom akind = getBuildKindAtom(kind);

                final OtpErlangObject projectInfo = BuilderUtils
                        .createProjectInfo(erlProject);
                // we only need the result if we're not retrieving the messages
                // asynchronously, but we need to wait for the build to finish
                backend.getRpcSite().call(30000, "erlide_builder_rebar", "build", "ax",
                        akind, projectInfo);

                // backend.getRpcSite().call(30000, "erlide_builder_rebar",
                // "eunit", "x",
                // projectInfo);
            } catch (final RpcException e) {
                ErlLogger.error(e);
            }
        } finally {
            backend.getRuntime().unregisterEventListener(handler);
        }
    }

    private OtpErlangAtom getBuildKindAtom(final BuildKind kind) {
        OtpErlangAtom akind;
        switch (kind) {
        case FULL:
            akind = FULL;
            break;
        case AUTO:
            akind = AUTO;
            break;
        case INCREMENTAL:
            akind = INCREMENTAL;
            break;
        default:
            akind = FULL;
            break;
        }
        return akind;
    }

    @Override
    public void clean(final IErlProject erlProject, final BuildNotifier notifier) {
        if (erlProject == null) {
            return;
        }
        final IProject project = erlProject.getWorkspaceProject();
        if (project == null || !project.isAccessible()) {
            return;
        }

        if (BuilderHelper.isDebugging()) {
            ErlLogger.trace("build", "Cleaning " + project.getName() //$NON-NLS-1$
                    + " @ " + new Date(System.currentTimeMillis()));
        }

        try {
            initializeBuilder(notifier);
            MarkerUtils.removeProblemMarkersFor(project);

            final IBackend backend = BackendCore.getBackendManager().getBuildBackend(
                    erlProject);
            if (backend == null) {
                final String message = "No backend with the required "
                        + "version could be found. Can't build.";
                MarkerUtils.createProblemMarker(project, null, message, 0,
                        IMarker.SEVERITY_ERROR);
                throw new BackendException(message);
            }

            try {
                final OtpErlangObject projectInfo = BuilderUtils
                        .createProjectInfo(erlProject);
                backend.getRpcSite().call("erlide_builder_rebar", "clean", "x",
                        projectInfo);
                project.refreshLocal(IResource.DEPTH_INFINITE, null);

            } catch (final RpcException e) {
                ErlLogger.error(e);
            }
            project.refreshLocal(IResource.DEPTH_INFINITE, null);

        } catch (final Exception e) {
            ErlLogger.error(e);
            final String msg = NLS.bind(BuilderMessages.build_inconsistentProject,
                    e.getLocalizedMessage(), e.getClass().getName());
            MarkerUtils
                    .createProblemMarker(project, null, msg, 0, IMarker.SEVERITY_ERROR);
        } finally {
            cleanup(notifier);
            if (BuilderHelper.isDebugging()) {
                ErlLogger.debug("Finished cleaning " + project.getName() //$NON-NLS-1$
                        + " @ " + new Date(System.currentTimeMillis()));
            }
        }
    }

    private void initializeBuilder(final BuildNotifier notifier) {
        notifier.begin();
    }

    private void cleanup(final BuildNotifier notifier) {
        notifier.done();
    }

    public IResource findCorrespondingSource(final IResource beam) throws CoreException {
        final String[] p = beam.getName().split("\\.");
        final SearchVisitor searcher = helper.new SearchVisitor(p[0], null);
        beam.getProject().accept(searcher);
        final IResource source = searcher.getResult();
        return source;
    }

    @Override
    public BuilderProperties getProperties() {
        return null;
    }

}
