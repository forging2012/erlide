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
import org.eclipse.core.runtime.QualifiedName;
import org.eclipse.osgi.util.NLS;
import org.erlide.backend.BackendCore;
import org.erlide.backend.api.BackendException;
import org.erlide.backend.api.IBackend;
import org.erlide.core.ErlangCore;
import org.erlide.core.builder.BuilderHelper;
import org.erlide.core.builder.BuilderHelper.SearchVisitor;
import org.erlide.engine.model.builder.BuilderProperties;
import org.erlide.engine.model.builder.MarkerUtils;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.runtime.events.ErlangEventHandler;
import org.erlide.runtime.rpc.RpcException;
import org.erlide.util.ErlLogger;

import com.ericsson.otp.erlang.OtpErlangObject;

public class InternalBuilderRebar extends ErlangBuilder {

    private static final QualifiedName BUILDER_STATE = new QualifiedName(
            ErlangCore.PLUGIN_ID, "builder_state");

    private final BuilderHelper helper = new BuilderHelper();
    private BuilderState state;
    // TODO user configurable
    final boolean compileEunit = false;
    // TODO user configurable
    final boolean runXref = false;
    // TODO user configurable
    final boolean runEunit = false;

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
            initializeBuilder(project, notifier);
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
            cleanup(project, notifier);
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
                notifier, project, state);
        backend.getRuntime().registerEventListener(handler);
        try {
            try {
                final OtpErlangObject projectInfo = BuilderUtils
                        .createProjectInfo(erlProject);
                if (kind == BuildKind.FULL) {
                    backend.getOtpRpc().call(30000, "erlide_builder_rebar", "clean", "x",
                            projectInfo);
                }

                backend.getOtpRpc().call(30000, "erlide_builder_rebar", "build", "x",
                        projectInfo);

                if (compileEunit) {
                    backend.getOtpRpc().call(30000, "erlide_builder_rebar",
                            "build_eunit", "x", projectInfo);
                }

                if (runXref) {
                    backend.getOtpRpc().call(30000, "erlide_builder_rebar", "xref", "x",
                            projectInfo);
                }
                if (runEunit) {
                    backend.getOtpRpc().call(30000, "erlide_builder_rebar", "eunit", "x",
                            projectInfo);
                }

            } catch (final RpcException e) {
                ErlLogger.error(e);
            }
        } finally {
            backend.getRuntime().unregisterEventListener(handler);
        }
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
            initializeBuilder(project, notifier);
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
                backend.getOtpRpc().call("erlide_builder_rebar", "clean", "x",
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
            cleanup(project, notifier);
            if (BuilderHelper.isDebugging()) {
                ErlLogger.debug("Finished cleaning " + project.getName() //$NON-NLS-1$
                        + " @ " + new Date(System.currentTimeMillis()));
            }
        }
    }

    private void initializeBuilder(final IProject project, final BuildNotifier notifier)
            throws CoreException {
        state = null;
        state = (BuilderState) project.getSessionProperty(BUILDER_STATE);
        notifier.begin();
    }

    private void cleanup(final IProject project, final BuildNotifier notifier) {
        notifier.done();
        try {
            project.setSessionProperty(BUILDER_STATE, state);
        } catch (final CoreException e) {
        }
        state = null;
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
