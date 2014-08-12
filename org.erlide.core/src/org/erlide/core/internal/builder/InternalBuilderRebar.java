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
import java.util.Map;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.osgi.util.NLS;
import org.erlide.backend.BackendCore;
import org.erlide.backend.api.BackendException;
import org.erlide.backend.api.IBackend;
import org.erlide.core.builder.BuilderHelper;
import org.erlide.core.builder.BuilderHelper.SearchVisitor;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.builder.BuilderProperties;
import org.erlide.engine.model.builder.ErlangBuilder;
import org.erlide.engine.model.builder.MarkerUtils;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.runtime.rpc.RpcException;
import org.erlide.util.ErlLogger;
import org.erlide.util.erlang.ErlUtils;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;

public class InternalBuilderRebar extends ErlangBuilder {

    BuildNotifier notifier;
    private final BuilderHelper helper = new BuilderHelper();

    @Override
    public IProject[] build(final int kind, final Map<String, String> args,
            final IProgressMonitor monitor) throws CoreException {

        super.build(kind, args, monitor);

        final long time = System.currentTimeMillis();
        final IProject project = getProject();
        if (project == null || !project.isAccessible()) {
            return null;
        }

        // if (BuilderHelper.isDebugging()) {
        ErlLogger.trace("build",
                "Start " + project.getName() + ": " + helper.buildKind(kind));
        // }
        final IErlProject erlProject = ErlangEngine.getInstance().getModel()
                .getErlangProject(project);
        MarkerUtils.removeProblemMarkersFor(project);
        try {
            initializeBuilder(monitor);
            do_build(kind, args, project, erlProject);
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
            cleanup();
            // if (BuilderHelper.isDebugging()) {
            ErlLogger.trace(
                    "build",
                    " Done " + project.getName() + " took "
                            + Long.toString(System.currentTimeMillis() - time));
            // }
        }
        return null;
    }

    private void do_build(final int kind, final Map<String, String> args,
            final IProject project, final IErlProject erlProject) throws BackendException {
        // TODO validate source and include directories
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

        try {
            final OtpErlangObject msgs0 = backend.getRpcSite().call(15000,
                    "erlide_builder_rebar", "build", "s",
                    project.getLocation().toPortableString());

            final OtpErlangObject[] msgs = ((OtpErlangList) msgs0).elements();
            final ErlcMessageParser parser = new ErlcMessageParser(project);
            for (final OtpErlangObject msg : msgs) {
                parser.createMarkers(ErlUtils.asString(msg));
            }

        } catch (final RpcException e) {
            // TODO handle error
            e.printStackTrace();
        }

    }

    @Override
    public void clean(final IProgressMonitor monitor) {
        final IProject project = getProject();
        if (project == null || !project.isAccessible()) {
            return;
        }

        if (BuilderHelper.isDebugging()) {
            ErlLogger.trace("build", "Cleaning " + project.getName() //$NON-NLS-1$
                    + " @ " + new Date(System.currentTimeMillis()));
        }

        try {
            initializeBuilder(monitor);
            MarkerUtils.removeProblemMarkersFor(project);

            final IErlProject erlProject = ErlangEngine.getInstance().getModel()
                    .getErlangProject(project);
            if (erlProject == null) {
                return;
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

            try {
                backend.getRpcSite().call("erlide_builder_rebar", "clean", "s",
                        project.getLocation().toPortableString());

                // TODO reload beams!

            } catch (final RpcException e) {
                // TODO handle error
                e.printStackTrace();
            }
            project.refreshLocal(IResource.DEPTH_INFINITE, null);

        } catch (final Exception e) {
            ErlLogger.error(e);
            final String msg = NLS.bind(BuilderMessages.build_inconsistentProject,
                    e.getLocalizedMessage(), e.getClass().getName());
            MarkerUtils
                    .createProblemMarker(project, null, msg, 0, IMarker.SEVERITY_ERROR);
        } finally {
            cleanup();
            if (BuilderHelper.isDebugging()) {
                ErlLogger.debug("Finished cleaning " + project.getName() //$NON-NLS-1$
                        + " @ " + new Date(System.currentTimeMillis()));
            }
        }
    }

    private void initializeBuilder(final IProgressMonitor monitor) {
        final IProject currentProject = getProject();
        notifier = new BuildNotifier(monitor, currentProject);
        notifier.begin();
    }

    private void cleanup() {
        notifier.done();
        notifier = null;
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
