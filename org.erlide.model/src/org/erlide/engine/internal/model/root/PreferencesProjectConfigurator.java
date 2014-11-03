package org.erlide.engine.internal.model.root;

import org.eclipse.core.resources.WorkspaceJob;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.jdt.annotation.NonNull;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.model.root.IProjectConfigurator;
import org.erlide.engine.model.root.PathSerializer;
import org.erlide.engine.model.root.ProjectPreferencesConstants;
import org.erlide.runtime.runtimeinfo.RuntimeVersion;
import org.erlide.util.ErlLogger;
import org.osgi.service.prefs.BackingStoreException;

public class PreferencesProjectConfigurator implements IProjectConfigurator {
    @NonNull
    private final IEclipsePreferences node;
    private final IEclipsePreferences oldNode;

    public PreferencesProjectConfigurator(final IEclipsePreferences node,
            final IEclipsePreferences oldNode) {
        Assert.isNotNull(node);
        this.node = node;
        this.oldNode = oldNode;
    }

    public ErlangProjectProperties getConfiguration(final IEclipsePreferences aNode) {
        final ErlangProjectProperties result = new ErlangProjectProperties();

        final String sourceDirsStr = aNode.get(ProjectPreferencesConstants.SOURCE_DIRS,
                ProjectPreferencesConstants.DEFAULT_SOURCE_DIRS);
        result.setSourceDirs(PathSerializer.unpackList(sourceDirsStr));
        final String includeDirsStr = aNode.get(ProjectPreferencesConstants.INCLUDE_DIRS,
                ProjectPreferencesConstants.DEFAULT_INCLUDE_DIRS);
        result.setIncludeDirs(PathSerializer.unpackList(includeDirsStr));
        final String outputDirStr = aNode.get(ProjectPreferencesConstants.OUTPUT_DIR,
                ProjectPreferencesConstants.DEFAULT_OUTPUT_DIR);
        final String outputStr = outputDirStr.replaceAll(";", "");
        result.setOutputDir(new Path(outputStr));
        result.setRequiredRuntimeVersion(RuntimeVersion.Serializer.parse(aNode.get(
                ProjectPreferencesConstants.RUNTIME_VERSION, null)));
        if (!result.getRequiredRuntimeVersion().isDefined()) {
            result.setRequiredRuntimeVersion(ProjectPreferencesConstants.FALLBACK_RUNTIME_VERSION);
        }
        result.setExternalModulesFile(aNode.get(
                ProjectPreferencesConstants.PROJECT_EXTERNAL_MODULES,
                ProjectPreferencesConstants.DEFAULT_EXTERNAL_MODULES));
        result.setExternalIncludesFile(aNode.get(
                ProjectPreferencesConstants.EXTERNAL_INCLUDES,
                ProjectPreferencesConstants.DEFAULT_EXTERNAL_INCLUDES));
        return result;
    }

    @Override
    public ErlangProjectProperties getConfiguration(final IPath baseDir) {
        ErlangProjectProperties result = null;
        // new config takes precedence
        if (hasData(node)) {
            result = getConfiguration(node);
        } else if (hasData(oldNode)) {
            result = getConfiguration(oldNode);
            setConfiguration(result);
            try {
                oldNode.removeNode();
                // oldNode.flush();
            } catch (final BackingStoreException e) {
                // ignore, projects may be read-only
                ErlLogger
                        .warn("Could not delete old project configuration, is project read-only? "
                                + e.getMessage());
            }
        } else {
            result = getConfiguration(node);
        }
        result.setBaseDir(baseDir);
        return result;
    }

    @Override
    public void setConfiguration(final ErlangProjectProperties info) {
        node.put(ProjectPreferencesConstants.SOURCE_DIRS,
                PathSerializer.packList(info.getSourceDirs()));
        node.put(ProjectPreferencesConstants.INCLUDE_DIRS,
                PathSerializer.packList(info.getIncludeDirs()));
        node.put(ProjectPreferencesConstants.OUTPUT_DIR, info.getOutputDir()
                .toPortableString());
        node.put(ProjectPreferencesConstants.EXTERNAL_INCLUDES,
                info.getExternalIncludesFile());
        if (info.getRequiredRuntimeVersion().isDefined()) {
            node.put(ProjectPreferencesConstants.RUNTIME_VERSION, info
                    .getRequiredRuntimeVersion().asMinor().toString());
        } else {
            node.remove(ProjectPreferencesConstants.RUNTIME_VERSION);
        }
        node.put(ProjectPreferencesConstants.PROJECT_EXTERNAL_MODULES,
                info.getExternalModulesFile());

        // this might be called from a resource change event, so the workspace is locked
        // for modification; we do it asynchronously
        WorkspaceJob job = new WorkspaceJob("erlide") {
            @Override
            public IStatus runInWorkspace(IProgressMonitor monitor) throws CoreException {
                try {
                    node.flush();
                } catch (final BackingStoreException e) {
                    // projects may be read-only
                    ErlLogger
                            .warn("Could not set project configuration, is project read-only? "
                                    + e.getMessage());
                }
                return Status.OK_STATUS;
            }
        };
        job.schedule();

    }

    private boolean hasData(final IEclipsePreferences aNode) {
        return aNode.get(ProjectPreferencesConstants.SOURCE_DIRS, null) != null;
    }
}
