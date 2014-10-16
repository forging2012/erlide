/*******************************************************************************
 * Copyright (c) 2014 1C LLC.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Vladimir Piskarev (1C) - initial API and implementation
 *     Vlad Dumitrescu - updated to Junit 4
 *******************************************************************************/
package org.eclipse.handly.junit;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceDescription;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.IWorkspaceRunnable;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.jobs.Job;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;

import com.google.common.io.Files;

/**
 * A workspace test case.
 * <p>
 * Provides a number of useful methods, such as {@link #setUpProject(String)}.
 * </p>
 * <p>
 * Also, in its {@link #setUp()} and {@link #tearDown()} methods, this class
 * enforces some common rules for workspace tests:
 * <ul>
 * <li>each test suite starts running in a clean workspace with auto-build
 * turned off;</li>
 * <li>each test is responsible for setting up the necessary workspace state;</li>
 * <li>after running each test suite, the workspace is cleaned up.</li>
 * </ul>
 * </p>
 */
public abstract class WorkspaceTest {
    /**
     * Turns auto-build off, cleans up the workspace.
     */
    @Before
    public void setUp() throws Exception {
        setAutoBuilding(false);
        cleanUpWorkspace();
    }

    /**
     * Cleans up the workspace.
     */
    @After
    public void tearDown() throws Exception {
        cleanUpWorkspace();
    }

    /**
     * Shortcut to <code>ResourcesPlugin.getWorkspace()</code>
     */
    protected final IWorkspace getWorkspace() {
        return ResourcesPlugin.getWorkspace();
    }

    /**
     * Shortcut to <code>getWorkspace().getRoot()</code>
     */
    protected final IWorkspaceRoot getWorkspaceRoot() {
        return getWorkspace().getRoot();
    }

    /**
     * Shortcut to <code>getWorkspaceRoot().getProject(name)</code>
     *
     * @param name
     *            the name of the project
     * @return the project (never <code>null</code>)
     */
    protected final IProject getProject(final String name) {
        return getWorkspaceRoot().getProject(name);
    }

    /**
     * Creates a new project in the workspace by copying its content from the
     * OSGi-bundle of this test case. The content must reside in the folder
     * <code>/workspace/</code>&lt;project-name&gt; inside the bundle.
     *
     * @param name
     *            the name of the project
     * @return the created and opened project (never <code>null</code>)
     * @throws CoreException
     * @throws IOException
     */
    protected final IProject setUpProject(final String name) throws CoreException,
            IOException {
        setUpFile(getWorkspaceURL(), name);

        // create the project
        final IProject project = getProject(name);
        final IWorkspaceRunnable runnable = new IWorkspaceRunnable() {
            @Override
            public void run(final IProgressMonitor monitor) throws CoreException {
                project.create(null);
                project.open(null);
            }
        };
        getWorkspace().run(runnable, null);
        return project;
    }

    protected final void setUpFile(final URL resourceURL, final String path)
            throws IOException {
        // copy the project's content
        Assert.assertNotNull(resourceURL);
        final URL fileURL = FileLocator.toFileURL(resourceURL);
        File sourceRoot;
        try {
            sourceRoot = new File(fileURL.toURI());
        } catch (final URISyntaxException e) {
            throw new IOException(e);
        }
        Assert.assertNotNull(sourceRoot);
        final File source = new File(sourceRoot, path);
        Assert.assertTrue(source + " doesn't exist", source.exists());
        final File targetRoot = getWorkspaceRoot().getLocation().toFile();
        final File target = new File(targetRoot, path);
        copy(source, target);
    }

    protected final void setUpTmpFile(final String path) throws IOException {
        setUpFile(getWorkspaceURL(), "tmp/" + path);
    }

    public URL getWorkspaceURL() {
        return getClass().getClassLoader().getResource("workspace");
    }

    /**
     * Sets the property "Build Automatically" for the workspace.
     *
     * @param value
     *            boolean
     * @throws CoreException
     */
    protected final void setAutoBuilding(final boolean value) throws CoreException {
        final IWorkspaceDescription description = getWorkspace().getDescription();
        if (value != description.isAutoBuilding()) {
            description.setAutoBuilding(value);
            getWorkspace().setDescription(description);
        }
    }

    /**
     * Builds the workspace, waiting for build completion.
     *
     * @throws CoreException
     */
    protected final void buildWorkspace() throws CoreException {
        getWorkspace().build(IncrementalProjectBuilder.INCREMENTAL_BUILD, null);
        waitForBuildCompletion();
    }

    /**
     * Waits for build completion.
     */
    protected final void waitForBuildCompletion() {
        boolean wasInterrupted = false;
        do {
            try {
                Job.getJobManager().join(ResourcesPlugin.FAMILY_AUTO_BUILD, null);
                wasInterrupted = false;
            } catch (final OperationCanceledException e) {
                e.printStackTrace();
            } catch (final InterruptedException e) {
                wasInterrupted = true;
            }
        } while (wasInterrupted);
    }

    /**
     * Deletes all resources in the workspace.
     *
     * @throws CoreException
     */
    protected final void cleanUpWorkspace() throws CoreException {
        final IWorkspaceRoot root = getWorkspaceRoot();
        root.delete(IResource.ALWAYS_DELETE_PROJECT_CONTENT, null);
        final File file = new File(root.getLocation().toPortableString(), "tmp");
        deleteDirectory(file);
    }

    private static final boolean deleteDirectory(final File directory) {
        if (directory.exists()) {
            final File[] files = directory.listFiles();
            if (null != files) {
                for (final File f : files) {
                    if (f.isDirectory()) {
                        deleteDirectory(f);
                    } else {
                        f.delete();
                    }
                }
            }
        }
        return directory.delete();
    }

    /*
     * Copy the given source (a file or a directory) to the given destination
     * directory.
     */
    protected static final void copy(final File source, final File dest)
            throws IOException {
        if (!source.exists()) {
            return;
        }

        if (source.isDirectory()) {
            createDir(dest);
            final String[] files = source.list();
            if (files != null) {
                for (final String file : files) {
                    final File sourceFile = new File(source, file);
                    if (sourceFile.isDirectory()) {
                        final File destSubDir = new File(dest, file);
                        copy(sourceFile, destSubDir);
                    } else {
                        copy(sourceFile, dest);
                    }
                }
            }
        } else {
            File destDir;
            if (dest.isDirectory()) {
                destDir = dest;
            } else {
                destDir = dest.getParentFile();
            }
            createDir(destDir);
            final File destFile = new File(destDir, source.getName());
            if (!destFile.createNewFile()) {
                throw new IOException(destFile + " already exists"); //$NON-NLS-1$
            }
            Files.copy(source, destFile);
        }
    }

    private static void createDir(final File dest) throws IOException {
        if (!dest.exists()) {
            if (!dest.mkdirs()) {
                throw new IOException("Could not create directory " + dest); //$NON-NLS-1$
            }
        }
    }
}
