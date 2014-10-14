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

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.core.filesystem.URIUtil;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
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
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.jobs.Job;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.ModelActivator;
import org.erlide.engine.internal.model.erlang.ErlAttribute;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.IErlModel;
import org.erlide.engine.model.builder.BuilderTool;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.util.ErlLogger;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.google.common.collect.Lists;
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
    public final void setUp() throws Exception {
        setAutoBuilding(false);
        tearDown();
    }

    /**
     * Cleans up the workspace.
     */
    @After
    public final void tearDown() throws Exception {
        cleanUpWorkspace();
        getModel().close();
        ModelActivator.cleanupStateDir();
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

    protected IErlModel getModel() {
        return ErlangEngine.getInstance().getModel();
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

    protected IErlProject getErlProject(final String name) {
        return getModel().getErlangProject(getWorkspaceRoot().getProject(name));
    }

    protected IErlModule getErlModule(final String name) {
        try {
            return getModel().findModule(name);
        } catch (final ErlModelException e) {
            return null;
        }
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

    private final boolean deleteDirectory(final File directory) {
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
    protected final void copy(final File source, final File dest) throws IOException {
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

    private void createDir(final File dest) throws IOException {
        if (!dest.exists()) {
            if (!dest.mkdirs()) {
                throw new IOException("Could not create directory " + dest); //$NON-NLS-1$
            }
        }
    }

    // //////////////////////////////////////////////

    private void buildPaths(final IProject project, final Collection<IPath> paths)
            throws CoreException {
        for (final IPath path : paths) {
            if (pathIsOk(path)) {
                final IFolder folder = project.getFolder(path);
                createFolder(folder);
            }
        }
    }

    private boolean pathIsOk(final IPath path) {
        return !path.isAbsolute() && !path.toString().equals(".") && !path.isEmpty();
    }

    protected void createFolder(final IFolder folder) throws CoreException {
        if (!folder.exists()) {
            final IContainer parent = folder.getParent();
            if (parent instanceof IFolder) {
                createFolder((IFolder) parent);
            }
            folder.create(false, true, null);
        }
    }

    public IErlModule createModule(final IErlProject project, final String moduleName,
            final String moduleContents) throws CoreException {
        final IFolder folder = project.getWorkspaceProject().getFolder("src");
        final IErlModule module = createModule(folder, moduleName, moduleContents);
        return module;
    }

    public IErlModule createInclude(final IErlProject project, final String moduleName,
            final String moduleContents) throws CoreException {
        final IFolder folder = project.getWorkspaceProject().getFolder("include");
        final IErlModule module = createModule(folder, moduleName, moduleContents);
        return module;
    }

    private IErlModule createModule(final IFolder folder, final String moduleName,
            final String moduleContents) throws CoreException {
        final IFile file = createFile(folder, moduleName, moduleContents);
        final IErlModel model = getModel();
        IErlModule module = model.findModule(file);
        if (module != null) {
            module.close();
        }

        if (module == null) {
            final String path = file.getLocation().toPortableString();
            module = model.getModuleFromFile(model, file.getName(), path, Charset
                    .defaultCharset().name(), path);
        }
        return module;
    }

    public IFile createFile(final IFolder folder, final String name, final String contents)
            throws CoreException {
        final IFile file = folder.getFile(name);
        final File f = new File(file.getLocation().toOSString());
        f.delete();
        file.create(
                new ByteArrayInputStream(contents.getBytes(Charset.defaultCharset())),
                true, null);
        return file;
    }

    public void deleteModule(final IErlModule module) throws CoreException {
        module.dispose();

        final String scannerName = module.getScannerName();
        final IFile file = (IFile) module.getResource();
        if (file != null && file.exists()) {
            file.delete(true, null);
        }
        final IPath stateDir = new Path(ErlangEngine.getInstance().getStateDir());
        // FIXME this code should not know about caches!
        final String cacheExts[] = { ".noparse", ".refs", ".scan" };
        for (final String ext : cacheExts) {
            final IPath p = stateDir.append(scannerName + ext);
            final File f = new File(p.toOSString());
            if (f.exists()) {
                f.delete();
            }
        }
    }

    public IErlProject createProject(final String name, final IPath path)
            throws CoreException {
        final IProject project2 = getProject(name);
        if (project2.exists()) {
            project2.delete(true, null);
        }
        final IErlProject erlProject = getModel().newProject(name,
                path.toPortableString());
        erlProject.getBuilderProperties().setBuilderTool(BuilderTool.INTERNAL);

        final IProject project = erlProject.getWorkspaceProject();
        final ErlangProjectProperties prefs = erlProject.getProperties();

        final List<IPath> srcDirs = new ArrayList<IPath>();
        srcDirs.add(new Path("src"));
        prefs.setSourceDirs(srcDirs);
        buildPaths(project, srcDirs);

        final List<IPath> includeDirs = new ArrayList<IPath>();
        includeDirs.add(new Path("include"));
        buildPaths(project, includeDirs);
        prefs.setIncludeDirs(includeDirs);

        final IPath ebinDir = new Path("ebin");
        buildPaths(project, Lists.newArrayList(ebinDir));
        prefs.setOutputDir(ebinDir);

        erlProject.open(null);
        return erlProject;
    }

    public IErlProject getExistingProject(final String name) {
        final IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
        final IProject project = root.getProject(name);
        return getModel().getErlangProject(project);
    }

    public IPath getTmpPath(final String fileName) {
        final String tmpdir = System.getProperty("java.io.tmpdir");
        return new Path(tmpdir).append(fileName);
    }

    public URI getTmpURIPath(final String fileName) {
        return URIUtil.toURI(getTmpPath(fileName).toPortableString());
    }

    public File createTmpFile(final String fileName, final String contentString)
            throws IOException, FileNotFoundException {
        final String pathString = getTmpPath(fileName).toOSString();
        final File f = new File(pathString);
        if (f.exists()) {
            f.delete();
        }
        f.createNewFile();
        final FileOutputStream fileOutputStream = new FileOutputStream(pathString);
        fileOutputStream.write(contentString.getBytes());
        fileOutputStream.close();
        return f;
    }

    public void deleteProject(final IErlProject erlProject) throws CoreException {
        final IProject project = erlProject.getWorkspaceProject();
        final IPath location = project.getLocation();
        try {
            project.delete(true, null);
        } catch (final Exception e) {
            ErlLogger.error(e);
        }
        if (location != null) {
            new File(location.toPortableString()).delete();
        }
        erlProject.dispose();
        final IErlModel model = getModel();
        model.resourceChanged(null);
        model.open(null);
    }

    public IErlModule createModuleFromText(final String name, final String initialText) {
        final IErlModel model = getModel();
        final IErlModule module = model.getModuleFromText(model, name, initialText, name);
        return module;
    }

    public IErlProject createTmpErlProject(final String projectName) throws CoreException {
        return createProject(projectName, getTmpPath(projectName));
    }

    public IErlElement createErlAttribute(final IErlElement parent, final String name,
            final OtpErlangObject value, final String extra, final int sourceRangeOffset,
            final int sourceRangeLength) {
        final ErlAttribute attribute = new ErlAttribute(parent, name, value, extra);
        attribute.setSourceRangeOffset(sourceRangeOffset);
        attribute.setSourceRangeLength(sourceRangeLength);
        return attribute;
    }

}
