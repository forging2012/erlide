/*******************************************************************************
 * Copyright (c) 2014 1C LLC. All rights reserved. This program and the accompanying
 * materials are made available under the terms of the Eclipse Public License v1.0 which
 * accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors: Vladimir Piskarev (1C) - initial API and implementation Vlad Dumitrescu -
 * updated to Junit 4
 *******************************************************************************/
package org.erlide.testing.utils

import com.ericsson.otp.erlang.OtpErlangObject
import com.google.common.collect.Lists
import com.google.common.io.Files
import java.io.ByteArrayInputStream
import java.io.File
import java.io.FileNotFoundException
import java.io.FileOutputStream
import java.io.IOException
import java.net.URI
import java.net.URISyntaxException
import java.net.URL
import java.nio.charset.Charset
import java.util.ArrayList
import java.util.Collection
import java.util.List
import org.eclipse.core.resources.IContainer
import org.eclipse.core.resources.IFile
import org.eclipse.core.resources.IFolder
import org.eclipse.core.resources.IProject
import org.eclipse.core.resources.IResource
import org.eclipse.core.resources.IWorkspace
import org.eclipse.core.resources.IWorkspaceRoot
import org.eclipse.core.resources.IWorkspaceRunnable
import org.eclipse.core.resources.IncrementalProjectBuilder
import org.eclipse.core.resources.ResourcesPlugin
import org.eclipse.core.runtime.CoreException
import org.eclipse.core.runtime.FileLocator
import org.eclipse.core.runtime.IPath
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.core.runtime.OperationCanceledException
import org.eclipse.core.runtime.Path
import org.eclipse.core.runtime.jobs.Job
import org.erlide.engine.ErlangEngine
import org.erlide.engine.ModelActivator
import org.erlide.engine.internal.model.erlang.ErlAttribute
import org.erlide.engine.model.ErlModelException
import org.erlide.engine.model.IErlModel
import org.erlide.engine.model.builder.BuilderTool
import org.erlide.engine.model.erlang.IErlModule
import org.erlide.engine.model.root.ErlangProjectProperties
import org.erlide.engine.model.root.IErlElement
import org.erlide.engine.model.root.IErlProject
import org.erlide.util.ErlLogger
import org.junit.After
import org.junit.Assert
import org.junit.Before
import org.eclipse.core.filesystem.URIUtil

/**
 * A workspace test case.
 * <p>
 * Provides a number of useful methods, such as {@link #setUpProject(String)}.
 * </p>
 * <p>
 * Also, in its {@link #setUp()} and {@link #tearDown()} methods, this class enforces some
 * common rules for workspace tests:
 * <ul>
 * <li>each test suite starts running in a clean workspace with auto-build turned off</li>
 * <li>each test is responsible for setting up the necessary workspace state</li>
 * <li>after running each test suite, the workspace is cleaned up.</li>
 * </ul>
 * </p>
 */
class WorkspaceTest {

    /**
     * Turns auto-build off, cleans up the workspace.
     */
    @Before
    def void setUp() throws Exception {
        ModelActivator.initModel()
        setAutoBuilding(false)
        workspaceRoot.setDefaultCharset("UTF-8", null)
        tearDown()
    }

    /**
     * Cleans up the workspace.
     */
    @After
    def void tearDown() throws Exception {
        cleanUpWorkspace()
        model.close()
        ModelActivator.cleanupStateDir()
    }

    /**
     * Shortcut to <code>ResourcesPlugin.workspace</code>
     */
    protected def IWorkspace getWorkspace() {
        return ResourcesPlugin.workspace
    }

    /**
     * Shortcut to <code>workspace.root</code>
     */
    protected def IWorkspaceRoot getWorkspaceRoot() {
        return workspace.root
    }

    protected def IErlModel getModel() {
        return ErlangEngine.instance.model
    }

    /**
     * Shortcut to <code>getWorkspaceRoot().getProject(name)</code>
     *
     * @param name
     *            the name of the project
     * @return the project (never <code>null</code>)
     */
    protected def IProject getProject(String name) {
        return workspaceRoot.getProject(name)
    }

    /**
     * Creates a new project in the workspace by copying its content from the OSGi-bundle
     * of this test case. The content must reside in the folder <code>/workspace/</code>
     * &ltproject-name&gt inside the bundle.
     *
     * @param name
     *            the name of the project
     * @return the created and opened project (never <code>null</code>)
     * @throws CoreException
     * @throws IOException
     */
    protected def IProject setUpProject(String name) throws CoreException,
            IOException {
        setUpFile(workspaceURL, name)

        // create the project
        val project = getProject(name)
        val runnable = new IWorkspaceRunnable() {
            override void run(IProgressMonitor monitor) throws CoreException {
                project.create(null)
                project.open(null)
            }
        }
        workspace.run(runnable, null)
        return project
    }

    protected def void setUpFile(URL resourceURL, String path) throws IOException {

        // copy the project's content
        Assert.assertNotNull(resourceURL)
        val fileURL = FileLocator.toFileURL(resourceURL)
        var File sourceRoot
        try {
            sourceRoot = new File(fileURL.toURI())
        } catch (URISyntaxException e) {
            throw new IOException(e)
        }
        Assert.assertNotNull(sourceRoot)
        val source = new File(sourceRoot, path)
        Assert.assertTrue(source + " doesn't exist", source.exists())
        val targetRoot = workspaceRoot.location.toFile()
        val target = new File(targetRoot, path)
        copy(source, target)
    }

    protected def void setUpTmpFile(String path) throws IOException {
        setUpFile(workspaceURL, "tmp/" + path)
    }

    def URL getWorkspaceURL() {
        return class.classLoader.getResource("workspace")
    }

    protected def IErlProject getErlProject(String name) {
        return model.getErlangProject(getWorkspaceRoot().getProject(name))
    }

    protected def IErlModule getErlModule(String name) {
        try {
            return model.findModule(name)
        } catch (ErlModelException e) {
            return null
        }
    }

    /**
     * Sets the property "Build Automatically" for the workspace.
     *
     * @param value
     *            boolean
     * @throws CoreException
     */
    protected def void setAutoBuilding(boolean value) throws CoreException {
        val description = workspace.description
        if (value != description.autoBuilding) {
            description.autoBuilding = value
            workspace.description = description
        }
    }

    /**
     * Builds the workspace, waiting for build completion.
     *
     * @throws CoreException
     */
    protected def void buildWorkspace() throws CoreException {
        workspace.build(IncrementalProjectBuilder.INCREMENTAL_BUILD, null)
        waitForBuildCompletion()
    }

    /**
     * Waits for build completion.
     */
    protected def void waitForBuildCompletion() {
        var wasInterrupted = false
        do {
            try {
                Job.getJobManager().join(ResourcesPlugin.FAMILY_AUTO_BUILD, null)
                wasInterrupted = false
            } catch (OperationCanceledException e) {
                e.printStackTrace()
            } catch (InterruptedException e) {
                wasInterrupted = true
            }
        } while (wasInterrupted)
    }

    /**
     * Deletes all resources in the workspace.
     *
     * @throws CoreException
     */
    protected def void cleanUpWorkspace() throws CoreException {
        workspaceRoot.delete(IResource.ALWAYS_DELETE_PROJECT_CONTENT, null)
        val file = new File(workspaceRoot.location.toPortableString(), "tmp")
        deleteDirectory(file)
    }

    private def boolean deleteDirectory(File directory) {
        if (directory.exists()) {
            val File[] files = directory.listFiles()
            if (null !== files) {
                for (File f : files) {
                    if (f.isDirectory()) {
                        deleteDirectory(f)
                    } else {
                        f.delete()
                    }
                }
            }
        }
        return directory.delete()
    }

    /*
     * Copy the given source (a file or a directory) to the given destination directory.
     */
    protected def void copy(File source, File dest) throws IOException {
        if (!source.exists()) {
            return
        }

        if (source.isDirectory()) {
            createDir(dest)
            val String[] files = source.list()
            if (files !== null) {
                for (String file : files) {
                    val File sourceFile = new File(source, file)
                    if (sourceFile.isDirectory()) {
                        val File destSubDir = new File(dest, file)
                        copy(sourceFile, destSubDir)
                    } else {
                        copy(sourceFile, dest)
                    }
                }
            }
        } else {
            var File destDir
            if (dest.isDirectory()) {
                destDir = dest
            } else {
                destDir = dest.getParentFile()
            }
            createDir(destDir)
            val File destFile = new File(destDir, source.getName())
            if (!destFile.createNewFile()) {
                throw new IOException(destFile + " already exists") //$NON-NLS-1$
            }
            Files.copy(source, destFile)
        }
    }

    private def void createDir(File dest) throws IOException {
        if (!dest.exists()) {
            if (!dest.mkdirs()) {
                throw new IOException("Could not create directory " + dest) //$NON-NLS-1$
            }
        }
    }

    // //////////////////////////////////////////////
    private def void buildPaths(IProject project, Collection<IPath> paths) throws CoreException {
        for (IPath path : paths) {
            if (pathIsOk(path)) {
                val IFolder folder = project.getFolder(path)
                createFolder(folder)
            }
        }
    }

    private def boolean pathIsOk(IPath path) {
        return !path.isAbsolute() && !path.toString().equals(".") && !path.isEmpty()
    }

    protected def void createFolder(IFolder folder) throws CoreException {
        if (!folder.exists()) {
            val IContainer parent = folder.getParent()
            if (parent instanceof IFolder) {
                createFolder(parent)
            }
            folder.create(false, true, null)
        }
    }

    def IErlModule createModule(IErlProject project, String moduleName, String moduleContents) throws CoreException {
        val IFolder folder = project.workspaceProject.getFolder("src")
        val IErlModule module = createModule(folder, moduleName, moduleContents)
        return module
    }

    def IErlModule createInclude(IErlProject project, String moduleName, String moduleContents) throws CoreException {
        val IFolder folder = project.workspaceProject.getFolder("include")
        val IErlModule module = createModule(folder, moduleName, moduleContents)
        return module
    }

    private def IErlModule createModule(IFolder folder, String moduleName, String moduleContents) throws CoreException {
        val IFile file = createFile(folder, moduleName, moduleContents)
        val IErlModel model = getModel()
        var IErlModule module = model.findModule(file)
        if (module !== null) {
            module.close()
        }

        if (module === null) {
            val String path = file.location.toPortableString()
            module = model.getModuleFromFile(model, file.getName(), path, Charset.defaultCharset().name(), path)
        }
        return module
    }

    def IFile createFile(IFolder folder, String name, String contents) throws CoreException {
        val IFile file = folder.getFile(name)
        val File f = new File(file.location.toOSString())
        f.delete()
        file.create(new ByteArrayInputStream(contents.getBytes(Charset.defaultCharset())), true, null)
        return file
    }

    def void deleteModule(IErlModule module) throws CoreException {
        module.dispose()

        val String scannerName = module.getScannerName()
        val IFile file = module.getResource() as IFile
        if (file !== null && file.exists()) {
            file.delete(true, null)
        }
        val IPath stateDir = new Path(ErlangEngine.getInstance().getStateDir())

        // FIXME this code should not know about caches!
        val cacheExts = #[".noparse", ".refs", ".scan"]
        for (String ext : cacheExts) {
            val IPath p = stateDir.append(scannerName + ext)
            val File f = new File(p.toOSString())
            if (f.exists()) {
                f.delete()
            }
        }
    }

    def IErlProject createProject(String name, IPath path) throws CoreException {
        val project2 = getProject(name)
        if (project2.exists()) {
            project2.delete(true, null)
        }
        val erlProject = model.newProject(name, path.toPortableString())
        erlProject.builderProperties.setBuilderTool(BuilderTool.INTERNAL)

        val project = erlProject.workspaceProject
        val prefs = erlProject.properties

        val srcDirs = new ArrayList<IPath>()
        srcDirs.add(new Path("src"))
        prefs.setSourceDirs(srcDirs)
        buildPaths(project, srcDirs)

        val includeDirs = new ArrayList<IPath>()
        includeDirs.add(new Path("include"))
        buildPaths(project, includeDirs)
        prefs.setIncludeDirs(includeDirs)

        val ebinDir = new Path("ebin")
        buildPaths(project, Lists.newArrayList(ebinDir))
        prefs.outputDir = ebinDir

        erlProject.open(null)
        return erlProject
    }

    def IErlProject getExistingProject(String name) {
        val project = workspaceRoot.getProject(name)
        return model.getErlangProject(project)
    }

    def IPath getTmpPath(String fileName) {
        val String tmpdir = System.getProperty("java.io.tmpdir")
        return new Path(tmpdir).append(fileName)
    }

    def URI getTmpURIPath(String fileName) {
        return URIUtil.toURI(getTmpPath(fileName).toPortableString())
    }

    def File createTmpFile(String fileName, String contentString) throws IOException, FileNotFoundException {
        val String pathString = getTmpPath(fileName).toOSString()
        val File f = new File(pathString)
        if (f.exists()) {
            f.delete()
        }
        f.createNewFile()
        val FileOutputStream fileOutputStream = new FileOutputStream(pathString)
        fileOutputStream.write(contentString.getBytes())
        fileOutputStream.close()
        return f
    }

    def void deleteProject(IErlProject erlProject) throws CoreException {
        val IProject project = erlProject.getWorkspaceProject()
        val IPath location = project.getLocation()
        try {
            project.delete(true, null)
        } catch (Exception e) {
            ErlLogger.error(e)
        }
        if (location !== null) {
            new File(location.toPortableString()).delete()
        }
        erlProject.dispose()
        model.resourceChanged(null)
        model.open(null)
    }

    def IErlModule createModuleFromText(String name, String initialText) {
        val IErlModule module = model.getModuleFromText(model, name, initialText, name)
        return module
    }

    def IErlProject createTmpErlProject(String projectName) throws CoreException {
        return createProject(projectName, getTmpPath(projectName))
    }

    def IErlElement createErlAttribute(IErlElement parent, String name, OtpErlangObject value, String extra,
        int sourceRangeOffset, int sourceRangeLength) {
        val ErlAttribute attribute = new ErlAttribute(parent, name, value, extra)
        attribute.sourceRangeOffset = sourceRangeOffset
        attribute.sourceRangeLength = sourceRangeLength
        return attribute
    }

}
