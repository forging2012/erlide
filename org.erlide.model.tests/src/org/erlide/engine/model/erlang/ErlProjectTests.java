package org.erlide.engine.model.erlang;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.util.Collection;
import java.util.List;
import java.util.Set;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.erlide.engine.internal.model.root.ErlProject;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.runtime.runtimeinfo.RuntimeInfo;
import org.erlide.runtime.runtimeinfo.RuntimeVersion;
import org.erlide.util.SystemConfiguration;
import org.junit.Test;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;

public class ErlProjectTests extends ErlModelTestBase {

    private static final String XX_ERLIDEX = "xx.erlidex";

    // Collection<IErlModule> getModules() throws ErlModelException;
    @Test
    public void getModules() throws Exception {
        final Collection<IErlModule> expected = project.getModules();
        createInclude(project, "bb.erl", "-module(bb).\n");
        createModule(project, "cc.hrl", "-define(A, hej).\n");
        createInclude(project, "dd.hrl", "-define(B, du).\n");
        project.open(null);
        final Collection<IErlModule> modules = project.getModules();
        assertEquals(expected, modules);
    }

    // FIXME write tests that gives exceptions!

    // Collection<IErlModule> getIncludes() throws ErlModelException;
    @Test
    public void getIncludes() throws Exception {
        createModule(projects[1], "aa.erl", "-module(aa).\n");
        createInclude(projects[1], "bb.erl", "-module(bb).\n");
        createModule(projects[1], "cc.hrl", "-define(A, hej).\n");
        final IErlModule includeDD = createInclude(projects[1], "dd.hrl",
                "-define(B, du).\n");
        final List<IErlModule> expected = Lists.newArrayList(includeDD);
        final Collection<IErlModule> includes = projects[1].getIncludes();
        assertEquals(expected, includes);
    }

    // Collection<IErlModule> getModulesAndIncludes() throws ErlModelException;
    @Test
    public void getModulesAndIncludes() throws Exception {
        final Collection<IErlModule> original = projects[1].getModules();
        createInclude(projects[1], "bb.erl", "-module(bb).\n");
        createModule(projects[1], "cc.hrl", "-define(A, hej).\n");
        final IErlModule includeD = createInclude(projects[1], "dd.hrl",
                "-define(B, du).\n");
        final List<IErlModule> expected = Lists.newArrayList(original);
        expected.add(includeD);
        final Collection<IErlModule> includes = projects[1].getModulesAndIncludes();
        assertEquals(expected, includes);
    }

    // Collection<IErlModule> getExternalModules() throws ErlModelException;
    // void setExternalModulesFile(String absolutePath)
    // throws BackingStoreException;
    @Test
    public void getExternalModules() throws Exception {
        File externalFile = null;
        File externalsFile = null;
        final IErlProject aProject = projects[1];
        final String externalModulesString = aProject.getProperties()
                .getExternalModules();
        try {
            // given
            // an erlang project and an external file not in any project
            final String externalFileName = "external.erl";
            externalFile = createTmpFile(externalFileName,
                    "-module(external).\nf([_ | _]=L ->\n    atom_to_list(L).\n");
            final String absolutePath = externalFile.getAbsolutePath();
            externalsFile = createTmpFile(XX_ERLIDEX, absolutePath);
            aProject.open(null);
            final Collection<IErlModule> otpModules = aProject.getExternalModules();
            ((ErlProject) aProject).setExternalModulesFile(externalsFile
                    .getAbsolutePath());
            aProject.open(null);
            // when
            // fetching all external modules
            final Collection<IErlModule> externalModules = aProject.getExternalModules();
            // then
            // the external file should be returned
            final Set<IErlModule> otpSet = Sets.newHashSet(otpModules);
            final Set<IErlModule> externalSet = Sets.newHashSet(externalModules);
            final Set<IErlModule> difference = Sets.difference(externalSet, otpSet);
            assertEquals(1, difference.size());
            final IErlModule externalModule = difference.iterator().next();
            assertNotNull(externalModule);
            if (SystemConfiguration.getInstance().isOnWindows()) {
                assertEquals(new Path(absolutePath.toLowerCase()), new Path(
                        externalModule.getFilePath().toLowerCase()));
            } else {
                assertEquals(new Path(absolutePath),
                        new Path(externalModule.getFilePath()));
            }
        } finally {
            if (externalFile != null && externalFile.exists()) {
                externalFile.delete();
            }
            if (externalsFile != null && externalsFile.exists()) {
                externalsFile.delete();
            }
            ((ErlProject) aProject).setExternalModulesFile(externalModulesString);
        }
    }

    // Collection<IErlModule> getExternalIncludes() throws ErlModelException;
    // void setExternalIncludesFile(String absolutePath)
    // throws BackingStoreException;
    @Test
    public void getExternalIncludes() throws Exception {
        File externalFile = null;
        File externalsFile = null;
        final IErlProject aProject = projects[1];
        final String externalIncludesString = aProject.getProperties()
                .getExternalIncludes();
        try {
            // given
            // an erlang project and an external file not in any project
            final String externalFileName = "external.hrl";
            externalFile = createTmpFile(externalFileName, "-define(E, hej).\n");
            final String absolutePath = externalFile.getAbsolutePath();
            final String externalsFileName = XX_ERLIDEX;
            externalsFile = createTmpFile(externalsFileName, absolutePath);
            aProject.open(null);
            final Collection<IErlModule> otpIncludes = aProject.getExternalIncludes();
            ((ErlProject) aProject).setExternalIncludesFile(externalsFile
                    .getAbsolutePath());
            aProject.open(null);
            // when
            // fetching all external includes
            final Collection<IErlModule> externalIncludes = aProject
                    .getExternalIncludes();
            // then
            // the external file should be returned
            final Set<IErlModule> otpSet = Sets.newHashSet(otpIncludes);
            final Set<IErlModule> externalSet = Sets.newHashSet(externalIncludes);
            final Set<IErlModule> difference = Sets.difference(externalSet, otpSet);
            assertEquals(1, difference.size());
            final IErlModule externalInclude = difference.iterator().next();
            assertNotNull(externalInclude);
            if (SystemConfiguration.getInstance().isOnWindows()) {
                assertEquals(new Path(absolutePath.toLowerCase()), new Path(
                        externalInclude.getFilePath().toLowerCase()));
            } else {
                assertEquals(new Path(absolutePath),
                        new Path(externalInclude.getFilePath()));
            }
        } finally {
            if (externalFile != null && externalFile.exists()) {
                externalFile.delete();
            }
            if (externalsFile != null && externalsFile.exists()) {
                externalsFile.delete();
            }
            ((ErlProject) aProject).setExternalIncludesFile(externalIncludesString);
        }
    }

    @Test
    public void getExternalIncludes_includeDirs() throws Exception {
        File externalFile = null;
        final IErlProject aProject = projects[1];
        final Collection<IPath> includeDirs = aProject.getProperties().getIncludeDirs();
        try {
            // given
            // an erlang project and an external file not in any project, but on
            // the include-path
            final String externalFileName = "external.hrl";
            externalFile = createTmpFile(externalFileName, "-define(E, hej).\n");
            final String absolutePath = externalFile.getAbsolutePath();
            final List<IPath> newIncludeDirs = Lists.newArrayList(includeDirs);
            aProject.open(null);
            final Collection<IErlModule> otpIncludes = aProject.getExternalIncludes();
            final IPath absoluteDir = new Path(absolutePath).removeLastSegments(1);
            newIncludeDirs.add(absoluteDir);
            ((ErlProject) aProject).setIncludeDirs(newIncludeDirs);
            aProject.open(null);
            // when
            // fetching all external includes
            final Collection<IErlModule> externalIncludes = aProject
                    .getExternalIncludes();
            // then
            // the external file should be returned
            final Set<IErlModule> otpSet = Sets.newHashSet(otpIncludes);
            final Set<IErlModule> externalSet = Sets.newHashSet(externalIncludes);
            final Set<IErlModule> difference = Sets.difference(externalSet, otpSet);
            final IErlModule externalInclude = difference.iterator().next();
            assertNotNull(externalInclude);
            if (SystemConfiguration.getInstance().isOnWindows()) {
                assertEquals(new Path(absolutePath.toLowerCase()), new Path(
                        externalInclude.getFilePath().toLowerCase()));
            } else {
                assertEquals(new Path(absolutePath),
                        new Path(externalInclude.getFilePath()));
            }
        } finally {
            if (externalFile != null && externalFile.exists()) {
                externalFile.delete();
            }
            ((ErlProject) aProject).setIncludeDirs(includeDirs);
        }
    }

    // String getExternalModulesString();
    @Test
    public void getExternalModulesString() throws Exception {
        final IErlProject aProject = projects[1];
        final String externalIncludesString = aProject.getProperties()
                .getExternalIncludes();
        try {
            final String s = "/hej";
            ((ErlProject) aProject).setExternalModulesFile(s);
            assertEquals(s, aProject.getProperties().getExternalModules());
        } finally {
            ((ErlProject) aProject).setExternalModulesFile(externalIncludesString);
        }
    }

    // String getExternalIncludesString();
    @Test
    public void getExternalIncludesString() throws Exception {
        final IErlProject aProject = projects[1];
        final String externalIncludesString = aProject.getProperties()
                .getExternalIncludes();
        try {
            final String s = "/tjo";
            ((ErlProject) aProject).setExternalIncludesFile(s);
            assertEquals(s, aProject.getProperties().getExternalIncludes());
        } finally {
            ((ErlProject) aProject).setExternalIncludesFile(externalIncludesString);
        }
    }

    // void setIncludeDirs(Collection<IPath> includeDirs)
    // throws BackingStoreException;
    @Test
    public void setIncludeDirs() throws Exception {
        File externalFile = null;
        final IErlProject aProject = projects[1];
        final Collection<IPath> includeDirs = aProject.getProperties().getIncludeDirs();
        try {
            // given
            // an erlang project and an external file not in any project
            final String externalFileName = "external.hrl";
            externalFile = createTmpFile(externalFileName, "-define(E, hej).\n");
            final String absolutePath = externalFile.getAbsolutePath();
            final List<IPath> newIncludeDirs = Lists.newArrayList(includeDirs);
            aProject.open(null);
            final Collection<IErlModule> otpIncludes = aProject.getExternalIncludes();
            final IPath absoluteDir = new Path(absolutePath).removeLastSegments(1);
            newIncludeDirs.add(absoluteDir);
            ((ErlProject) aProject).setIncludeDirs(newIncludeDirs);
            aProject.open(null);
            // when
            // fetching all external includes
            final Collection<IErlModule> externalIncludes = aProject
                    .getExternalIncludes();
            // then
            // the external file should be returned
            final Set<IErlModule> otpSet = Sets.newHashSet(otpIncludes);
            final Set<IErlModule> externalSet = Sets.newHashSet(externalIncludes);
            final Set<IErlModule> difference = Sets.difference(externalSet, otpSet);
            assertEquals(1, difference.size());
            final IErlModule externalInclude = difference.iterator().next();
            assertNotNull(externalInclude);
            assertEquals(new Path(absolutePath), new Path(externalInclude.getFilePath()));
        } finally {
            if (externalFile != null && externalFile.exists()) {
                externalFile.delete();
            }
            ((ErlProject) aProject).setIncludeDirs(includeDirs);
        }
    }

    // void setSourceDirs(Collection<IPath> sourceDirs)
    // throws BackingStoreException;
    @Test
    public void setSourceDirs() throws Exception {
        final IErlProject aProject = project;
        final Collection<IPath> sourceDirs = aProject.getProperties().getSourceDirs();
        try {
            // given
            // an Erlang project and a module
            final IPath srcxPath = new Path("srcx");
            final List<IPath> srcxDirs = Lists.newArrayList(srcxPath);
            aProject.open(null);
            // when
            // setting source dirs so the module is on source path
            final Collection<IErlModule> modules = aProject.getModules();
            ((ErlProject) aProject).setSourceDirs(srcxDirs);
            aProject.open(null);
            final Collection<IErlModule> srcxModules = aProject.getModules();
            ((ErlProject) aProject).setSourceDirs(sourceDirs);
            aProject.open(null);
            final Collection<IErlModule> modulesAgain = aProject.getModules();
            // then
            // the it should be returned, but not otherwise
            assertEquals(0, srcxModules.size());
            assertTrue(modules.size() > 0);
            assertEquals(module, modules.iterator().next());
            assertTrue(modulesAgain.size() > 0);
            assertEquals(module, modulesAgain.iterator().next());
        } finally {
            ((ErlProject) aProject).setSourceDirs(sourceDirs);
        }
    }

    // Collection<IPath> getSourceDirs();
    @Test
    public void getSourceDirs() throws Exception {
        final Collection<IPath> sourceDirs = projects[1].getProperties().getSourceDirs();
        assertEquals(1, sourceDirs.size());
        final IPath path = new Path("src");
        assertEquals(path, sourceDirs.iterator().next());
    }

    // Collection<IPath> getIncludeDirs();
    @Test
    public void getIncludeDirs() throws Exception {
        final Collection<IPath> includeDirs = project.getProperties().getIncludeDirs();
        assertEquals(1, includeDirs.size());
        final IPath path = new Path("include");
        assertEquals(path, includeDirs.iterator().next());
    }

    // IPath getOutputLocation();
    @Test
    public void getOutputLocation() throws Exception {
        final IPath outputLocation = projects[1].getProperties().getOutputDir();
        assertEquals(new Path("ebin"), outputLocation);
    }

    // RuntimeInfo getRuntimeInfo();
    @Test
    public void getRuntimeInfo() throws Exception {
        final IErlProject aProject = projects[1];
        final RuntimeInfo info = aProject.getRuntimeInfo();
        // final String expected = ResourcesPlugin.getWorkspace().getRoot()
        // .getLocation().toString();
        assertNotNull(info);
        // The working dir might be relative to the project and can also be "."
        // We need to convert it to a canonical absolute path in order to be
        // able to compare it with a value.
        // This is not very portable across OSs
    }

    // RuntimeVersion getRuntimeVersion();
    @Test
    public void getRuntimeVersion() throws Exception {
        final IErlProject aProject = projects[1];
        final RuntimeVersion version = aProject.getRuntimeVersion();
        assertNotNull(version);
        final int majorVersion = version.getMajor();
        assertTrue(majorVersion >= 12);
    }

    // TODO check more properties than source dirs property
    @Test
    public void setProperties() throws Exception {
        final IErlProject aProject = projects[1];
        final Collection<IPath> sourceDirs = aProject.getProperties().getSourceDirs();
        try {
            final ErlangProjectProperties properties = aProject.getProperties();
            final IPath srcx = new Path("srcx");
            properties.setSourceDirs(Lists.newArrayList(srcx));
            aProject.setProperties(properties);
            final Collection<IPath> sourceDirs2 = aProject.getProperties()
                    .getSourceDirs();
            assertEquals(1, sourceDirs2.size());
            assertEquals(srcx, sourceDirs2.iterator().next());
        } finally {
            ((ErlProject) aProject).setSourceDirs(sourceDirs);
        }
    }

    @Test
    public void getReferencedProjects() throws Exception {
        final IProject aProject = projects[0].getWorkspaceProject();
        final IProjectDescription description = aProject.getDescription();
        final IProject[] refs = new IProject[] { projects[1].getWorkspaceProject() };
        try {
            description.setReferencedProjects(refs);
            aProject.setDescription(description, null);
            final List<IErlProject> expected = Lists.newArrayList(projects[1]);
            assertEquals(expected, projects[0].getReferencedProjects());
        } finally {
            description.setReferencedProjects(new IProject[0]);
            aProject.setDescription(description, null);
        }
    }

    public void getProjectReferences_closedProject() throws Exception {
        final IErlProject erlProject = projects[1];
        final IProject aProject = erlProject.getWorkspaceProject();
        try {
            aProject.close(null);
            erlProject.getReferencedProjects();
        } finally {
            if (!aProject.isOpen()) {
                aProject.open(null);
            }
        }
    }

    // IErlModule getModule(String name) throws ErlModelException;
    @Test
    public void getModule() throws Exception {
        final IErlProject aProject = projects[1];
        final Collection<IPath> sourceDirs = aProject.getProperties().getSourceDirs();
        try {
            // given
            // an Erlang project and a module
            final IErlModule aModule = createModule(aProject, "aa.erl", "-module(aa).\n");
            final IPath srcxPath = new Path("srcx");
            final List<IPath> srcxDirs = Lists.newArrayList(srcxPath);
            aProject.open(null);
            // when
            // setting source dirs so the module is on source path
            final IErlModule module2 = aProject.getModule("aa");
            final IErlModule nullModule = aProject.getModule("aa.hrl");
            final IErlModule nullModule2 = aProject.getModule("AA");
            final IErlModule nullModule3 = aProject.getModule("aA");
            final IErlModule nullModule4 = aProject.getModule("AA.erl");
            final IErlModule module4 = aProject.getModule("aa.erl");
            ((ErlProject) aProject).setSourceDirs(srcxDirs);
            aProject.open(null);
            final IErlModule srcxModule = aProject.getModule("aa");
            ((ErlProject) aProject).setSourceDirs(sourceDirs);
            aProject.open(null);
            final IErlModule module3 = aProject.getModule("aa");
            // then
            // the it should be returned, but not otherwise
            assertEquals(aModule, module2);
            assertNull(srcxModule);
            assertNull(nullModule);
            assertNull(nullModule2);
            assertNull(nullModule3);
            assertNull(nullModule4);
            assertEquals(aModule, module3);
            assertEquals(aModule, module4);
        } finally {
            ((ErlProject) aProject).setSourceDirs(sourceDirs);
        }
    }

    // IProject getWorkspaceProject();
    @Test
    public void getWorkspaceProject() throws Exception {
        final IErlProject aProject = projects[1];
        final IProject workspaceProject = aProject.getWorkspaceProject();
        assertNotNull(workspaceProject);
        assertEquals(aProject.getName(), workspaceProject.getName());
    }
}
