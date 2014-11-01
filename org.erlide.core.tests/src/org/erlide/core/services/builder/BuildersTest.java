package org.erlide.core.services.builder;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;

import java.io.IOException;
import java.io.StringBufferInputStream;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceDescription;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.jobs.IJobManager;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.handly.junit.WorkspaceTest;
import org.erlide.core.internal.builder.BuildNotifier;
import org.erlide.core.internal.builder.ErlangBuilder;
import org.erlide.core.internal.builder.ErlangBuilder.BuildKind;
import org.erlide.core.internal.builder.ErlangBuilderFactory;
import org.erlide.core.internal.builder.ErlangNature;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.builder.BuilderTool;
import org.erlide.engine.model.root.IErlProject;
import org.junit.After;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

@SuppressWarnings("deprecation")
public class BuildersTest extends WorkspaceTest {

    private IProject prj;

    public class LocalProgressMonitor extends NullProgressMonitor {
        private final boolean[] output;

        public LocalProgressMonitor(final boolean[] output) {
            this.output = output;
            output[0] = false;
        }

        @Override
        public void worked(final int work) {
            System.out.println("WORKED!!");
            output[0] = true;
        }

    }

    @Before
    public void initialClean() throws CoreException, IOException {
        setUpProject("builders");

        final IErlProject p2 = getExistingProject("builders");
        prj = p2.getResource().getProject();

        final IResource ebin = prj.findMember("ebin");
        if (ebin != null) {
            ebin.delete(true, null);
        } else {
            prj.getFolder("ebin").create(true, true, null);
        }
    }

    @After
    public void restore() {
        prj = null;
    }

    @Test
    public void internalBuilderShouldWork() throws CoreException {
        testBuilder(BuilderTool.INTERNAL);
        testClean(BuilderTool.INTERNAL);
    }

    @Test
    public void makeBuilderShouldWork() throws CoreException {
        ensureNoAppSrcExists();
        final IFolder folder = (IFolder) prj.findMember("src");
        final IFile app = folder.getFile("builders.app.src");
        app.create(
                new StringBufferInputStream("{application, builders,[{vsn, \"1\"}]}."),
                true, null);
        try {
            testBuilder(BuilderTool.MAKE);
            testClean(BuilderTool.MAKE);
        } finally {
            app.delete(true, null);
        }
    }

    @Test
    public void emakeBuilderShouldWork() throws CoreException {
        ensureNoAppSrcExists();
        testBuilder(BuilderTool.EMAKE);
        testClean(BuilderTool.EMAKE);
    }

    @Test
    public void rebarBuilderShouldWork() throws CoreException {
        ensureNoAppSrcExists();
        final IFolder folder = (IFolder) prj.findMember("src");
        final IFile app = folder.getFile("builders.app.src");
        app.create(
                new StringBufferInputStream("{application, builders,[{vsn, \"1\"}]}."),
                true, null);
        try {
            testBuilder(BuilderTool.REBAR);

            final IResource beam = prj.findMember("ebin/builders.app");
            assertThat("app was not created", beam, notNullValue());

            testClean(BuilderTool.REBAR);
        } finally {
            app.delete(true, null);
        }
    }

    private void ensureNoAppSrcExists() {
        final IFolder folder = (IFolder) prj.findMember("src");
        try {
            final IResource[] srcs = folder.members();
            for (final IResource f : srcs) {
                assertThat("An .app.src exists: " + f.getName(),
                        !f.getName().endsWith(".app.src"));
            }
        } catch (final CoreException e) {
        }
    }

    @Test(expected = AssertionError.class)
    public void rebarBuilderShouldNotWorkWithoutAppFile() throws CoreException {
        ensureNoAppSrcExists();
        testBuilder(BuilderTool.REBAR);
    }

    private static void setAutoBuild(final IWorkspace workspace, final boolean enabled)
            throws CoreException {
        final IWorkspaceDescription def = workspace.getDescription();
        def.setAutoBuilding(enabled);
        workspace.setDescription(def);
    }

    private void testBuilder(final BuilderTool builderTool) throws CoreException {
        final String targetBeamPath = "ebin/mod.beam";

        final IResource beam0 = prj.findMember(targetBeamPath);
        assertThat("beam existed before test", beam0, nullValue());

        final ErlangBuilder builder = ErlangBuilderFactory.get(builderTool);
        final BuildNotifier notifier = new BuildNotifier(null, prj);
        final IErlProject erlProject = ErlangEngine.getInstance().getModel()
                .getErlangProject(prj);

        builder.build(BuildKind.FULL, erlProject, notifier);
        prj.refreshLocal(IResource.DEPTH_INFINITE, null);
        waitJobsToFinish(ResourcesPlugin.FAMILY_MANUAL_REFRESH);

        final IResource beam = prj.findMember(targetBeamPath);
        assertThat("beam was not created", beam, notNullValue());
    }

    private void testClean(final BuilderTool builderTool) throws CoreException {
        final ErlangBuilder builder = ErlangBuilderFactory.get(builderTool);
        final BuildNotifier notifier = new BuildNotifier(null, prj);
        final IErlProject erlProject = ErlangEngine.getInstance().getModel()
                .getErlangProject(prj);

        builder.clean(erlProject, notifier);
        prj.refreshLocal(IResource.DEPTH_INFINITE, null);
        waitJobsToFinish(ResourcesPlugin.FAMILY_MANUAL_REFRESH);

        final String targetBeamPath = "ebin/mod.beam";
        final IResource beam2 = prj.findMember(targetBeamPath);
        assertThat("beam was not removed", beam2, nullValue());
    }

    private void waitJobsToFinish(final Object family) {
        final IJobManager jobMan = Job.getJobManager();
        final Job[] build = jobMan.find(family);
        while (build.length == 1) {
            try {
                build[0].join();
            } catch (final InterruptedException e) {
            }
        }
    }

}
