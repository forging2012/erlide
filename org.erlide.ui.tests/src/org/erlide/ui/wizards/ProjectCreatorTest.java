package org.erlide.ui.wizards;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.contains;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.nullValue;

import java.net.URI;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.operation.IRunnableContext;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.builder.BuilderTool;
import org.erlide.engine.model.root.ErlangProjectProperties;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.model.root.IProjectConfiguratorFactory;
import org.erlide.engine.model.root.NewProjectData;
import org.erlide.engine.model.root.ProjectConfigType;
import org.erlide.ui.tests.util.DummyRunnableContext;
import org.erlide.ui.tests.util.WorkbenchHelper;
import org.erlide.util.Util;
import org.junit.After;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import com.google.common.base.Charsets;
import com.google.common.collect.Lists;

public class ProjectCreatorTest {

    private ProjectCreator creator;
    private static URI location;
    private static IRunnableContext context;
    private static String name;
    private IProject prj = null;
    static IProjectConfiguratorFactory factory;

    @BeforeClass
    public static void init() {
        name = "demo111";
        location = null;
        context = new DummyRunnableContext();
        factory = ErlangEngine.getInstance().getProjectConfiguratorFactory();

        WorkbenchHelper.waitForWorkbench();
    }

    @Before
    public void setup() throws CoreException {
        if (prj != null) {
            if (prj.exists()) {
                prj.delete(true, null);
            }
            if (prj.exists()) {
                throw new IllegalArgumentException();
            }
            prj = null;
        }
    }

    @After
    public void teardown() throws CoreException {
        final IWorkspace w = ResourcesPlugin.getWorkspace();
        final IProject p = w.getRoot().getProject(name);
        if (p.exists()) {
            p.delete(true, null);
        }
    }

    @Test
    public void createSimpleProject() throws CoreException {
        final NewProjectData info = new NewProjectData(factory);
        info.copyFrom(ErlangProjectProperties.DEFAULT);
        creator = new ProjectCreator(name, location, new IProject[] {}, info, context,
                null);
        prj = creator.createProject();
        assertThat(prj, is(not(nullValue())));

        final IErlProject erlPrj = ErlangEngine.getInstance().getModel().findProject(prj);
        assertThat(erlPrj, is(not(nullValue())));

        final ErlangProjectProperties props = erlPrj.getProperties();
        assertThat(props.getOutputDir(), is((IPath) new Path("ebin")));

    }

    @Test(expected = CoreException.class)
    public void createExistingProjectShouldCrash() throws CoreException {
        final NewProjectData info = new NewProjectData(factory);
        info.copyFrom(ErlangProjectProperties.DEFAULT);
        creator = new ProjectCreator(name, location, new IProject[] {}, info, context,
                null);
        prj = creator.createProject();
        prj = creator.createProject();
    }

    @Test
    public void createRebarProject() throws CoreException {
        final NewProjectData info = new NewProjectData(factory);
        info.copyFrom(ErlangProjectProperties.DEFAULT);
        info.setConfigType(ProjectConfigType.REBAR);
        info.setBuilder(BuilderTool.REBAR);
        final List<IPath> src = Lists.newArrayList(info.getSourceDirs());
        src.add(new Path("foz"));
        info.setSourceDirs(src);

        creator = new ProjectCreator(name, location, new IProject[] {}, info, context,
                null);
        prj = creator.createProject();
        assertThat(prj, is(not(nullValue())));

        final IErlProject erlPrj = ErlangEngine.getInstance().getModel().findProject(prj);
        assertThat(erlPrj, is(not(nullValue())));

        final ErlangProjectProperties props = erlPrj.getProperties();
        assertThat(props.getSourceDirs(),
                contains((IPath) new Path("src"), (IPath) new Path("foz")));

        final IResource r = prj.findMember(ProjectConfigType.REBAR.getConfigName());
        assertThat(r, is(not(nullValue())));
        assertThat("config file doesn't exist", r.exists());
        final IFile f = (IFile) r;
        final String str = Util.getInputStreamAsString(f.getContents(),
                Charsets.ISO_8859_1.name());
        assertThat(str, is(""));
    }
}
