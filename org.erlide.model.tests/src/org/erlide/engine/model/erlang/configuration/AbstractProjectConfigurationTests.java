package org.erlide.engine.model.erlang.configuration;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.erlide.engine.internal.model.root.ErlProject;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.util.ErlideTestUtils;
import org.junit.After;
import org.junit.Before;

import com.google.common.base.Charsets;

public abstract class AbstractProjectConfigurationTests {

    protected IErlProject project;

    @Before
    public void setUp() throws Exception {
        ErlideTestUtils.initProjects();
        final String name = "testproject3";
        project = ErlideTestUtils.createProject(ErlideTestUtils.getTmpPath(name), name);
        final ErlProject p = (ErlProject) project;
        p.loadCoreProperties();
    }

    @After
    public void tearDown() throws Exception {
        ErlideTestUtils.deleteProjects();
    }

    protected void setFileContent(final String name, final String content)
            throws CoreException {
        final IProject wproject = project.getWorkspaceProject();
        ErlideTestUtils.createFileInProjectAt(wproject, name, content,
                Charsets.ISO_8859_1);
    }

    protected String getFileContent(final String name) throws CoreException {
        final IProject wproject = project.getWorkspaceProject();
        final IFile res = wproject.getFile(name);
        if (!res.exists()) {
            return "";
        }
        return ErlideTestUtils.convertStreamToString(res.getContents(),
                Charsets.ISO_8859_1);
    }

    public abstract void configCanBeParsed() throws CoreException;

}
