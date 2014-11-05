package org.erlide.engine.model.erlang.configuration;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.erlide.engine.internal.model.root.ErlProject;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.testing.utils.WorkspaceTest;
import org.erlide.util.FileUtils;
import org.erlide.util.Util;
import org.junit.Before;

import com.google.common.base.Charsets;

public abstract class AbstractProjectConfigurationTests extends WorkspaceTest {

    protected IErlProject project;

    @Before
    public void setup() throws Exception {
        final String name = "testproject3";
        project = createProject(name, getTmpPath(name));
        final ErlProject p = (ErlProject) project;
        p.configurationChanged();
    }

    protected void setFileContent(final String name, final String content)
            throws CoreException {
        final IProject wproject = project.getWorkspaceProject();
        FileUtils.createFileInProjectAt(wproject, name, content, Charsets.ISO_8859_1);
    }

    protected String getFileContent(final String name) throws CoreException {
        final IProject wproject = project.getWorkspaceProject();
        final IFile res = wproject.getFile(name);
        if (!res.exists()) {
            return "";
        }
        return Util.getInputStreamAsString(res.getContents(), Charsets.ISO_8859_1.name());
    }

    public abstract void configCanBeParsed() throws CoreException;

}
