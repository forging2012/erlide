package org.erlide.engine.model.erlang;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.util.List;

import org.eclipse.handly.junit.WorkspaceTest;
import org.erlide.engine.internal.model.root.ErlProject;
import org.erlide.engine.model.root.ErlElementKind;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.model.root.IErlExternal;
import org.erlide.engine.model.root.IErlProject;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class ErlExternalTests extends WorkspaceTest {

    private static final String XX_ERLIDEX = "xx.erlidex";
    private File externalFile;
    private File externalsFile;
    private String externalModulesString;
    private IErlProject project;

    @Before
    public void setup() throws Exception {
        setUpProject("testproject1");
        project = getErlProject("testproject1");
    }

    @Before
    public void set_up() throws Exception {
        externalModulesString = project.getProperties().getExternalModules();
        final String externalFileName = "external.erl";
        externalFile = createTmpFile(externalFileName,
                "-module(external).\nf([_ | _]=L ->\n    atom_to_list(L).\n");
        final String absolutePath = externalFile.getAbsolutePath();
        externalsFile = createTmpFile(XX_ERLIDEX, absolutePath);
        ((ErlProject) project).setExternalModulesFile(externalsFile.getAbsolutePath());
        project.open(null);
    }

    @After
    public void tear_down() throws Exception {
        if (externalFile != null && externalFile.exists()) {
            externalFile.delete();
        }
        if (externalsFile != null && externalsFile.exists()) {
            externalsFile.delete();
        }
        ((ErlProject) project).setExternalModulesFile(externalModulesString);
    }

    // boolean isOTP();
    @Test
    public void isOTP() throws Exception {
        final List<IErlElement> externals = project
                .getChildrenOfKind(ErlElementKind.EXTERNAL_ROOT);
        final IErlExternal external = (IErlExternal) externals.get(0);
        external.open(null);
        assertFalse(external.isOTP());

        final IErlExternal external2 = (IErlExternal) externals.get(1);
        external2.open(null);
        assertTrue(external2.isOTP());

        final IErlExternal external3 = (IErlExternal) external.getChildren().get(0);
        assertFalse(external3.isOTP());

        final IErlExternal external4 = (IErlExternal) external2.getChildren().get(0);
        assertTrue(external4.isOTP());
    }

    // boolean hasIncludes();
    @Test
    public void hasIncludes() throws Exception {
        final List<IErlElement> externals = project
                .getChildrenOfKind(ErlElementKind.EXTERNAL_ROOT);
        final IErlExternal external = (IErlExternal) externals.get(0);
        external.open(null);
        final IErlExternal externalOTP = (IErlExternal) externals.get(1);
        externalOTP.open(null);
        final IErlExternal externalDialyzer = (IErlExternal) externalOTP
                .getChildNamed("dialyzer");
        assertTrue(external.hasIncludes());
        assertTrue(externalOTP.hasIncludes());
        assertFalse(externalDialyzer.hasIncludes());
    }

}
