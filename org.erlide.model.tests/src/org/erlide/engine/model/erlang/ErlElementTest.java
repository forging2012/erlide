package org.erlide.engine.model.erlang;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.EnumSet;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IMarkerDelta;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceDelta;
import org.eclipse.core.resources.IResourceDeltaVisitor;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.handly.junit.WorkspaceTest;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.internal.model.erlang.ErlAttribute;
import org.erlide.engine.internal.model.root.ErlProject;
import org.erlide.engine.model.ErlModelException;
import org.erlide.engine.model.IErlModel;
import org.erlide.engine.model.root.ErlElementKind;
import org.erlide.engine.model.root.IErlElement;
import org.erlide.engine.model.root.IErlElement.AcceptFlags;
import org.erlide.engine.model.root.IErlElementLocator;
import org.erlide.engine.model.root.IErlElementVisitor;
import org.erlide.engine.model.root.IErlExternal;
import org.erlide.engine.model.root.IErlFolder;
import org.erlide.engine.model.root.IErlProject;
import org.junit.Before;
import org.junit.Test;

import com.google.common.collect.Lists;

public class ErlElementTest extends WorkspaceTest {

    private IErlModule module;
    private IErlProject project;

    @Before
    public void setup() throws Exception {
        setUpProject("testproject1");
        project = getErlProject("testproject1");
        module = getErlModule("xx.erl");
    }

    // boolean exists();
    @Test
    public void exists() throws Exception {
        final boolean exists = module.exists();
        module.getResource().delete(true, null);
        final boolean exists2 = module.exists();
        assertTrue(exists);
        assertFalse(exists2);
    }

    // IErlElement getAncestorOfKind(Kind kind);
    @Test
    public void getAncestorOfKind() throws Exception {
        module.open(null);
        final IErlElement element = module.getElementAtLine(3);
        final IErlElement ancestor = element.getAncestorOfKind(ErlElementKind.FUNCTION);
        final IErlElement ancestor2 = element.getAncestorOfKind(ErlElementKind.MODULE);
        final IErlElement ancestor3 = element.getAncestorOfKind(ErlElementKind.FOLDER);
        final IErlElement ancestor4 = element.getAncestorOfKind(ErlElementKind.PROJECT);
        final IErlElement ancestor5 = element.getAncestorOfKind(ErlElementKind.MODEL);
        final IErlElement ancestor6 = element.getAncestorOfKind(ErlElementKind.TYPESPEC);
        assertNotNull(ancestor);
        assertTrue(ancestor instanceof IErlFunction);
        assertEquals(ErlElementKind.FUNCTION, ancestor.getKind());
        assertEquals(element, ancestor);
        assertEquals(ErlElementKind.MODULE, ancestor2.getKind());
        assertEquals(ErlElementKind.FOLDER, ancestor3.getKind());
        assertEquals(ErlElementKind.PROJECT, ancestor4.getKind());
        assertEquals(ErlElementKind.MODEL, ancestor5.getKind());
        assertEquals(ancestor3, ancestor2.getAncestorOfKind(ErlElementKind.FOLDER));
        assertEquals(ancestor4, ancestor2.getAncestorOfKind(ErlElementKind.PROJECT));
        assertNull(ancestor6);
    }

    // IErlProject getProject();
    @Test
    public void getProject() throws Exception {
        assertEquals(project, ErlangEngine.getInstance().getModelUtilService()
                .getProject(module));
        assertEquals(project, ErlangEngine.getInstance().getModelUtilService()
                .getProject(project));
        module.open(null);
        final IErlElement element = module.getElementAtLine(3);
        assertEquals(project, ErlangEngine.getInstance().getModelUtilService()
                .getProject(element));
    }

    // IErlModule getModule();
    @Test
    public void getModule() throws Exception {
        module.open(null);
        final IErlMember element = module.getElementAtLine(3);
        assertEquals(module,
                ErlangEngine.getInstance().getModelUtilService().getModule(element));
    }

    // IResource getCorrespondingResource();
    @Test
    public void getCorrespondingResource() throws Exception {
        project.open(null);
        final IProject workspaceProject = project.getWorkspaceProject();
        final IFolder srcFolder = workspaceProject.getFolder("src");
        final IFile file = srcFolder.getFile("xx.erl");
        final IErlElementLocator model = ErlangEngine.getInstance().getModel();
        final IErlModule otpFile = model.findModuleFromProject(project, "file.erl", null,
                IErlElementLocator.Scope.PROJECT_ONLY);
        module.open(null);
        final IErlElement element = module.getElementAtLine(3);
        assertEquals(file, module.getCorrespondingResource());
        assertNull(otpFile.getCorrespondingResource());
        assertNull(element.getCorrespondingResource());
    }

    // String getName();
    @Test
    public void getName() throws Exception {
        module.open(null);
        final IErlElement element = module.getElementAtLine(3);
        final IErlElement element2 = module.getElementAtLine(0);
        assertEquals("xx.erl", module.getName());
        assertEquals("testproject1", project.getName());
        assertEquals("f", element.getName());
        assertEquals("module", element2.getName());
    }

    // Kind getKind();
    @Test
    public void getKind() throws Exception {
        module.open(null);
        final IErlElement element = module.getElementAtLine(3);
        final IErlElement element2 = module.getElementAtLine(0);
        assertEquals(ErlElementKind.MODULE, module.getKind());
        assertEquals(ErlElementKind.PROJECT, project.getKind());
        assertEquals(ErlElementKind.FUNCTION, element.getKind());
        assertEquals(ErlElementKind.ATTRIBUTE, element2.getKind());
        assertEquals(ErlElementKind.MODEL, ErlangEngine.getInstance().getModel()
                .getKind());
    }

    // IErlModel getModel();
    @Test
    public void getModelTest() throws Exception {
        module.open(null);
        final IErlElementLocator model = ErlangEngine.getInstance().getModel();
        assertEquals(model, ErlangEngine.getInstance().getModel());
    }

    // IErlElement getParent();
    @Test
    public void getParent() throws Exception {
        project.open(null);
        final IErlElement srcFolder = project.getChildNamed("src");
        module.open(null);
        final IErlElement element = module.getElementAtLine(3);
        assertEquals(project, srcFolder.getParent());
        assertEquals(srcFolder, module.getParent());
        assertEquals(module, element.getParent());
    }

    // IResource getResource();
    @Test
    public void getResource() throws Exception {
        project.open(null);
        final IProject workspaceProject = project.getWorkspaceProject();
        final IFolder srcFolder = workspaceProject.getFolder("src");
        final IFile file = srcFolder.getFile("xx.erl");
        final IErlElementLocator model = ErlangEngine.getInstance().getModel();
        final IErlModule otpFile = model.findModuleFromProject(project, "file.erl", null,
                IErlElementLocator.Scope.PROJECT_ONLY);
        module.open(null);
        final IErlElement element = module.getElementAtLine(3);
        assertEquals(file, module.getResource());
        assertNull(otpFile.getResource());
        assertEquals(module.getResource(), element.getResource());
    }

    // boolean isReadOnly();
    // Empty method

    // boolean isStructureKnown() throws ErlModelException;
    @Test
    public void isStructureKnown() throws Exception {
        ((ErlProject) project).setSourceDirs(project.getProperties().getSourceDirs());
        // this sets structureKnown to false

        final boolean structureKnown = project.isStructureKnown();
        project.open(null);
        final boolean structureKnown2 = project.isStructureKnown();
        final boolean structureKnown3 = module.isStructureKnown();
        module.open(null);
        final boolean structureKnown4 = module.isStructureKnown();
        final IErlElementLocator model = ErlangEngine.getInstance().getModel();
        final IErlModule otpFile = model.findModuleFromProject(project, "file.erl", null,
                IErlElementLocator.Scope.PROJECT_ONLY);
        final IErlExternal external = (IErlExternal) otpFile.getParent();
        final boolean structureKnown5 = external.isStructureKnown();
        final IErlModule module2 = createModule(project, "yy.erl",
                "-module(yy).\n% comment\n% same\nf(x) -> y.\n% last");
        final boolean structureKnown6 = module2.isStructureKnown();
        module2.open(null);
        final boolean structureKnown7 = module2.isStructureKnown();
        assertFalse(structureKnown);
        assertTrue(structureKnown2);
        assertFalse(structureKnown3);
        assertTrue(structureKnown4);
        assertTrue(structureKnown5);
        assertFalse(structureKnown6);
        assertTrue(structureKnown7);
    }

    // void resourceChanged(IResourceDelta delta);
    @Test
    public void resourceChanged() throws Exception {
        project.open(null);
        final boolean structureKnown = project.isStructureKnown();
        project.resourceChanged(new ResourceDeltaStub());
        final boolean structureKnown2 = project.isStructureKnown();
        assertTrue(structureKnown);
        assertFalse(structureKnown2);
    }

    // void accept(IErlElementVisitor visitor, EnumSet<AcceptFlags> flags,
    // IErlElement.Kind leafKind) throws ErlModelException;
    @Test
    public void accept() throws Exception {
        project.open(null);
        // ErlLogger.debug(project.getChildren().toString());
        module.open(null);
        final List<IErlElement> elements = Lists.newArrayList();
        final IErlElementVisitor visitor = new IErlElementVisitor() {

            @Override
            public boolean visit(final IErlElement element) throws ErlModelException {
                if (element instanceof IErlExternal) {
                    return false; // avoid digging through otp
                }
                final String name = element.getName();
                if (name.equals("ebin")) {
                    return false; // avoid possible beam-files
                } else if (name.startsWith(".")) {
                    return false; // avoid eclipse internals
                }
                elements.add(element);
                return true;
            }

        };
        final EnumSet<AcceptFlags> noneOf = EnumSet.noneOf(AcceptFlags.class);
        project.accept(visitor, noneOf, ErlElementKind.MODULE);
        final List<IErlElement> kindModuleElementsVisited = Lists.newArrayList(elements);
        elements.clear();
        project.accept(visitor, noneOf, ErlElementKind.FUNCTION);
        final List<IErlElement> kindFunctionElementsVisited = Lists
                .newArrayList(elements);
        elements.clear();
        project.accept(visitor, EnumSet.of(AcceptFlags.CHILDREN_FIRST),
                ErlElementKind.MODULE);
        final List<IErlElement> childrenFirst = Lists.newArrayList(elements);
        elements.clear();
        project.accept(visitor, EnumSet.of(AcceptFlags.LEAFS_ONLY), ErlElementKind.MODULE);
        final List<IErlElement> leafsOnly = Lists.newArrayList(elements);
        elements.clear();
        // assertEquals(4, kindModuleElementsVisited.size());
        assertEquals(project, kindModuleElementsVisited.get(0));
        // ErlLogger.debug(kindModuleElementsVisited.toString());
        assertEquals("include", kindModuleElementsVisited.get(1).getName());
        assertEquals("src", kindModuleElementsVisited.get(2).getName());
        assertEquals(module, kindModuleElementsVisited.get(3));
        assertEquals(8, kindFunctionElementsVisited.size());
        final int projectIndex = childrenFirst.indexOf(project);
        final int moduleIndex = childrenFirst.indexOf(module);
        assertTrue(moduleIndex < projectIndex);
        assertFalse(leafsOnly.contains(project));
        assertTrue(leafsOnly.contains(module));
    }

    // String getLabelString();
    // Should be removed

    // String getFilePath();
    @Test
    public void getFilePath() throws Exception {
        final String modulePath = module.getResource().getLocation().toString();
        // final String projectPath = project.getResource().getLocation()
        // .toString();
        // final String srcFolderPath = projectPath + "/src";
        final IErlElement parent = module.getParent();
        module.open(null);
        final IErlElement element = module.getElementAtLine(3);
        assertEquals(modulePath, module.getFilePath());
        assertNull(project.getFilePath());
        assertNull(parent.getFilePath());
        assertNull(element.getFilePath());
    }

    // void clearCaches();
    /**
     * @see org.erlide.engine.model.root.IErlElement#clearCaches()
     */
    // TODO check more than source dir cache
    @Test
    public void clearCaches() throws Exception {
    }

    // TODO replace ResourceDeltaStub with a mock object
    public static class ResourceDeltaStub implements IResourceDelta {
        @Override
        public Object getAdapter(final Class adapter) {
            return null;
        }

        @Override
        public void accept(final IResourceDeltaVisitor visitor) throws CoreException {
        }

        @Override
        public void accept(final IResourceDeltaVisitor visitor,
                final boolean includePhantoms) throws CoreException {
        }

        @Override
        public void accept(final IResourceDeltaVisitor visitor, final int memberFlags)
                throws CoreException {
        }

        @Override
        public IResourceDelta findMember(final IPath path) {
            return null;
        }

        @Override
        public IResourceDelta[] getAffectedChildren() {
            return null;
        }

        @Override
        public IResourceDelta[] getAffectedChildren(final int kindMask) {
            return null;
        }

        @Override
        public IResourceDelta[] getAffectedChildren(final int kindMask,
                final int memberFlags) {
            return null;
        }

        @Override
        public int getFlags() {
            return CONTENT;
        }

        @Override
        public IPath getFullPath() {
            return null;
        }

        @Override
        public int getKind() {
            return 0;
        }

        @Override
        public IMarkerDelta[] getMarkerDeltas() {
            return null;
        }

        @Override
        public IPath getMovedFromPath() {
            return null;
        }

        @Override
        public IPath getMovedToPath() {
            return null;
        }

        @Override
        public IPath getProjectRelativePath() {
            return null;
        }

        @Override
        public IResource getResource() {
            return null;
        }
    }

    // List<IErlElement> getChildren() throws ErlModelException;
    @Test
    public void getChildren() throws Exception {
        final List<IErlElement> children = module.getChildren();
        module.open(null);
        final List<IErlElement> children2 = module.getChildren();
        assertEquals(0, children.size());
        assertEquals(3, children2.size());
        assertEquals(module, children2.get(0).getParent());
    }

    @Test(expected = UnsupportedOperationException.class)
    public void getChildren_unmodifiable() throws Exception {
        module.open(null);
        final List<IErlElement> children = module.getChildren();
        children.remove(0);
    }

    // int getChildCount();
    @Test
    public void getChildCount() throws Exception {
        final int childCount = module.getChildCount();
        module.open(null);
        final int childCount2 = module.getChildCount();
        project.open(null);
        final int childCount3 = project.getChildCount();
        assertEquals(0, childCount);
        assertEquals(3, childCount2);
        assertTrue(childCount3 >= 2);
    }

    // boolean hasChildren();
    @Test
    public void hasChildren() throws Exception {
        final boolean hasChildren = module.hasChildren();
        module.open(null);
        final boolean hasChildren2 = module.hasChildren();
        assertFalse(hasChildren);
        assertTrue(hasChildren2);
    }

    // List<IErlElement> getChildrenOfKind(Kind kind) throws ErlModelException;
    @Test
    public void getChildrenOfKind() throws Exception {
        module.open(null);
        final List<IErlElement> childrenOfKind = module
                .getChildrenOfKind(ErlElementKind.ATTRIBUTE);
        final List<IErlElement> childrenOfKind2 = module
                .getChildrenOfKind(ErlElementKind.FUNCTION);
        final List<IErlElement> childrenOfKind3 = module
                .getChildrenOfKind(ErlElementKind.TYPESPEC);
        assertEquals(2, childrenOfKind.size());
        assertEquals(1, childrenOfKind2.size());
        assertEquals(0, childrenOfKind3.size());
    }

    // boolean hasChildrenOfKind(Kind kind);
    @Test
    public void hasChildrenOfKind() throws Exception {
        module.open(null);
        final boolean hasChildrenOfKind = module
                .hasChildrenOfKind(ErlElementKind.ATTRIBUTE);
        final boolean hasChildrenOfKind2 = module
                .hasChildrenOfKind(ErlElementKind.FUNCTION);
        final boolean hasChildrenOfKind3 = module
                .hasChildrenOfKind(ErlElementKind.TYPESPEC);
        assertTrue(hasChildrenOfKind);
        assertTrue(hasChildrenOfKind2);
        assertFalse(hasChildrenOfKind3);
    }

    // IErlElement getChildNamed(String s);
    @Test
    public void getChildNamed() throws Exception {
        project.open(null);
        final String src = "src";
        final IErlElement childNamed = project.getChildNamed(src);
        final IErlElement childNamed2 = project.getChildNamed("SRC");
        final IErlElement childNamed3 = project.getChildNamed("noway");
        module.open(null);
        final IErlElement childNamed4 = module.getChildNamed("module");
        final IErlElement childNamed5 = module.getChildNamed("f");
        assertEquals(src, childNamed.getName());
        assertNull(childNamed2);
        assertNull(childNamed3);
        assertNotNull(childNamed4);
        assertTrue(childNamed5.getKind() == ErlElementKind.FUNCTION);
    }

    // IErlElement getChildWithResource(IResource rsrc);
    @Test
    public void getChildWithResource() throws Exception {
        final IProject workspaceProject = project.getWorkspaceProject();
        final IErlModel model = ErlangEngine.getInstance().getModel();
        final IErlElement childWithResource = model
                .getChildWithResource(workspaceProject);
        final IResource resource = module.getResource();
        final IErlElement childWithResource2 = model.getChildWithResource(resource);
        final IErlFolder folder = (IErlFolder) project.getChildNamed("src");
        final IErlElement childWithResource3 = folder.getChildWithResource(resource);
        assertEquals(project, childWithResource);
        assertNull(childWithResource2);
        assertEquals(module, childWithResource3);
    }

    // void addChild(IErlElement child);
    @Test
    public void addChild() throws Exception {
        module.open(null);
        final int childCount = module.getChildCount();
        final String aname = "test_a";
        final IErlAttribute attribute = new ErlAttribute(module, aname, null, "test");
        module.addChild(attribute);
        final int childCount2 = module.getChildCount();
        final IErlElement childNamed = module.getChildNamed(aname);
        assertEquals(childCount + 1, childCount2);
        assertEquals(attribute, childNamed);
    }

    // public void setChildren(final Collection<? extends IErlElement>
    // children);
    @Test
    public void setChildren() throws Exception {
        module.open(null);
        final List<IErlElement> children = module.getChildren();
        final String aname = "test_a";
        final IErlAttribute attribute = new ErlAttribute(module, aname, null, "test");
        module.setChildren(Lists.newArrayList(attribute));
        final int childCount = module.getChildCount();
        final List<IErlElement> children2 = module.getChildren();
        final IErlElement element = children2.iterator().next();
        assertEquals(1, childCount);
        assertFalse(children2.equals(children));
        assertEquals(attribute, element);
    }

    // void removeChild(IErlElement e);
    @Test
    public void removeChild() throws Exception {
        module.open(null);
        final int childCount = module.getChildCount();
        final IErlElement element = module.getChildrenOfKind(ErlElementKind.ATTRIBUTE)
                .iterator().next();
        final IErlElement childNamed = module.getChildNamed(element.getName());
        module.removeChild(element);
        final int childCount2 = module.getChildCount();
        final IErlElement childNamed2 = module.getChildNamed(element.getName());
        assertEquals(childCount - 1, childCount2);
        assertNotNull(childNamed);
        assertNull(childNamed2);
    }

}
