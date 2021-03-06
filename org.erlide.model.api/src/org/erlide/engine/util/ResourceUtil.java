/*
 * ResourceUtil.java
 * Created on 2004-08-20
 *
 * cvs-id : $Id$
 */
package org.erlide.engine.util;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.nullValue;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

import org.eclipse.core.filesystem.EFS;
import org.eclipse.core.filesystem.URIUtil;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceVisitor;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.Path;

/**
 * <p>
 * contains static helping functionality to work on file resources in the
 * workspace.
 * </p>
 *
 * @author Leif Frenzel
 * @author Andrei Formiga
 */
public class ResourceUtil {

    /**
     * <p>
     * reads an input stream and returns the contents as String.
     * </p>
     */
    public static String readStream(final InputStream is) throws IOException {
        final StringBuilder sbResult = new StringBuilder();
        final BufferedReader br = new BufferedReader(new InputStreamReader(is));
        String line = br.readLine();
        while (line != null) {
            sbResult.append(line);
            sbResult.append('\n');
            line = br.readLine();
        }
        br.close();
        is.close();

        return sbResult.toString();
    }

    /**
     * finds the corresponding resource for the specified element. This is
     * element itself, if it is an IResource, or an adapter. Returns null, if no
     * resource could be found.
     */
    public static IResource findResource(final Object element) {
        IResource result = null;
        if (element instanceof IResource) {
            result = (IResource) element;
        } else if (element instanceof IAdaptable) {
            final Object adapter = ((IAdaptable) element).getAdapter(IResource.class);
            if (adapter instanceof IResource) {
                result = (IResource) adapter;
            }
        }
        return result;
    }

    public static IResource recursiveFindNamedResource(final IContainer container,
            final String name, final ContainerFilter filter) throws CoreException {
        if (!container.isAccessible()) {
            return null;
        }
        IResource r = container.findMember(name);
        if (r != null && (filter == null || filter.accept(container))) {
            return r;
        }
        final IResource[] members = container.members();
        for (final IResource element : members) {
            r = element;
            if (r instanceof IContainer) {
                r = recursiveFindNamedResource((IContainer) r, name, filter);
                if (r != null) {
                    return r;
                }
            }
        }
        return null;
    }

    public static IFile getFileFromLocation(final String location) {
        final IWorkspaceRoot wr = ResourcesPlugin.getWorkspace().getRoot();
        final IFile[] f = wr.findFilesForLocationURI(URIUtil.toURI(location));
        if (f.length > 0) {
            return f[0];
        }
        return null;
    }

    public static boolean samePath(final String p1, final String p2) {
        assertThat(p1, is(not(nullValue())));
        assertThat(p2, is(not(nullValue())));
        String p1a = p1;
        String p2a = p2;
        if (!EFS.getLocalFileSystem().isCaseSensitive()) {
            p1a = p1.toLowerCase();
            p2a = p2.toLowerCase();
        }
        final Path path1 = new Path(p1a);
        final Path path2 = new Path(p2a);
        return path1.equals(path2);
    }

    private final static class FindResourceVisitor implements IResourceVisitor {
        enum FindKind {
            FIND_BY_NAME, FIND_BY_LOCATION
        }

        private final String fileName;
        private IResource found = null;
        private final FindKind how;

        private FindResourceVisitor(final String fileName, final FindKind how) {
            this.fileName = fileName;
            this.how = how;
        }

        @Override
        public boolean visit(final IResource resource) throws CoreException {
            if (compare(resource, fileName, how)) {
                found = resource;
                return false;
            }
            return true;
        }

        private boolean compare(final IResource resource, final String s,
                final FindKind theHow) {
            switch (theHow) {
            case FIND_BY_NAME:
                return ResourceUtil.samePath(resource.getName(), s);
            case FIND_BY_LOCATION:
                return ResourceUtil.samePath(resource.getLocation().toString(), s)
                        || ResourceUtil.samePath(resource.getProjectRelativePath()
                                .toString(), s);
            default:
                return false;
            }
        }

        public IResource getFound() {
            return found;
        }
    }

    public static IResource findResourceByLocation(final IContainer container,
            final String fileName) {
        return findResource(container, fileName,
                FindResourceVisitor.FindKind.FIND_BY_LOCATION);
    }

    public static IResource findResourceByName(final IContainer container,
            final String fileName) {
        return findResource(container, fileName,
                FindResourceVisitor.FindKind.FIND_BY_NAME);
    }

    private static IResource findResource(final IContainer container,
            final String fileName, final FindResourceVisitor.FindKind how) {
        final FindResourceVisitor visitor = new FindResourceVisitor(fileName, how);
        try {
            container.accept(visitor);
        } catch (final CoreException e) {
            return null;
        }
        return visitor.getFound();
    }

}
