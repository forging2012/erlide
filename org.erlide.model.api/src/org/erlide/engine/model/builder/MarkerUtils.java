package org.erlide.engine.model.builder;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.xtext.xbase.lib.Pair;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.model.erlang.IErlFunction;
import org.erlide.engine.model.erlang.IErlModule;
import org.erlide.engine.model.erlang.ISourceRange;
import org.erlide.engine.model.root.IErlElementLocator;
import org.erlide.engine.model.root.IErlProject;
import org.erlide.engine.util.ResourceUtil;
import org.erlide.util.ErlLogger;
import org.erlide.util.SystemConfiguration;
import org.erlide.util.SystemConfiguration.Features;
import org.erlide.util.erlang.OtpErlang;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.base.Joiner;

public final class MarkerUtils {

    // do this to keep them from being detected as real tasks by Jenkins plugin
    private static final String FIXME_TAG = "F" + "IXME";
    private static final String XXX_TAG = "X" + "XX";
    private static final String TODO_TAG = "T" + "ODO";
    private static final String TODO_XXX_FIXME_PATTERN = "^[^%]*%+[ \t]*(" + TODO_TAG
            + "|" + XXX_TAG + "|" + FIXME_TAG + ").*";
    // Copied from org.eclipse.ui.ide (since we don't want ui code in core)
    public static final String PATH_ATTRIBUTE = "org.eclipse.ui.views.markers.path";//$NON-NLS-1$

    private MarkerUtils() {
    }

    public static final String PROBLEM_MARKER = "org.erlide.core.problemmarker";
    public static final String TASK_MARKER = "org.erlide.core.taskmarker";

    public static void addTaskMarker(final IResource resource, final String message,
            final int lineNumber, final int priority) {
        createMarker(resource, null, message, lineNumber, IMarker.SEVERITY_INFO,
                TASK_MARKER);
    }

    /**
     * Add error markers from a list of error tuples
     *
     * @param resource
     * @param errorList
     */
    public static void addErrorMarkers(final IResource resource,
            final OtpErlangList errorList) {
        final OtpErlangObject[] messages = errorList.elements();
        addErrorMarkers(resource, messages);
    }

    public static void addErrorMarkers(final IResource resource,
            final Collection<OtpErlangObject> errors) {
        final OtpErlangObject[] messages = errors.toArray(new OtpErlangObject[errors
                .size()]);
        addErrorMarkers(resource, messages);
    }

    public static void addErrorMarkers(final IResource resource,
            final OtpErlangObject[] messages) {
        for (final OtpErlangObject entry : messages) {
            final OtpErlangTuple message = (OtpErlangTuple) entry;

            final String fileName = OtpErlang.asString(message.elementAt(1));
            final IResource res = findResourceForFileName(fileName, resource);
            addAnnotationForMessage(fileName, res, message);
        }
    }

    private static IResource findResourceForFileName(final String fileName,
            final IResource resource) {
        IResource result = resource;
        if (fileName == null || "project".equals(fileName)) {
            return resource;
        }
        if (!ResourceUtil.samePath(resource.getLocation().toString(), fileName)) {
            final IProject project = resource.getProject();
            result = ResourceUtil.findResourceByLocation(project, fileName);
            if (result == null) {
                try {
                    final IErlElementLocator model = ErlangEngine.getInstance()
                            .getModel();
                    final IErlProject erlProject = model.findProject(project);
                    if (erlProject != null) {
                        final IErlModule includeFile = model.findIncludeFromProject(
                                erlProject, fileName, fileName,
                                IErlElementLocator.Scope.REFERENCED_PROJECTS);

                        if (includeFile == null) {
                            result = resource;
                        } else {
                            result = includeFile.getResource();
                        }
                    } else {
                        result = resource;
                    }
                } catch (final Exception e) {
                    ErlLogger.warn(e);
                }
            }
        }
        return result;
    }

    private static void addAnnotationForMessage(final String fileName,
            final IResource res, final OtpErlangTuple data) {
        final IMarker marker = AnnotationBuilder.get(res, fileName, data);
        if (marker != null) {
            try {
                marker.setAttribute(IMarker.SOURCE_ID, res.getLocation().toString());
            } catch (final CoreException e) {
            }
        }
    }

    public static IMarker createProblemMarker(final IResource resource,
            final String path, final String message, final int lineNumber,
            final int severity) {
        return createMarker(resource, path, message, lineNumber, severity, PROBLEM_MARKER);
    }

    public static IMarker[] getProblemsFor(final IResource resource) {
        return getMarkersFor(resource, PROBLEM_MARKER);
    }

    public static IMarker[] getTasksFor(final IResource resource) {
        return getMarkersFor(resource, TASK_MARKER);
    }

    private static IMarker[] getMarkersFor(final IResource resource, final String type) {
        try {
            if (resource != null && resource.exists()) {
                return resource.findMarkers(type, false, IResource.DEPTH_INFINITE);
            }
        } catch (final CoreException e) {
            // assume there are no tasks
        }
        return new IMarker[0];
    }

    public static void removeProblemMarkersFor(final IResource resource) {
        removeMarkersFor(resource, PROBLEM_MARKER);
    }

    public static void removeTaskMarkers(final IResource resource) {
        removeMarkersFor(resource, TASK_MARKER);
    }

    private static void removeMarkersFor(final IResource resource, final String type) {
        try {
            if (resource != null && resource.exists()) {
                resource.deleteMarkers(type, true, IResource.DEPTH_INFINITE);
            }
        } catch (final CoreException e) {
            // assume there were no problems
        }
    }

    public static void deleteMarkers(final IResource resource) {
        removeProblemMarkersFor(resource);
        if (resource instanceof IFile) {
            deleteMarkersWithCompiledFile(resource.getProject(), (IFile) resource);
        }
    }

    private static void deleteMarkersWithCompiledFile(final IProject project,
            final IFile file) {
        if (!project.isAccessible()) {
            return;
        }
        try {
            for (final IMarker m : project.findMarkers(PROBLEM_MARKER, true,
                    IResource.DEPTH_INFINITE)) {
                final Object source_id = m.getAttribute(IMarker.SOURCE_ID);
                if (source_id instanceof String
                        && source_id.equals(file.getFullPath().toString())) {
                    try {
                        m.delete();
                    } catch (final CoreException e) {
                        // not much to do
                    }
                }
            }
        } catch (final CoreException e) {
            // not much to do
        }
    }

    public void createProblemMarkerFor(final IResource resource,
            final IErlFunction erlElement, final String message, final int problemSeverity)
            throws CoreException {
        final ISourceRange range = erlElement == null ? null : erlElement.getNameRange();

        final IMarker marker = createProblemMarker(resource, null, message, 0,
                problemSeverity);

        final int start = range == null ? 0 : range.getOffset();
        final int end = range == null ? 1 : start + range.getLength();
        marker.setAttribute(IMarker.CHAR_START, Integer.valueOf(start));
        marker.setAttribute(IMarker.CHAR_END, Integer.valueOf(end));
    }

    public static IMarker createSearchResultMarker(final IErlModule module,
            final String type, final int offset, final int length) throws CoreException {
        boolean setPath = false;
        IResource resource = module.getCorrespondingResource();
        if (resource == null) {
            resource = ResourcesPlugin.getWorkspace().getRoot();
            setPath = true;
        }
        final IMarker marker = resource.createMarker(type);
        marker.setAttribute(IMarker.CHAR_START, offset);
        marker.setAttribute(IMarker.CHAR_END, offset + length);
        if (setPath) {
            marker.setAttribute(PATH_ATTRIBUTE, module.getFilePath());
        }
        return marker;
    }

    public static IMarker createMarker(final IResource file, final String path,
            final String message, final int lineNumber, final int severity,
            final String markerKind) {
        try {
            IResource resource;
            if (file != null) {
                resource = file;
            } else {
                resource = ResourcesPlugin.getWorkspace().getRoot();
            }
            final IMarker marker = resource.createMarker(markerKind);
            marker.setAttribute(IMarker.MESSAGE, message);
            marker.setAttribute(IMarker.SEVERITY, severity);
            marker.setAttribute(IMarker.LINE_NUMBER, lineNumber >= 0 ? lineNumber : 1);
            String myPath = path;
            if (path == null) {
                myPath = "/" + resource.getProject().getName() + "/"
                        + resource.getParent().getProjectRelativePath().toString();
            }
            marker.setAttribute(PATH_ATTRIBUTE, myPath);
            marker.setAttribute(IMarker.SOURCE_ID, myPath);
            final ProblemData problem = ErlProblems.parse(message);
            if (problem != null) {
                marker.setAttribute(ProblemData.TAG, problem.getTag());
                marker.setAttribute(ProblemData.ARGS,
                        Joiner.on('\0').join(problem.getMessageArgs(message)));
            }
            return marker;
        } catch (final CoreException e) {
        }
        return null;
    }

    public static void createTaskMarkers(final IResource resource) {
        if (SystemConfiguration.hasFeatureEnabled(Features.SKIP_TASKS)) {
            return;
        }
        getScanMarkersFor(resource);
    }

    public static void getScanMarkersFor(final IResource resource) {
        try {
            final IPath location = resource.getLocation();
            if (location == null) {
                ErlLogger.warn("can't locate " + resource);
                return;
            }
            final BufferedReader reader = new BufferedReader(new FileReader(
                    location.toPortableString()));
            try {
                String line = reader.readLine();
                final List<Pair<String, Integer>> cl = new ArrayList<Pair<String, Integer>>();
                int numline = 0;
                while (line != null) {
                    if (line.matches(TODO_XXX_FIXME_PATTERN)) {
                        cl.add(new Pair<String, Integer>(line, numline));
                    }
                    numline++;
                    line = reader.readLine();
                }

                for (final Pair<String, Integer> c : cl) {
                    createTaskMarkerAtText(resource, c.getValue(), c.getKey(), TODO_TAG,
                            IMarker.PRIORITY_NORMAL);
                    createTaskMarkerAtText(resource, c.getValue(), c.getKey(), XXX_TAG,
                            IMarker.PRIORITY_NORMAL);
                    createTaskMarkerAtText(resource, c.getValue(), c.getKey(), FIXME_TAG,
                            IMarker.PRIORITY_HIGH);
                }
            } finally {
                reader.close();
            }
        } catch (final IOException e) {
        }
    }

    private static void createTaskMarkerAtText(final IResource resource, final int line,
            final String text, final String tag, final int prio) {
        if (text.contains(tag)) {
            final int ix = text.indexOf(tag);
            final String msg = text.substring(ix);
            int dl = 0;
            for (int i = 0; i < ix; i++) {
                if (text.charAt(i) == '\n') {
                    dl++;
                }
            }
            addTaskMarker(resource, msg, line + 1 + dl, prio);
        }
    }

    private static class AnnotationBuilder {

        private static final Object ERROR = new OtpErlangAtom("error");
        private static final Object WARNING = new OtpErlangAtom("warning");

        public static IMarker get(final IResource res, final String fileName,
                final OtpErlangTuple data) {
            final int line = getLine(data);
            final int sev = getSeverity(data);
            final String msg = getMessage(data);

            return createMarker(res, fileName, msg, line, sev, PROBLEM_MARKER);
        }

        private static String getMessage(final OtpErlangTuple data) {
            return OtpErlang.asString(data.elementAt(2));
        }

        private static int getSeverity(final OtpErlangTuple data) {
            int sev = IMarker.SEVERITY_INFO;
            final OtpErlangObject sev_tag = data.elementAt(3);
            if (sev_tag instanceof OtpErlangAtom) {
                final OtpErlangAtom tag = (OtpErlangAtom) sev_tag;
                if (tag.equals(ERROR)) {
                    sev = IMarker.SEVERITY_ERROR;
                } else if (tag.equals(WARNING)) {
                    sev = IMarker.SEVERITY_WARNING;
                }
            } else {
                final OtpErlangLong tag = (OtpErlangLong) sev_tag;
                try {
                    switch (tag.intValue()) {
                    case 0:
                        sev = IMarker.SEVERITY_ERROR;
                        break;
                    case 1:
                        sev = IMarker.SEVERITY_WARNING;
                        break;
                    default:
                        sev = IMarker.SEVERITY_INFO;
                        break;
                    }
                } catch (final OtpErlangRangeException e) {
                }
            }
            return sev;
        }

        private static int getLine(final OtpErlangTuple data) {
            int line = 0;
            final OtpErlangObject elem = data.elementAt(0);
            if (elem instanceof OtpErlangLong) {
                try {
                    line = ((OtpErlangLong) elem).intValue();
                } catch (final OtpErlangRangeException e) {
                }
            }
            return line;
        }

    }

}
