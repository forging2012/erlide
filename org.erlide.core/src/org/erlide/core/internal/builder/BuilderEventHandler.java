package org.erlide.core.internal.builder;

import java.util.List;

import org.erlide.runtime.events.ErlEvent;
import org.erlide.runtime.events.ErlangEventHandler;
import org.erlide.util.ErlLogger;
import org.erlide.util.erlang.Bindings;
import org.erlide.util.erlang.ErlUtils;
import org.erlide.util.erlang.TermParserException;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.google.common.base.Objects;
import com.google.common.collect.Lists;
import com.google.common.eventbus.Subscribe;

public class BuilderEventHandler extends ErlangEventHandler {

    private State state;
    private final Context context;

    private class Context {

        String project;
        String operation;
        BuildNotifier notifier;
        public long num;
        public String tag;
        public List<OtpErlangObject> crtItems = Lists.newArrayList();

    }

    private enum State {
        INIT {
            @Override
            State process(final OtpErlangObject data, final Context context)
                    throws TermParserException, OtpErlangException {
                final Bindings b = ErlUtils.match("{start,Operation,Project}", data);
                if (b != null) {
                    final String operation = b.getAtom("Operation");
                    final String project = b.getString("Project");
                    if (Objects.equal(project, context.project)) {
                        context.operation = operation;
                        return State.valueOf(operation.toUpperCase());
                    }
                    return IGNORING;
                }
                System.out.println("> UNHANDLED: " + data + " in " + this);
                return INIT;
            }
        },
        IGNORING {
            @Override
            State process(final OtpErlangObject data, final Context context)
                    throws TermParserException, OtpErlangException {
                final Bindings b = ErlUtils.match("{start,Operation,Project}", data);
                if (b != null) {
                    final String operation = b.getAtom("Operation");
                    final String project = b.getString("Project");
                    if (Objects.equal(project, context.project)) {
                        context.operation = operation;
                        return State.valueOf(operation.toUpperCase());
                    }
                    return IGNORING;
                }
                System.out.println("> IGNORED: " + data + " in " + this);
                return IGNORING;
            }
        },
        COMPILE {
            @Override
            State process(final OtpErlangObject data, final Context context)
                    throws TermParserException, OtpErlangException {
                if (data instanceof OtpErlangAtom) {
                    if ("done".equals(((OtpErlangAtom) data).atomValue())) {
                        return INIT;
                    }
                }
                final Bindings b = ErlUtils.match("{start,Operation,Project}", data);
                if (b != null) {
                    final String operation = b.getAtom("Operation");
                    final String project = b.getString("Project");
                    if (Objects.equal(project, context.project)) {
                        context.operation = operation;
                        return State.valueOf(operation.toUpperCase());
                    }
                    return IGNORING;
                }
                if (handleTotal(data, context)) {
                    return COMPILE_1;
                }
                System.out.println("> UNHANDLED: " + data + " in " + this);
                return COMPILE;
            }

        },
        COMPILE_1 {
            @Override
            State process(final OtpErlangObject data, final Context context)
                    throws TermParserException, OtpErlangException {
                if (data instanceof OtpErlangAtom) {
                    if ("done".equals(((OtpErlangAtom) data).atomValue())) {
                        return INIT;
                    }
                }
                final Bindings b = ErlUtils.match("{start,Operation,Project}", data);
                if (b != null) {
                    final String operation = b.getAtom("Operation");
                    return State.valueOf(operation.toUpperCase());
                }
                if (handleTotal(data, context)) {
                    return COMPILE_1;
                }
                if (handleCompileMessages(data, context)) {
                    return COMPILE_2;
                }
                System.out.println("> UNHANDLED: " + data + " in " + this);
                return COMPILE_1;
            }
        },
        COMPILE_2 {
            @Override
            State process(final OtpErlangObject data, final Context context)
                    throws TermParserException, OtpErlangException {
                if (data instanceof OtpErlangAtom) {
                    if ("done".equals(((OtpErlangAtom) data).atomValue())) {
                        return INIT;
                    }
                }
                if (handleTotal(data, context)) {
                    return COMPILE_1;
                }
                if (handleCompileMessages(data, context)) {
                    return COMPILE_2;
                }
                System.out.println("> UNHANDLED: " + data + " in " + this);
                return COMPILE_1;
            }
        },
        EUNIT {
            @Override
            State process(final OtpErlangObject data, final Context context)
                    throws TermParserException, OtpErlangException {
                if (data instanceof OtpErlangAtom) {
                    if ("done".equals(((OtpErlangAtom) data).atomValue())) {
                        return INIT;
                    }
                }
                final Bindings b1 = ErlUtils.match("{start,Operation,Project}", data);
                if (b1 != null) {
                    final String operation = b1.getAtom("Operation");
                    return State.valueOf(operation.toUpperCase());
                }
                System.out.println("> UNHANDLED: " + data + " in " + this);
                return EUNIT;
            }
        },
        CLEAN {
            @Override
            State process(final OtpErlangObject data, final Context context)
                    throws TermParserException, OtpErlangException {
                if (data instanceof OtpErlangAtom) {
                    if ("done".equals(((OtpErlangAtom) data).atomValue())) {
                        return INIT;
                    }
                }
                final Bindings b1 = ErlUtils.match("{start,Operation,Project}", data);
                if (b1 != null) {
                    final String operation = b1.getAtom("Operation");
                    return State.valueOf(operation.toUpperCase());
                }
                System.out.println("> UNHANDLED: " + data + " in " + this);
                return CLEAN;
            }
        };

        abstract State process(OtpErlangObject data, final Context context)
                throws TermParserException, OtpErlangException;

        private static boolean handleCompileMessages(final OtpErlangObject data,
                final Context context) throws TermParserException, OtpErlangException {
            if (!context.tag.equals(".erl")) {
                return false;
            }

            final Bindings b = ErlUtils.match("{Tag,File}", data);
            if (b != null) {
                final String tag = b.getAtom("Tag");
                if ("compiled".equals(tag)) {
                    final String fileName = b.getString("File");
                    createMarkers(fileName, context.crtItems, context.operation);
                    reloadBeam(fileName);
                }
                context.notifier.updateProgressDelta(1.0f / context.num);
                // context.notifier.compiled(unit);
                context.crtItems.clear();
                return true;
            }

            if (data instanceof OtpErlangList) {
                final OtpErlangList list = (OtpErlangList) data;
                for (final OtpErlangObject item : list.elements()) {
                    context.crtItems.add(item);
                }
                return true;
            }
            return false;
        }

        private static boolean handleTotal(final OtpErlangObject data,
                final Context context) throws TermParserException, OtpErlangException {
            final Bindings b1 = ErlUtils.match("{total,Tag,Num}", data);
            if (b1 != null) {
                final String tag = b1.getString("Tag");
                final long num = b1.getLong("Num");
                context.tag = tag;
                context.num = num;
                return true;
            }
            return false;
        }
    }

    public BuilderEventHandler(final String backendName, final BuildNotifier notifier,
            final String project) {
        super("builder", backendName);
        this.state = State.INIT;
        this.context = new Context();
        context.notifier = notifier;
        context.project = project;
    }

    @Subscribe
    public void handleEvent(final ErlEvent event) {
        if (!event.getTopic().equals(getTopic())) {
            return;
        }
        final OtpErlangObject data = event.getEvent();
        System.out.println(" --> " + data);
        try {
            final State newState = state.process(data, context);
            if (state != newState) {
                System.out.println("   # " + state + " -> " + newState);
            }
            state = newState;
        } catch (final Exception e) {
            ErlLogger.error("erroneous builder msg: %s", data);
            ErlLogger.error(e);
        }
    }

    private static void createMarkers(final String fileName,
            final List<OtpErlangObject> items, final String operation) {
        System.out.println("MARK " + fileName + " " + items + " @" + operation);
        // TODO find resource
        // TODO clear markers for resource (not if building for eunit!)
        // TODO add markers from items
    }

    private static void reloadBeam(final String asString) {
        // TODO find beam resource
        // TODO reload it
    }

}
