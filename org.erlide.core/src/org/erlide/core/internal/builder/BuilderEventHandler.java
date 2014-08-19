package org.erlide.core.internal.builder;

import java.util.List;

import org.erlide.runtime.events.ErlEvent;
import org.erlide.runtime.events.ErlangEventHandler;
import org.erlide.util.ErlLogger;
import org.erlide.util.erlang.Bindings;
import org.erlide.util.erlang.ErlUtils;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.google.common.collect.Lists;
import com.google.common.eventbus.Subscribe;

public class BuilderEventHandler extends ErlangEventHandler {

    private final BuildNotifier notifier;
    String operation;
    private final List<OtpErlangObject> crtItems = Lists.newArrayList();

    public BuilderEventHandler(final String backendName, final BuildNotifier notifier) {
        super("builder", backendName);
        this.notifier = notifier;
    }

    @Subscribe
    public void handleEvent(final ErlEvent event) {
        if (!event.getTopic().equals(getTopic())) {
            return;
        }
        final OtpErlangObject data = event.getEvent();
        try {
            System.out.println("---> " + data);
            final Bindings b1 = ErlUtils.match("{start,Operation,Project}", data);
            if (b1 != null) {
                operation = b1.getAtom("Operation");
                return;
            }
            final Bindings b = ErlUtils.match("{Tag,File}", data);
            if (b != null) {
                final String tag = b.getAtom("Tag");
                if ("compiled".equals(tag)) {
                    final String fileName = b.getString("File");
                    createMarkers(fileName, crtItems, operation);
                    reloadBeam(fileName);
                }
                // TODO how to compute the right increment?
                notifier.updateProgressDelta(0.1f);
                // notifier.compiled(unit);
                crtItems.clear();
                return;
            }
            if (data instanceof OtpErlangList) {
                final OtpErlangList list = (OtpErlangList) data;
                for (final OtpErlangObject item : list.elements()) {
                    crtItems.add(item);
                }
                return;
            }
            System.out.println("> UNHANDLED: " + data);
        } catch (final Exception e) {
            ErlLogger.error("erroneous builder msg: %s", data);
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
