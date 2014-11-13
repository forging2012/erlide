package org.erlide.runtime.internal;

import org.erlide.runtime.api.IOtpNodeProxy;
import org.erlide.runtime.events.ErlEvent;
import org.erlide.util.ErlLogger;
import org.erlide.util.erlang.Bindings;
import org.erlide.util.erlang.OtpErlang;
import org.erlide.util.erlang.TermParserException;

import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;

public class EventParser {

    public ErlEvent parse(final OtpErlangObject msg, final IOtpNodeProxy nodeProxy) {
        if (msg == null) {
            return null;
        }
        try {
            final Bindings b = OtpErlang.match("{event,Topic,Event,Sender}", msg);
            if (b == null) {
                return null;
            }
            final String topic = b.getAtom("Topic");
            final OtpErlangObject event = b.get("Event");
            final OtpErlangPid sender = b.getPid("Sender");
            return new ErlEvent(topic, nodeProxy, event, sender);
        } catch (final TermParserException e) {
            return null;
        } catch (final OtpErlangException e) {
            ErlLogger.warn("Unrecognized event: " + msg);
            return null;
        }
    }

}
