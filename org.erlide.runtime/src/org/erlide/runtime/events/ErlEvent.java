package org.erlide.runtime.events;

import org.erlide.runtime.api.IOtpNodeProxy;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;

public final class ErlEvent {

    private final String topic;
    private final IOtpNodeProxy nodeProxy;
    private final OtpErlangObject event;
    private final OtpErlangPid sender;

    public ErlEvent(final String topic, final IOtpNodeProxy nodeProxy,
            final OtpErlangObject event, final OtpErlangPid sender) {
        this.topic = topic;
        this.nodeProxy = nodeProxy;
        this.event = event;
        this.sender = sender;
    }

    public OtpErlangObject getEvent() {
        return event;
    }

    public String getTopic() {
        return topic;
    }

    public IOtpNodeProxy getNodeProxy() {
        return nodeProxy;
    }

    public OtpErlangPid getSender() {
        return sender;
    }
}
