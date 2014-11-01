package org.erlide.engine.services.parsing;

import java.util.List;

import org.erlide.engine.services.ErlangService;

import com.ericsson.otp.erlang.OtpErlangObject;

public interface SimpleParserService extends ErlangService {

    List<OtpErlangObject> parse(String s);

}
