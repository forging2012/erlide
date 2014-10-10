package org.erlide.engine.services.importer;

import java.util.Collection;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

import org.erlide.util.Util;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class ErlProjectImport {
    private final SortedSet<String> resources;
    private final List<String> sourceDirs;
    private final List<String> includeDirs;
    private final String beamDir;
    private final List<String> directories;

    public ErlProjectImport(final OtpErlangObject o) {
        final OtpErlangTuple t = (OtpErlangTuple) o;

        OtpErlangList l = (OtpErlangList) t.elementAt(0);
        resources = new TreeSet<String>(Util.asStringList(l));

        l = (OtpErlangList) t.elementAt(1);
        sourceDirs = Util.asStringList(l);

        l = (OtpErlangList) t.elementAt(2);
        includeDirs = Util.asStringList(l);

        final OtpErlangObject beamDirElement = t.elementAt(3);
        if (beamDirElement instanceof OtpErlangAtom) {
            beamDir = "ebin";
        } else {
            beamDir = Util.stringValue(beamDirElement);
        }

        l = (OtpErlangList) t.elementAt(4);
        directories = Util.asStringList(l);
        directories.add(0, ".");
    }

    public Collection<String> getResources() {
        return resources;
    }

    public List<String> getDirectories() {
        return directories;
    }

    public List<String> getSourceDirs() {
        return sourceDirs;
    }

    public List<String> getIncludeDirs() {
        return includeDirs;
    }

    public String getBeamDir() {
        return beamDir;
    }
}
