package org.erlide.engine.services.search;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.base.Objects;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.erlide.engine.services.search.ExternalCallOpenResult;
import org.erlide.engine.services.search.FieldOpenResult;
import org.erlide.engine.services.search.IncludeOpenResult;
import org.erlide.engine.services.search.LocalCallOpenResult;
import org.erlide.engine.services.search.MacroOpenResult;
import org.erlide.engine.services.search.RecordOpenResult;
import org.erlide.engine.services.search.VariableOpenResult;
import org.erlide.util.ErlLogger;
import org.erlide.util.Util;

@SuppressWarnings("all")
public abstract class OpenResult {
  public static OpenResult build(final OtpErlangObject res) {
    if ((!(res instanceof OtpErlangTuple))) {
      return null;
    }
    final OtpErlangTuple openTuple = ((OtpErlangTuple) res);
    final OtpErlangObject tag = openTuple.elementAt(0);
    if ((!(tag instanceof OtpErlangAtom))) {
      return null;
    }
    final String kind = ((OtpErlangAtom) tag).atomValue();
    try {
      boolean _matched = false;
      if (!_matched) {
        if (Objects.equal(kind, "external")) {
          _matched=true;
          OtpErlangObject _elementAt = openTuple.elementAt(1);
          String _atomValue = ((OtpErlangAtom) _elementAt).atomValue();
          OtpErlangObject _elementAt_1 = openTuple.elementAt(2);
          String _atomValue_1 = ((OtpErlangAtom) _elementAt_1).atomValue();
          OtpErlangObject _elementAt_2 = openTuple.elementAt(3);
          int _intValue = ((OtpErlangLong) _elementAt_2).intValue();
          String _xifexpression = null;
          boolean _and = false;
          int _arity = openTuple.arity();
          boolean _greaterThan = (_arity > 4);
          if (!_greaterThan) {
            _and = false;
          } else {
            OtpErlangObject _elementAt_3 = openTuple.elementAt(4);
            _and = (_elementAt_3 instanceof OtpErlangString);
          }
          if (_and) {
            OtpErlangObject _elementAt_4 = openTuple.elementAt(4);
            _xifexpression = ((OtpErlangString) _elementAt_4).stringValue();
          } else {
            _xifexpression = null;
          }
          return new ExternalCallOpenResult(_atomValue, _atomValue_1, _intValue, _xifexpression);
        }
      }
      if (!_matched) {
        if (Objects.equal(kind, "local")) {
          _matched=true;
          OtpErlangObject _elementAt_5 = openTuple.elementAt(1);
          String _atomValue_2 = ((OtpErlangAtom) _elementAt_5).atomValue();
          OtpErlangObject _elementAt_6 = openTuple.elementAt(2);
          int _intValue_1 = ((OtpErlangLong) _elementAt_6).intValue();
          return new LocalCallOpenResult(_atomValue_2, _intValue_1);
        }
      }
      if (!_matched) {
        if (Objects.equal(kind, "include")) {
          _matched=true;
          OtpErlangObject _elementAt_7 = openTuple.elementAt(1);
          String _stringValue = Util.stringValue(_elementAt_7);
          return new IncludeOpenResult(_stringValue, null);
        }
      }
      if (!_matched) {
        if (Objects.equal(kind, "include_lib")) {
          _matched=true;
          OtpErlangObject _elementAt_8 = openTuple.elementAt(1);
          String _stringValue_1 = Util.stringValue(_elementAt_8);
          OtpErlangObject _elementAt_9 = openTuple.elementAt(2);
          String _stringValue_2 = Util.stringValue(_elementAt_9);
          return new IncludeOpenResult(_stringValue_1, _stringValue_2);
        }
      }
      if (!_matched) {
        if (Objects.equal(kind, "record")) {
          _matched=true;
          OtpErlangObject _elementAt_10 = openTuple.elementAt(1);
          String _string = ((OtpErlangAtom) _elementAt_10).toString();
          return new RecordOpenResult(_string);
        }
      }
      if (!_matched) {
        if (Objects.equal(kind, "macro")) {
          _matched=true;
          OtpErlangObject _elementAt_11 = openTuple.elementAt(1);
          final OtpErlangAtom element = ((OtpErlangAtom) _elementAt_11);
          String _atomValue_3 = element.atomValue();
          String _removeQuestionMark = OpenResult.removeQuestionMark(_atomValue_3);
          return new MacroOpenResult(_removeQuestionMark);
        }
      }
      if (!_matched) {
        if (Objects.equal(kind, "variable")) {
          _matched=true;
          final OtpErlangObject o = openTuple.elementAt(1);
          String _xifexpression_1 = null;
          if ((o instanceof OtpErlangTuple)) {
            String _xblockexpression = null;
            {
              OtpErlangObject _elementAt_12 = ((OtpErlangTuple)o).elementAt(0);
              final OtpErlangAtom a = ((OtpErlangAtom) _elementAt_12);
              _xblockexpression = a.atomValue();
            }
            _xifexpression_1 = _xblockexpression;
          } else {
            String _xifexpression_2 = null;
            if ((o instanceof OtpErlangAtom)) {
              _xifexpression_2 = ((OtpErlangAtom)o).atomValue();
            }
            _xifexpression_1 = _xifexpression_2;
          }
          return new VariableOpenResult(_xifexpression_1);
        }
      }
      if (!_matched) {
        if (Objects.equal(kind, "field")) {
          _matched=true;
          OtpErlangObject _elementAt_12 = openTuple.elementAt(1);
          String _atomValue_4 = ((OtpErlangAtom) _elementAt_12).atomValue();
          OtpErlangObject _elementAt_13 = openTuple.elementAt(2);
          String _atomValue_5 = ((OtpErlangAtom) _elementAt_13).atomValue();
          return new FieldOpenResult(_atomValue_4, _atomValue_5);
        }
      }
      return null;
    } catch (final Throwable _t) {
      if (_t instanceof Exception) {
        final Exception e = (Exception)_t;
        ErlLogger.warn(e);
        return null;
      } else {
        throw Exceptions.sneakyThrow(_t);
      }
    }
  }
  
  private static String removeQuestionMark(final String name) {
    final int i = name.indexOf("?");
    if (((i == 0) || (i == 1))) {
      String _substring = name.substring(0, i);
      String _substring_1 = name.substring((i + 1));
      return (_substring + _substring_1);
    }
    return name;
  }
}
