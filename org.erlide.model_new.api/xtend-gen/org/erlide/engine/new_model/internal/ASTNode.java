package org.erlide.engine.new_model.internal;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import java.util.List;
import org.eclipse.handly.util.TextRange;
import org.eclipse.xtend.lib.annotations.Accessors;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.Pure;
import org.erlide.util.erlang.Bindings;
import org.erlide.util.erlang.OtpErlang;

@Accessors
@SuppressWarnings("all")
public class ASTNode {
  private List<OtpErlangObject> items;
  
  public ASTNode(final OtpErlangTuple source) {
    OtpErlangObject[] _elements = source.elements();
    this.items = ((List<OtpErlangObject>)Conversions.doWrapArray(_elements));
  }
  
  public String getKind() {
    OtpErlangObject _head = IterableExtensions.<OtpErlangObject>head(this.items);
    return ((OtpErlangAtom) _head).atomValue();
  }
  
  public TextRange getPos() {
    try {
      TextRange _xblockexpression = null;
      {
        Iterable<OtpErlangObject> _tail = IterableExtensions.<OtpErlangObject>tail(this.items);
        OtpErlangObject _head = IterableExtensions.<OtpErlangObject>head(_tail);
        final Bindings pos = OtpErlang.match("{{Line,Offset,FileOffset},Len}", _head);
        int _int = pos.getInt("FileOffset");
        int _int_1 = pos.getInt("Len");
        _xblockexpression = new TextRange(_int, _int_1);
      }
      return _xblockexpression;
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  public String getName() {
    Iterable<OtpErlangObject> _tail = IterableExtensions.<OtpErlangObject>tail(this.items);
    Iterable<OtpErlangObject> _tail_1 = IterableExtensions.<OtpErlangObject>tail(_tail);
    OtpErlangObject _head = IterableExtensions.<OtpErlangObject>head(_tail_1);
    return ((OtpErlangAtom) _head).atomValue();
  }
  
  public int getArg() {
    try {
      Iterable<OtpErlangObject> _tail = IterableExtensions.<OtpErlangObject>tail(this.items);
      Iterable<OtpErlangObject> _tail_1 = IterableExtensions.<OtpErlangObject>tail(_tail);
      Iterable<OtpErlangObject> _tail_2 = IterableExtensions.<OtpErlangObject>tail(_tail_1);
      OtpErlangObject _head = IterableExtensions.<OtpErlangObject>head(_tail_2);
      return ((OtpErlangLong) _head).intValue();
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Pure
  public List<OtpErlangObject> getItems() {
    return this.items;
  }
  
  public void setItems(final List<OtpErlangObject> items) {
    this.items = items;
  }
}
