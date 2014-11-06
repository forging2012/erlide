package org.erlide.engine.new_model.internal;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import java.util.List;
import org.eclipse.xtend.lib.annotations.Accessors;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.InputOutput;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.ListExtensions;
import org.eclipse.xtext.xbase.lib.Pure;
import org.erlide.engine.new_model.internal.ASTNode;

@Accessors
@SuppressWarnings("all")
public class ErlangAST {
  private List<ASTNode> forms;
  
  private List<ASTNode> comments;
  
  public ErlangAST(final OtpErlangTuple term) {
    OtpErlangObject[] _elements = term.elements();
    Iterable<OtpErlangObject> _tail = IterableExtensions.<OtpErlangObject>tail(((Iterable<OtpErlangObject>)Conversions.doWrapArray(_elements)));
    OtpErlangObject _head = IterableExtensions.<OtpErlangObject>head(_tail);
    final OtpErlangObject[] forms = ((OtpErlangList) _head).elements();
    final Function1<OtpErlangObject, ASTNode> _function = new Function1<OtpErlangObject, ASTNode>() {
      public ASTNode apply(final OtpErlangObject it) {
        return ErlangAST.this.parse(((OtpErlangTuple) it));
      }
    };
    List<ASTNode> _map = ListExtensions.<OtpErlangObject, ASTNode>map(((List<OtpErlangObject>)Conversions.doWrapArray(forms)), _function);
    this.forms = _map;
    OtpErlangObject[] _elements_1 = term.elements();
    Iterable<OtpErlangObject> _tail_1 = IterableExtensions.<OtpErlangObject>tail(((Iterable<OtpErlangObject>)Conversions.doWrapArray(_elements_1)));
    Iterable<OtpErlangObject> _tail_2 = IterableExtensions.<OtpErlangObject>tail(_tail_1);
    OtpErlangObject _head_1 = IterableExtensions.<OtpErlangObject>head(_tail_2);
    final OtpErlangObject[] comments = ((OtpErlangList) _head_1).elements();
    final Function1<OtpErlangObject, ASTNode> _function_1 = new Function1<OtpErlangObject, ASTNode>() {
      public ASTNode apply(final OtpErlangObject it) {
        return ErlangAST.this.parse(((OtpErlangTuple) it));
      }
    };
    List<ASTNode> _map_1 = ListExtensions.<OtpErlangObject, ASTNode>map(((List<OtpErlangObject>)Conversions.doWrapArray(comments)), _function_1);
    this.comments = _map_1;
  }
  
  public ASTNode parse(final OtpErlangTuple object) {
    ASTNode _xblockexpression = null;
    {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("!! ");
      _builder.append(object, "");
      InputOutput.<String>println(_builder.toString());
      _xblockexpression = new ASTNode(object);
    }
    return _xblockexpression;
  }
  
  @Pure
  public List<ASTNode> getForms() {
    return this.forms;
  }
  
  public void setForms(final List<ASTNode> forms) {
    this.forms = forms;
  }
  
  @Pure
  public List<ASTNode> getComments() {
    return this.comments;
  }
  
  public void setComments(final List<ASTNode> comments) {
    this.comments = comments;
  }
}
