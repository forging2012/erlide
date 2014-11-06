package org.erlide.engine.new_model.internal;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.base.Objects;
import java.io.IOException;
import java.util.Map;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.handly.model.IHandle;
import org.eclipse.handly.model.impl.Body;
import org.eclipse.handly.model.impl.HandleManager;
import org.eclipse.handly.model.impl.SourceElementBody;
import org.eclipse.handly.model.impl.SourceFile;
import org.eclipse.xtend.lib.annotations.Data;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.IErlangEngine;
import org.erlide.engine.NewModelActivator;
import org.erlide.engine.new_model.IErlAttribute;
import org.erlide.engine.new_model.IErlComment;
import org.erlide.engine.new_model.IErlError;
import org.erlide.engine.new_model.IErlForm;
import org.erlide.engine.new_model.IErlFunction;
import org.erlide.engine.new_model.IErlSource;
import org.erlide.engine.new_model.internal.ErlFileStructureBuilder;
import org.erlide.engine.new_model.internal.ErlFunction;
import org.erlide.engine.new_model.internal.ErlModelManager;
import org.erlide.engine.new_model.internal.ErlProject;
import org.erlide.engine.new_model.internal.ErlangAST;
import org.erlide.engine.services.parsing.ParserService;
import org.erlide.util.Util;

@Data
@SuppressWarnings("all")
public abstract class ErlSource extends SourceFile implements IErlSource {
  public ErlSource(final ErlProject parent, final IFile file) {
    super(parent, file);
  }
  
  protected void buildStructure(final SourceElementBody body, final Map<IHandle, Body> newElements, final Object ast, final String source) {
    final ErlFileStructureBuilder builder = new ErlFileStructureBuilder(newElements, ((ErlangAST) ast));
    builder.buildStructure(this, body);
  }
  
  protected Object createStructuralAst(final String source) throws CoreException {
    try {
      IFile _file = this.getFile();
      String _charset = _file.getCharset();
      return this.parse(source, _charset);
    } catch (final Throwable _t) {
      if (_t instanceof IOException) {
        final IOException e = (IOException)_t;
        String _message = e.getMessage();
        IStatus _createErrorStatus = NewModelActivator.createErrorStatus(_message, e);
        throw new CoreException(_createErrorStatus);
      } else {
        throw Exceptions.sneakyThrow(_t);
      }
    }
  }
  
  public ErlangAST parse(final String contents, final String encoding) {
    ErlangAST _xblockexpression = null;
    {
      IErlangEngine _instance = ErlangEngine.getInstance();
      final ParserService parser = _instance.getParserService();
      IPath _projectRelativePath = this.file.getProjectRelativePath();
      String _oSString = _projectRelativePath.toOSString();
      final OtpErlangObject result = parser.parse(_oSString, contents);
      ErlangAST _xifexpression = null;
      boolean _isOk = Util.isOk(result);
      if (_isOk) {
        ErlangAST _xblockexpression_1 = null;
        {
          final OtpErlangTuple tuple = ((OtpErlangTuple) result);
          OtpErlangObject _elementAt = tuple.elementAt(1);
          _xblockexpression_1 = new ErlangAST(((OtpErlangTuple) _elementAt));
        }
        _xifexpression = _xblockexpression_1;
      } else {
        _xifexpression = null;
      }
      _xblockexpression = _xifexpression;
    }
    return _xblockexpression;
  }
  
  protected HandleManager getHandleManager() {
    return ErlModelManager.INSTANCE.getHandleManager();
  }
  
  public Iterable<IErlForm> getForms() {
    try {
      return (Iterable<IErlForm>)Conversions.doWrapArray(this.<IErlForm>getChildren(IErlForm.class));
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  public IErlComment getHeaderComment() {
    try {
      IErlComment _xblockexpression = null;
      {
        IHandle[] _children = this.getChildren();
        final IHandle first = IterableExtensions.<IHandle>head(((Iterable<IHandle>)Conversions.doWrapArray(_children)));
        IErlComment _xifexpression = null;
        if ((first instanceof IErlComment)) {
          _xifexpression = ((IErlComment)first);
        } else {
          _xifexpression = null;
        }
        _xblockexpression = _xifexpression;
      }
      return _xblockexpression;
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  public Iterable<IErlAttribute> getAttributes() {
    try {
      return (Iterable<IErlAttribute>)Conversions.doWrapArray(this.<IErlAttribute>getChildren(IErlAttribute.class));
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  public Iterable<IErlAttribute> getAttributesWithTag(final String tag) {
    Iterable<IErlAttribute> _attributes = this.getAttributes();
    final Function1<IErlAttribute, Boolean> _function = new Function1<IErlAttribute, Boolean>() {
      public Boolean apply(final IErlAttribute it) {
        String _name = it.getName();
        return Boolean.valueOf(Objects.equal(_name, tag));
      }
    };
    return IterableExtensions.<IErlAttribute>filter(_attributes, _function);
  }
  
  public Iterable<IErlFunction> getFunctions() {
    try {
      return (Iterable<IErlFunction>)Conversions.doWrapArray(this.<IErlFunction>getChildren(IErlFunction.class));
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  public IErlFunction getFunction(final String name, final int arity) {
    return new ErlFunction(this, name, arity);
  }
  
  public Iterable<IErlError> getErrors() {
    try {
      return (Iterable<IErlError>)Conversions.doWrapArray(this.<IErlError>getChildren(IErlError.class));
    } catch (Throwable _e) {
      throw Exceptions.sneakyThrow(_e);
    }
  }
  
  @Override
  @Pure
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    return result;
  }
  
  @Override
  @Pure
  public boolean equals(final Object obj) {
    if (this == obj)
      return true;
    if (obj == null)
      return false;
    if (getClass() != obj.getClass())
      return false;
    if (!super.equals(obj))
      return false;
    ErlSource other = (ErlSource) obj;
    return true;
  }
  
  @Override
  @Pure
  public String toString() {
    String result = new ToStringBuilder(this)
    	.addAllFields()
    	.toString();
    return result;
  }
}
