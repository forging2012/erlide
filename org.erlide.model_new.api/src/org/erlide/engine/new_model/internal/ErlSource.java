package org.erlide.engine.new_model.internal;

import java.util.Map;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.handly.model.IElementChangeEvent;
import org.eclipse.handly.model.IHandle;
import org.eclipse.handly.model.IHandleDelta;
import org.eclipse.handly.model.impl.Body;
import org.eclipse.handly.model.impl.ElementChangeEvent;
import org.eclipse.handly.model.impl.HandleDelta;
import org.eclipse.handly.model.impl.HandleDeltaBuilder;
import org.eclipse.handly.model.impl.HandleManager;
import org.eclipse.handly.model.impl.SourceElementBody;
import org.eclipse.handly.model.impl.SourceFile;
import org.eclipse.handly.snapshot.NonExpiringSnapshot;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;
import org.erlide.engine.ErlangEngine;
import org.erlide.engine.new_model.IErlAttribute;
import org.erlide.engine.new_model.IErlComment;
import org.erlide.engine.new_model.IErlError;
import org.erlide.engine.new_model.IErlForm;
import org.erlide.engine.new_model.IErlFunction;
import org.erlide.engine.new_model.IErlSource;
import org.erlide.engine.services.parsing.ParserService;
import org.erlide.util.Util;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.google.common.base.Objects;

@SuppressWarnings("restriction")
public abstract class ErlSource extends SourceFile implements IErlSource {

    private class NotifyingReconcileOperation extends SourceFile.ReconcileOperation {
        @Override
        public void reconcile(final Object ast, final NonExpiringSnapshot snapshot,
                final boolean forced) throws CoreException {
            final HandleDeltaBuilder deltaBuilder = new HandleDeltaBuilder(ErlSource.this);
            super.reconcile(ast, snapshot, forced);
            deltaBuilder.buildDelta();
            if (!deltaBuilder.getDelta().isEmpty()) {
                final ElementChangeEvent event = new ElementChangeEvent(
                        IElementChangeEvent.POST_RECONCILE, deltaBuilder.getDelta());
                ErlModelManager.INSTANCE.fireElementChangeEvent(event);
            }
        }
    }

    public ErlSource(final ErlProject parent, final IFile file) {
        super(parent, file);
    }

    @Override
    protected void buildStructure(final SourceElementBody body,
            final Map<IHandle, Body> newElements, final Object ast, final String source) {
        final ErlFileStructureBuilder builder = new ErlFileStructureBuilder(newElements,
                (ErlangAST) ast);
        builder.buildStructure(this, body);
    }

    @Override
    protected Object createStructuralAst(final String text) throws CoreException {
        final String charset = getFile().getCharset();
        return this.parse(text, charset);
    }

    private ErlangAST parse(final String contents, final String encoding) {
        ErlangAST result = null;
        final ParserService parser = ErlangEngine.getInstance().getParserService();
        final OtpErlangObject parsed = parser.parse(createScannerName(), contents);
        if (Util.isOk(parsed)) {
            final OtpErlangTuple tuple = (OtpErlangTuple) parsed;
            result = new ErlangAST((OtpErlangTuple) tuple.elementAt(1));
        }
        return result;
    }

    @Override
    protected HandleManager getHandleManager() {
        return ErlModelManager.INSTANCE.getHandleManager();
    }

    @Override
    public SourceFile.ReconcileOperation getReconcileOperation() {
        return new ErlSource.NotifyingReconcileOperation();
    }

    @SuppressWarnings("unchecked")
    @Override
    public Iterable<IErlForm> getForms() throws CoreException {
        return (Iterable<IErlForm>) Conversions.doWrapArray(this
                .<IErlForm> getChildren(IErlForm.class));
    }

    @SuppressWarnings("unchecked")
    @Override
    public IErlComment getHeaderComment() {
        try {
            IErlComment _xblockexpression = null;
            {
                final IHandle[] _children = this.getChildren();
                final IHandle first = IterableExtensions
                        .<IHandle> head((Iterable<IHandle>) Conversions
                                .doWrapArray(_children));
                IErlComment _xifexpression = null;
                if (first instanceof IErlComment) {
                    _xifexpression = (IErlComment) first;
                }
                _xblockexpression = _xifexpression;
            }
            return _xblockexpression;
        } catch (final Throwable _e) {
            throw Exceptions.sneakyThrow(_e);
        }
    }

    @SuppressWarnings("unchecked")
    @Override
    public Iterable<IErlAttribute> getAttributes() {
        try {
            return (Iterable<IErlAttribute>) Conversions.doWrapArray(this
                    .<IErlAttribute> getChildren(IErlAttribute.class));
        } catch (final Throwable _e) {
            throw Exceptions.sneakyThrow(_e);
        }
    }

    @Override
    public Iterable<IErlAttribute> getAttributesWithTag(final String tag) {
        final Iterable<IErlAttribute> _attributes = this.getAttributes();
        final Function1<IErlAttribute, Boolean> _function = new Function1<IErlAttribute, Boolean>() {
            @SuppressWarnings("restriction")
            @Override
            public Boolean apply(final IErlAttribute it) {
                final String _name = it.getName();
                return Boolean.valueOf(Objects.equal(_name, tag));
            }
        };
        return IterableExtensions.<IErlAttribute> filter(_attributes, _function);
    }

    @SuppressWarnings("unchecked")
    @Override
    public Iterable<IErlFunction> getFunctions() {
        try {
            return (Iterable<IErlFunction>) Conversions.doWrapArray(this
                    .<IErlFunction> getChildren(IErlFunction.class));
        } catch (final Throwable _e) {
            throw Exceptions.sneakyThrow(_e);
        }
    }

    @Override
    public IErlFunction getFunction(final String aname, final int arity) {
        return new ErlFunction(this, aname, arity);
    }

    @SuppressWarnings("unchecked")
    @Override
    public Iterable<IErlError> getErrors() {
        try {
            return (Iterable<IErlError>) Conversions.doWrapArray(this
                    .<IErlError> getChildren(IErlError.class));
        } catch (final Throwable _e) {
            throw Exceptions.sneakyThrow(_e);
        }
    }

    private String createScannerName() {
        final IResource _resource = this.getResource();
        final boolean _tripleNotEquals = _resource != null;
        if (_tripleNotEquals) {
            final IResource _resource_1 = this.getResource();
            final IPath _fullPath = _resource_1.getFullPath();
            final String _portableString = _fullPath.toPortableString();
            return _portableString.substring(1);
        }
        return "dummy";
    }

    @Override
    public void workingCopyModeChanged() {
        super.workingCopyModeChanged();
        final IHandle _root = this.getRoot();
        final HandleDelta delta = new HandleDelta(_root);
        final boolean _exists = this.file.exists();
        if (_exists) {
            delta.insertChanged(this, IHandleDelta.F_WORKING_COPY);
        } else {
            delta.insertAdded(this, IHandleDelta.F_WORKING_COPY);
        }
        final ElementChangeEvent _elementChangeEvent = new ElementChangeEvent(
                IElementChangeEvent.POST_CHANGE, delta);
        ErlModelManager.INSTANCE.fireElementChangeEvent(_elementChangeEvent);
    }

    @Override
    @Pure
    public String toString() {
        final String result = new ToStringBuilder(this).addAllFields().toString();
        return result;
    }
}
