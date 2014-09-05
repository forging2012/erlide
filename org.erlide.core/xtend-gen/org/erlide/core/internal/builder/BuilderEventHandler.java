package org.erlide.core.internal.builder;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.google.common.base.Objects;
import com.google.common.eventbus.Subscribe;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.InputOutput;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.Pair;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.erlide.core.builder.BuilderHelper;
import org.erlide.core.internal.builder.BuildNotifier;
import org.erlide.core.internal.builder.BuilderState;
import org.erlide.engine.model.builder.MarkerUtils;
import org.erlide.runtime.events.ErlEvent;
import org.erlide.runtime.events.ErlangEventHandler;
import org.erlide.util.ErlLogger;
import org.erlide.util.erlang.Bindings;
import org.erlide.util.erlang.ErlUtils;

@SuppressWarnings("all")
public class BuilderEventHandler extends ErlangEventHandler {
  private final BuildNotifier notifier;
  
  private final BuilderState state;
  
  private final IProject project;
  
  public BuilderEventHandler(final String backendName, final BuildNotifier notifier, final IProject project, final BuilderState state) {
    super("builder", backendName);
    this.project = project;
    this.notifier = notifier;
    this.state = state;
  }
  
  @Subscribe
  public void handleEvent(final ErlEvent event) {
    String _topic = event.getTopic();
    String _topic_1 = this.getTopic();
    boolean _equals = _topic.equals(_topic_1);
    boolean _not = (!_equals);
    if (_not) {
      return;
    }
    final OtpErlangObject data = event.getEvent();
    final boolean processed = this.process(data);
    if ((!processed)) {
      StringConcatenation _builder = new StringConcatenation();
      _builder.append("??? unhandled build");
      _builder.append(data, "");
      ErlLogger.warn(_builder.toString());
    }
  }
  
  public boolean process(final OtpErlangObject data) {
    boolean _xblockexpression = false;
    {
      final Function1<Pair<String, Function1<Bindings, Boolean>>, Boolean> _function = new Function1<Pair<String, Function1<Bindings, Boolean>>, Boolean>() {
        public Boolean apply(final Pair<String, Function1<Bindings, Boolean>> pair) {
          String _key = pair.getKey();
          Function1<Bindings, Boolean> _value = pair.getValue();
          boolean _match = BuilderEventHandler.match(_key, data, _value);
          return Boolean.valueOf((!_match));
        }
      };
      final Iterable<Pair<String, Function1<Bindings, Boolean>>> r = IterableExtensions.<Pair<String, Function1<Bindings, Boolean>>>dropWhile(this.handlers, _function);
      boolean _isEmpty = IterableExtensions.isEmpty(r);
      _xblockexpression = (!_isEmpty);
    }
    return _xblockexpression;
  }
  
  public static boolean match(final String template, final OtpErlangObject data, final Function1<? super Bindings, ? extends Boolean> callback) {
    Boolean _xtrycatchfinallyexpression = null;
    try {
      Boolean _xblockexpression = null;
      {
        final Bindings b = ErlUtils.match(template, data);
        boolean _tripleEquals = (b == null);
        if (_tripleEquals) {
          return false;
        }
        _xblockexpression = callback.apply(b);
      }
      _xtrycatchfinallyexpression = _xblockexpression;
    } catch (final Throwable _t) {
      if (_t instanceof Exception) {
        final Exception e = (Exception)_t;
        ErlLogger.error(e);
        return false;
      } else {
        throw Exceptions.sneakyThrow(_t);
      }
    }
    return (_xtrycatchfinallyexpression).booleanValue();
  }
  
  private final List<Pair<String, Function1<Bindings, Boolean>>> handlers = Collections.<Pair<String, Function1<Bindings, Boolean>>>unmodifiableList(CollectionLiterals.<Pair<String, Function1<Bindings, Boolean>>>newArrayList(Pair.<String, Function1<Bindings, Boolean>>of("{phase,Tag}", new Function1<Bindings, Boolean>() {
    public Boolean apply(final Bindings b) {
      try {
        boolean _xblockexpression = false;
        {
          final String tag = b.getAtom("Tag");
          StringConcatenation _builder = new StringConcatenation();
          _builder.append("-- phase ");
          _builder.append(tag, "");
          InputOutput.<String>println(_builder.toString());
          _xblockexpression = BuilderEventHandler.this.processPhase(tag);
        }
        return Boolean.valueOf(_xblockexpression);
      } catch (Throwable _e) {
        throw Exceptions.sneakyThrow(_e);
      }
    }
  }), Pair.<String, Function1<Bindings, Boolean>>of("{step,Tag, Num}", new Function1<Bindings, Boolean>() {
    public Boolean apply(final Bindings b) {
      try {
        boolean _xblockexpression = false;
        {
          final String tag = b.getString("Tag");
          final int num = b.getInt("Num");
          StringConcatenation _builder = new StringConcatenation();
          _builder.append("-- step ");
          _builder.append(tag, "");
          _builder.append(" ");
          _builder.append(num, "");
          InputOutput.<String>println(_builder.toString());
          _xblockexpression = BuilderEventHandler.this.processStep(tag, num);
        }
        return Boolean.valueOf(_xblockexpression);
      } catch (Throwable _e) {
        throw Exceptions.sneakyThrow(_e);
      }
    }
  }), Pair.<String, Function1<Bindings, Boolean>>of("{compiled,File,Msgs,Deps}", new Function1<Bindings, Boolean>() {
    public Boolean apply(final Bindings b) {
      try {
        boolean _xblockexpression = false;
        {
          final String file = b.getString("File");
          final Collection<OtpErlangObject> msgs = b.getList("Msgs");
          final Collection<OtpErlangObject> deps = b.getList("Deps");
          _xblockexpression = BuilderEventHandler.this.processCompiled(file, msgs, deps);
        }
        return Boolean.valueOf(_xblockexpression);
      } catch (Throwable _e) {
        throw Exceptions.sneakyThrow(_e);
      }
    }
  }), Pair.<String, Function1<Bindings, Boolean>>of("{skipped,File}", new Function1<Bindings, Boolean>() {
    public Boolean apply(final Bindings b) {
      try {
        boolean _xblockexpression = false;
        {
          final String file = b.getString("File");
          _xblockexpression = BuilderEventHandler.this.processSkipped(file);
        }
        return Boolean.valueOf(_xblockexpression);
      } catch (Throwable _e) {
        throw Exceptions.sneakyThrow(_e);
      }
    }
  })));
  
  public boolean processStep(final String step, final int num) {
    this.notifier.newStep(step, num);
    return true;
  }
  
  public boolean processPhase(final String phase) {
    this.notifier.newPhase(phase);
    return true;
  }
  
  public boolean processCompiled(final String file, final Collection<OtpErlangObject> messages, final Collection<OtpErlangObject> dependencies) {
    BuilderEventHandler.createMarkers(this.project, file, messages, dependencies);
    BuilderEventHandler.reloadBeam(this.project, file);
    this.notifier.compiled(file);
    return true;
  }
  
  public boolean processSkipped(final String file) {
    this.notifier.compiled(file);
    return true;
  }
  
  private static void createMarkers(final IProject project, final String filePath, final Collection<OtpErlangObject> messages, final Collection<OtpErlangObject> dependencies) {
    boolean _tripleEquals = (project == null);
    if (_tripleEquals) {
      return;
    }
    final IResource srcFile = project.findMember(filePath);
    MarkerUtils.deleteMarkers(srcFile);
    final Procedure1<OtpErlangObject> _function = new Procedure1<OtpErlangObject>() {
      public void apply(final OtpErlangObject dep) {
        final String depstr = ((OtpErlangString) dep).stringValue();
        final IResource file = project.findMember(depstr);
        MarkerUtils.deleteMarkers(file);
      }
    };
    IterableExtensions.<OtpErlangObject>forEach(dependencies, _function);
    MarkerUtils.addErrorMarkers(srcFile, messages);
  }
  
  private static void reloadBeam(final IProject project, final String filePath) {
    boolean _tripleEquals = (project == null);
    if (_tripleEquals) {
      return;
    }
    final Path path = new Path(filePath);
    final String ext = path.getFileExtension();
    boolean _equal = Objects.equal(ext, "erl");
    if (_equal) {
      IPath _removeFileExtension = path.removeFileExtension();
      final String module = _removeFileExtension.lastSegment();
      BuilderHelper.loadModule(project, module);
    }
  }
}
