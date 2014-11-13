package org.erlide.core.internal.builder;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.google.common.base.Objects;
import com.google.common.eventbus.Subscribe;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.xtend2.lib.StringConcatenation;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.eclipse.xtext.xbase.lib.Conversions;
import org.eclipse.xtext.xbase.lib.Exceptions;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.InputOutput;
import org.eclipse.xtext.xbase.lib.IterableExtensions;
import org.eclipse.xtext.xbase.lib.Pair;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.erlide.core.builder.BuilderHelper;
import org.erlide.core.internal.builder.BuildNotifier;
import org.erlide.core.internal.builder.BuilderCanceledException;
import org.erlide.core.internal.builder.BuilderState;
import org.erlide.engine.model.builder.MarkerUtils;
import org.erlide.engine.util.ResourceUtil;
import org.erlide.runtime.events.ErlEvent;
import org.erlide.runtime.events.ErlangEventHandler;
import org.erlide.util.ErlLogger;
import org.erlide.util.erlang.Bindings;
import org.erlide.util.erlang.OtpErlang;

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
        final Bindings b = OtpErlang.match(template, data);
        boolean _tripleEquals = (b == null);
        if (_tripleEquals) {
          return false;
        }
        _xblockexpression = callback.apply(b);
      }
      _xtrycatchfinallyexpression = _xblockexpression;
    } catch (final Throwable _t) {
      if (_t instanceof BuilderCanceledException) {
        final BuilderCanceledException e = (BuilderCanceledException)_t;
        ErlLogger.info("Build job canceled");
        return true;
      } else if (_t instanceof Exception) {
        final Exception e_1 = (Exception)_t;
        ErlLogger.error(e_1);
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
  }), Pair.<String, Function1<Bindings, Boolean>>of("{messages,Msgs}", new Function1<Bindings, Boolean>() {
    public Boolean apply(final Bindings b) {
      try {
        boolean _xblockexpression = false;
        {
          final Collection<OtpErlangObject> msgs = b.getList("Msgs");
          _xblockexpression = BuilderEventHandler.this.processMessages(msgs);
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
    boolean _equals = Objects.equal(phase, "clean");
    if (_equals) {
      MarkerUtils.deleteMarkers(this.project);
    }
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
  
  public boolean processMessages(final Collection<OtpErlangObject> messages) {
    Iterable<OtpErlangObject> _cleanup = this.cleanup(messages);
    ArrayList<OtpErlangObject> _newArrayList = CollectionLiterals.<OtpErlangObject>newArrayList();
    BuilderEventHandler.createMarkers(this.project, null, _cleanup, _newArrayList);
    return true;
  }
  
  public Iterable<OtpErlangObject> cleanup(final Collection<OtpErlangObject> objects) {
    Iterable<OtpErlangObject> _xblockexpression = null;
    {
      InputOutput.<String>println("cleanup");
      final Function1<OtpErlangObject, OtpErlangObject> _function = new Function1<OtpErlangObject, OtpErlangObject>() {
        public OtpErlangObject apply(final OtpErlangObject it) {
          try {
            OtpErlangObject _xblockexpression = null;
            {
              final Bindings b = OtpErlang.match("{project,N,Msg,error}", it);
              boolean _tripleEquals = (b == null);
              if (_tripleEquals) {
                return it;
              }
              final long n = b.getLong("N");
              final String msg = b.getString("Msg");
              final String HDR = "Failed to extract name from ";
              boolean _startsWith = msg.startsWith(HDR);
              boolean _not = (!_startsWith);
              if (_not) {
                return it;
              }
              int _length = HDR.length();
              final String last = msg.substring(_length);
              final int ix = last.indexOf(": ");
              if ((ix <= 0)) {
                return it;
              }
              final String file = last.substring(0, ix);
              String _trim = file.trim();
              final IResource res = ResourceUtil.findResourceByLocation(BuilderEventHandler.this.project, _trim);
              IPath _projectRelativePath = null;
              if (res!=null) {
                _projectRelativePath=res.getProjectRelativePath();
              }
              final IPath newFile = _projectRelativePath;
              boolean _tripleEquals_1 = (newFile == null);
              if (_tripleEquals_1) {
                return it;
              }
              String _substring = last.substring((ix + 1));
              final String newMessage = (((HDR + newFile) + ": ") + _substring);
              _xblockexpression = OtpErlang.format("{project,~i,~s,error}", Long.valueOf(n), newMessage);
            }
            return _xblockexpression;
          } catch (Throwable _e) {
            throw Exceptions.sneakyThrow(_e);
          }
        }
      };
      _xblockexpression = IterableExtensions.<OtpErlangObject, OtpErlangObject>map(objects, _function);
    }
    return _xblockexpression;
  }
  
  private static void createMarkers(final IProject project, final String filePath, final Iterable<OtpErlangObject> messages, final Collection<OtpErlangObject> dependencies) {
    boolean _tripleEquals = (project == null);
    if (_tripleEquals) {
      return;
    }
    IResource _xifexpression = null;
    boolean _tripleEquals_1 = (filePath == null);
    if (_tripleEquals_1) {
      _xifexpression = project;
    } else {
      _xifexpression = project.findMember(filePath);
    }
    final IResource srcFile = _xifexpression;
    boolean _notEquals = (!Objects.equal(srcFile, project));
    if (_notEquals) {
      MarkerUtils.deleteMarkers(srcFile);
    }
    final Procedure1<OtpErlangObject> _function = new Procedure1<OtpErlangObject>() {
      public void apply(final OtpErlangObject dep) {
        final String depstr = ((OtpErlangString) dep).stringValue();
        final IResource file = project.findMember(depstr);
        MarkerUtils.deleteMarkers(file);
      }
    };
    IterableExtensions.<OtpErlangObject>forEach(dependencies, _function);
    MarkerUtils.addErrorMarkers(srcFile, ((OtpErlangObject[])Conversions.unwrapArray(messages, OtpErlangObject.class)));
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
