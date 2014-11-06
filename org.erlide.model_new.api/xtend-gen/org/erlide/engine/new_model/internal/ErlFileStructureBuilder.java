package org.erlide.engine.new_model.internal;

import com.google.common.base.Objects;
import java.util.List;
import java.util.Map;
import org.eclipse.handly.model.IHandle;
import org.eclipse.handly.model.impl.Body;
import org.eclipse.handly.model.impl.SourceElementBody;
import org.eclipse.handly.model.impl.StructureHelper;
import org.eclipse.handly.util.TextRange;
import org.erlide.engine.new_model.internal.ASTNode;
import org.erlide.engine.new_model.internal.ErlAttribute;
import org.erlide.engine.new_model.internal.ErlForm;
import org.erlide.engine.new_model.internal.ErlFunction;
import org.erlide.engine.new_model.internal.ErlSource;
import org.erlide.engine.new_model.internal.ErlangAST;

@SuppressWarnings("all")
public class ErlFileStructureBuilder extends StructureHelper {
  private final Map<IHandle, Body> newElements;
  
  private final ErlangAST ast;
  
  public ErlFileStructureBuilder(final Map<IHandle, Body> newElements, final ErlangAST ast) {
    super(newElements);
    this.newElements = newElements;
    this.ast = ast;
  }
  
  public void buildStructure(final ErlSource source, final SourceElementBody parentBody) {
    List<ASTNode> _forms = this.ast.getForms();
    for (final ASTNode node : _forms) {
      {
        ErlForm _switchResult = null;
        String _kind = node.getKind();
        boolean _matched = false;
        if (!_matched) {
          if (Objects.equal(_kind, "attribute")) {
            _matched=true;
            String _name = node.getName();
            _switchResult = new ErlAttribute(source, _name);
          }
        }
        if (!_matched) {
          if (Objects.equal(_kind, "function")) {
            _matched=true;
            String _name_1 = node.getName();
            int _arg = node.getArg();
            _switchResult = new ErlFunction(source, _name_1, _arg);
          }
        }
        final ErlForm handle = _switchResult;
        final SourceElementBody body = new SourceElementBody();
        TextRange _pos = node.getPos();
        body.setFullRange(_pos);
        this.addChild(parentBody, handle, body);
        this.complete(body);
      }
    }
    this.complete(parentBody);
  }
}
