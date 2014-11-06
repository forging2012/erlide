package org.erlide.engine.new_model.internal;

import java.util.Map;
import org.eclipse.handly.model.IHandle;
import org.eclipse.handly.model.impl.Body;
import org.eclipse.handly.model.impl.SourceElementBody;
import org.eclipse.handly.model.impl.StructureHelper;
import org.eclipse.handly.util.TextRange;
import org.erlide.engine.new_model.internal.ErlAttribute;
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
    final ErlAttribute handle = new ErlAttribute(source, "hello");
    final SourceElementBody body = new SourceElementBody();
    TextRange _textRange = new TextRange(0, 1);
    body.setFullRange(_textRange);
    this.addChild(parentBody, handle, body);
    this.complete(body);
    this.complete(parentBody);
  }
}
