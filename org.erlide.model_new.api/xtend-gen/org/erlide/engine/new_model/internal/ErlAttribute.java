package org.erlide.engine.new_model.internal;

import org.eclipse.handly.model.impl.Handle;
import org.eclipse.xtend.lib.annotations.Data;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;
import org.erlide.engine.new_model.IErlAttribute;
import org.erlide.engine.new_model.IErlExpression;
import org.erlide.engine.new_model.internal.ErlForm;

@Data
@SuppressWarnings("all")
public class ErlAttribute extends ErlForm implements IErlAttribute {
  public Iterable<IErlExpression> getValues() {
    throw new UnsupportedOperationException("auto-generated method stub");
  }
  
  public ErlAttribute(final Handle parent, final String name) {
    super(parent, name);
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
    ErlAttribute other = (ErlAttribute) obj;
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
