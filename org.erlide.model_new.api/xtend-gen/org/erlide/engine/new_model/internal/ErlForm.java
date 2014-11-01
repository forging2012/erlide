package org.erlide.engine.new_model.internal;

import org.eclipse.handly.model.impl.Handle;
import org.eclipse.handly.model.impl.HandleManager;
import org.eclipse.handly.model.impl.SourceConstruct;
import org.eclipse.xtend.lib.annotations.Data;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;
import org.erlide.engine.new_model.IErlForm;
import org.erlide.engine.new_model.internal.ErlModelManager;

@Data
@SuppressWarnings("all")
public abstract class ErlForm extends SourceConstruct implements IErlForm {
  protected HandleManager getHandleManager() {
    return ErlModelManager.INSTANCE.getHandleManager();
  }
  
  public ErlForm(final Handle parent, final String name) {
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
    ErlForm other = (ErlForm) obj;
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
