package org.erlide.engine.new_model.internal;

import org.eclipse.core.resources.IFile;
import org.eclipse.xtend.lib.annotations.Data;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;
import org.erlide.engine.new_model.IErlHeader;
import org.erlide.engine.new_model.internal.ErlProject;
import org.erlide.engine.new_model.internal.ErlSource;

@Data
@SuppressWarnings("all")
public class ErlHeader extends ErlSource implements IErlHeader {
  public ErlHeader(final ErlProject parent, final IFile file) {
    super(parent, file);
    String _extension = this.getExtension();
    String _fileExtension = file.getFileExtension();
    boolean _equals = _extension.equals(_fileExtension);
    boolean _not = (!_equals);
    if (_not) {
      throw new IllegalArgumentException();
    }
  }
  
  public String getExtension() {
    return "hrl";
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
    ErlHeader other = (ErlHeader) obj;
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
