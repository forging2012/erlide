/**
 *
 */
package org.erlide.engine.model.root;

import org.erlide.engine.model.IOpenable;

/**
 * @author jakob
 *
 * @noimplement This interface is not intended to be implemented by clients.
 *
 */
public interface IErlExternal extends IErlElement, IOpenable {

    boolean isOTP();

    boolean hasIncludes();
}
