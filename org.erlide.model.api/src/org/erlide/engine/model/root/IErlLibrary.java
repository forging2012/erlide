package org.erlide.engine.model.root;

import java.util.Collection;

import org.erlide.engine.model.IOpenable;

public interface IErlLibrary extends IErlElement, IOpenable {

    Collection<IErlProject> getProjects();

    IErlProject getProject(String name);

}
