package org.erlide.engine.model.root;

import java.util.Collection;

public interface IErlLibrary extends IErlElement {

    Collection<IErlProject> getProjects();

    IErlProject getProject(String name);

}
