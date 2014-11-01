package org.erlide.engine.services.search;

import java.util.Collection;
import java.util.Set;

import org.erlide.engine.model.erlang.IErlModule;

import com.google.common.collect.Sets;

public class ErlSearchScope {

    private final Set<IErlModule> modules;

    public ErlSearchScope() {
        modules = Sets.newHashSet();
    }

    public ErlSearchScope(final IErlModule... ms) {
        modules = Sets.newHashSet();
        for (final IErlModule module : ms) {
            modules.add(module);
        }
    }

    public void addModule(final IErlModule module) {
        modules.add(module);
    }

    public Collection<IErlModule> getModules() {
        return modules;
    }

    public int size() {
        return modules.size();
    }
}
