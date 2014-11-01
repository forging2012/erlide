/* CallableAdapter.java
 *
 * Copyright 2009-2012 Comcast Interactive Media, LLC.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.fishwife.jrugged;

import java.util.concurrent.Callable;

/**
 * Adapter to expose a Runnable as a Callable, with an optional result.
 *
 * @param <R>
 */

public class CallableAdapter<R> implements Callable<R> {

    private Runnable runnable;
    private R result;

    public CallableAdapter(final Runnable runnable) {
        this(runnable, null);
    }

    public CallableAdapter(final Runnable runnable, final R result) {
        this.runnable = runnable;
        this.result = result;
    }

    @Override
    public R call() throws Exception {
        runnable.run();
        return result;
    }
}
