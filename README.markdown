The `concurrent-extra` package offers among other things the following
selection of synchronisation primitives:

* `Broadcast`: Wake multiple threads by broadcasting a value.

* `Event`: Wake multiple threads by signalling an event.

* `Lock`: Enforce exclusive access to a resource. Also known as a
  binary semaphore or mutex. The package additionally provides an
  alternative that works in the `STM` monad.

* `RLock`: A lock which can be acquired multiple times by the same
  thread. Also known as a reentrant mutex.

* `ReadWriteLock`: Multiple-reader, single-writer locks. Used to
  protect shared resources which may be concurrently read, but only
  sequentially written.

* `ReadWriteVar`: Concurrent read, sequential write variables.

Please consult the API documentation of the individual modules for
more detailed information.

This package was inspired by the concurrency libraries of
[Java](http://download.oracle.com/javase/6/docs/technotes/guides/concurrency/index.html)
and [Python](http://docs.python.org/py3k/library/threading.html).
