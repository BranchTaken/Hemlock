# Runtime

The Hemlock runtime provides many of the services which are typically provided by a time-sharing
operating system. Hemlock's actors are isolated from each other such that a misbehaving actor cannot
typically dominate compute/memory resources. Computation is round-robin time-sliced so that
compute-intensive actors cannot starve other actors. Automatic memory management (garbage
collection) enables strong memory access safety, and Configurable per actor memory limits keep
individual actors from unintentionally dominating memory usage.

- [XXX Executors](executors.md) schedule actors, multiplex I/O, and drive incremental garbage
  collection for the global heap.
- [Work stealing](work_stealing.md) can automatically distribute long-running pure computation
  when there are fewer runnable actors than there are executors.
- [XXX Messaging](messaging.md) between actors is the only mechanism for communication, though
  messages can refer to the global heap.
- [Automatic memory management](memory.md) which utilizes various garbage collection technologies is
  highly scalable and provides strong memory safety. Each actor has an isolated heap that requires
  negligible synchronization with the rest of the system. The global heap enables multi-actor
  collaboration on arbitrarily large data structures.
