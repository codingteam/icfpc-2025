icfpc-2025
==========
This is the Codingteam's solution for the [ICFPC Programming Contest 2025][icfpc-2025].

Team Members
------------
TBD.

Prerequisites
-------------
- Any JDK 21 distribution. If you don't know which one to use, use [Temurin][temurin].
- [SBT][sbt] (any recent version should suffice, it will auto-download the correct one).
- (Optional) For Fortran solver, any Fortran compiler supporting Fortran 2008 features (or their set).

Configure
---------
Only for Fortran (with OpenMP support):
```console
$ cd fortran/solver
$ cmake -Bbuild -DENABLE_OPENMP=ON -DCMAKE_BUILD_TYPE=Release
```

Build
-----
Scala:
```console
$ sbt compile
```
or Fortran:
```console
$ make -C build
```

Run
---
```console
$ sbt -warn "run solve <solver-name> <problem-name>"
```

This will try to solve the problem with the given name using solver.

Available solvers:
- solver
- sat

(Add `-warn` to suppress informational output from SBT.)

Fortran solver:
```console
$ ./build/solver <problem-name>
```

This will try to solve the problem with the given name using Fortran solver.

Test
---
```console
$ sbt test
```

[icfpc-2025]: https://icfpcontest2025.github.io/
[sbt]: https://www.scala-sbt.org/
[temurin]: https://adoptium.net/temurin/releases/
