# Fortran random solver

This directory contains Fortran solver that generates almost random labyrinths and checks them.
The better solution would be to implement [SAT][sat] assistant generation of labyrinths, but we (or me) did not have a time for that.

The solver is able to solve `probatio` and `primus` in some reasonable time.
Other tasks are too big for random.


## Configure

With OpenMP support:
```console
$ cd fortran/solver
$ cmake -Bbuild -DENABLE_OPENMP=ON -DCMAKE_BUILD_TYPE=Release
```

## Build
```console
$ make -C build
```

## Run
```console
$ ./build/solver <problem-name>
```

This will try to solve the problem with the given name.

[sat]: https://en.wikipedia.org/wiki/SAT_solver
