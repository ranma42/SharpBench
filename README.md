Bench
=====

SharpBench is a simple benchmarking system for Mono/.Net.

It is written in F# and depends on [math.net
numerics](http://numerics.mathdotnet.com).

# Building

    $ cd src
    $ make fast all

# Running

BenchRunner can load an assembly and run the (static, unit->unit)
methods marked with the `Bench` attribute.  It accepts multiple
assemblies and verbosity can be controlled with the `SHARPBENCH_RAW`
and `SHARPBENCH_DETAILED` boolean flags:

    $ SHARPBENCH_DETAILED=true mono BenchRunner.exe ExampleBench.dll
    $ SHARPBENCH_RAW=true mono BenchRunner.exe ExampleBench.dll ArrayBench.dll > bench.dat

* SHARPBENCH_DETAILED defaults to false; when set to true it will
  provide an explanation of the numerical results of the benchmark

* SHARPBENCH_RAW defaults to false; when set to true it will cause
  BenchRunner to dump all of the measurements in CSV and the computed
  results in lines which begin with a '#'
