all: ExampleBench.dll BenchRunner.exe

clean:
	rm -f ExampleBench.dll BenchRunner.exe SharpBench*.dll

fast: SharpBench.Fast.dll
	cp $^ SharpBench.dll

accurate: SharpBench.Accurate.dll
	cp $^ SharpBench.dll 

SharpBench.Fast.dll : SharpBench.fs
	fsharpc -a -d:FAST -o $@ $^

SharpBench.Accurate.dll : SharpBench.fs
	fsharpc -a -o $@ $^

ExampleBench.dll : ExampleBench.fs
	fsharpc -a -r:SharpBench.dll -o $@ $<

BenchRunner.exe : BenchRunner.fs
	fsharpc -r:SharpBench.dll -r:MathNet.Numerics.FSharp.dll -o $@ $<

ExampleBench.dll : SharpBench.dll
BenchRunner.exe : SharpBench.dll
