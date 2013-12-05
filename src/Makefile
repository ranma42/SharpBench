TARGETS = \
	ExampleBench.dll \
	ArrayBench.Core.dll \
	ArrayBench.Impl.dll \
	BenchRunner.exe \
	$(NULL)

all: $(TARGETS)

clean:
	rm -f $(TARGETS) SharpBench*.dll

SharpBench.dll : SharpBench.fs
	fsharpc -a -o $@ $^

BenchRunner.exe : BenchRunner.fs SharpBench.dll
	fsharpc -r:SharpBench.dll -r:MathNet.Numerics.FSharp.dll -o $@ $<


ExampleBench.dll : ExampleBench.fs SharpBench.dll
	fsharpc -a -r:SharpBench.dll -o $@ $<

ArrayBench.Core.dll : ArrayBench.fs
	fsharpc -a -d:CORE -r:SharpBench.dll -o $@ $<

ArrayBench.Impl.dll : ArrayBench.fs
	fsharpc -a -r:SharpBench.dll -o $@ $<
