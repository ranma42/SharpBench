FSC ?= fsharpc

TARGETS = \
	SharpBench.dll \
	ExampleBench.dll \
	ArrayBench.Core.dll \
	ArrayBench.Impl.dll \
	BenchRunner.exe \
	$(NULL)

all: $(TARGETS)

clean:
	rm -f $(TARGETS)

SharpBench.dll : SharpBench.fs
	$(FSC) -a -o $@ $^

BenchRunner.exe : BenchRunner.fs SharpBench.dll
	$(FSC) -r:SharpBench.dll -r:MathNet.Numerics.FSharp.dll -o $@ $<


ExampleBench.dll : ExampleBench.fs SharpBench.dll
	$(FSC) -a -r:SharpBench.dll -o $@ $<

ArrayBench.Core.dll : ArrayBench.fs
	$(FSC) -a -d:CORE -r:SharpBench.dll -o $@ $<

ArrayBench.Impl.dll : ArrayBench.fs
	$(FSC) -a -r:SharpBench.dll -o $@ $<
