namespace ExampleBenchmark

  open SharpBench.Core

  module BenchModule =
    [<Bench("NoOp")>]
    let noop () =
      ()

    [<Bench("List Initialization")>]
    let fwithobjs () =
      List.init 1000 id |> ignore

    [<Bench("Naive Fibonacci(17)")>]
    let fnoobjs () =
      let rec fib x =
        if x < 1 then
          x
        else
          fib (x-1) + fib (x-2)
      fib 17 |> ignore

    let mutable size = 0.0
    [<Bench("Function which takes longer and longer as you call it")>]
    let sidef () =
      List.init (int(size)) id |> ignore
      size <- size + 0.01

    let proc = System.Diagnostics.Process.GetCurrentProcess()
    [<Bench("Function with inconsistent timing")>]
    let noisyf () =
      let t = proc.TotalProcessorTime.TotalMilliseconds
      let s = t + 0.03 * float(1 + proc.TotalProcessorTime.Seconds % 2)
      while proc.TotalProcessorTime.TotalMilliseconds < s do
        ()
