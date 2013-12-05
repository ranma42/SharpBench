namespace SharpBench

  open System

  module Core =

    let private defaultSamples =
      try Int32.Parse(Environment.GetEnvironmentVariable("SHARPBENCH_SAMPLES"))
      with | _ -> 100

    let private defaultTime =
      try Double.Parse(Environment.GetEnvironmentVariable("SHARPBENCH_TIME"))
      with | _ -> 1.0

    [<AttributeUsage(AttributeTargets.Method,AllowMultiple=false)>]
    type BenchAttribute (name:string) =
      inherit Attribute()
      member val Name = name
      member val Samples = defaultSamples with get,set
      member val TargetTime = defaultTime with get,set
