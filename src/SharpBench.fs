namespace SharpBench

  open System

  module Core =

#if FAST
    let private defaultSamples = 100
    let private defaultTime = 1.0
#else
    let private defaultSamples = 10000
    let private defaultTime = 100.0
#endif

    [<AttributeUsage(AttributeTargets.Method,AllowMultiple=false)>]
    type BenchAttribute (name:string) =
      inherit Attribute()
      member val Name = name
      member val Samples = defaultSamples with get,set
      member val TargetTime = defaultTime with get,set
