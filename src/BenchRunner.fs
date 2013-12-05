namespace SharpBench

  open MathNet.Numerics.LinearAlgebra.Generic
  open MathNet.Numerics.LinearAlgebra.Double
  open System.Reflection
  open SharpBench.Core
  open System

  module Analysis =
    let private averageRows x =
      let secondarg _ v = v
      (Matrix.sumRowsBy secondarg x) / float(x.RowCount)

    let private cov2 x y =
      let xm = averageRows x
      let ym = averageRows y
      let nm1 = x.RowCount - 1
      let r = new DenseMatrix(xm.Count, ym.Count)
      for i = 0 to nm1 do
        let x0 = x.Row(i) - xm
        let y0 = y.Row(i) - ym
        let xy = x0.OuterProduct(y0)
        r.Add(xy, r)
      r.Divide(float(nm1))

    let private cov x = (cov2 x x).Diagonal()

    let private corr x y =
      let vx = cov x
      let vy = cov y
      let c = cov2 x y
      let f i j v = v / sqrt(vx.At(i) * vy.At(j))
      Matrix.mapi f c

    let private partiallinearfit (x:Matrix<_>) (y:Vector<_>) =
      let n = x.ColumnCount
      let mutable x = x
      let mutable qr = x.QR()
      while not qr.IsFullRank do
        x <- x.SubMatrix(0, x.RowCount, 0, x.ColumnCount - 1)
        qr <- x.QR()
      let beta = qr.Solve(y) |> Seq.toArray
      Array.concat [ beta ; Array.zeroCreate (n - beta.Length) ]

    let private r2 (x:Matrix<_>) (y:Vector<_>) beta =
      let n = x.RowCount
      let p = x.ColumnCount
      let dft = n - 1
      let dfe = n - p - 1
      let sstot = y.Subtract(Seq.average y)
      let sstot = sstot * sstot
      let sserr = x.Multiply(DenseVector.ofSeq beta) - y
      let sserr = sserr * sserr
      1.0 - (sserr / float(dfe)) / (sstot / float(dft))

    let private analyze x y =
      let x = DenseMatrix.ofArray2 x
      let y = DenseVector.ofSeq y
      let xavg = averageRows x |> Seq.toArray
      let c = corr x (y.ToColumnMatrix())
      let c = c.Column(0) |> Seq.toArray
      let beta = partiallinearfit x y
      let r2 = r2 x y beta
      r2,beta,xavg,c


    let private predictors = 5

    let private proc = System.Diagnostics.Process.GetCurrentProcess()
    let mutable private lastT = 0.0
    let mutable private lastC0 = 0
    let mutable private lastC1 = 0

    let private getT () =
      let t = proc.TotalProcessorTime.TotalMilliseconds
      let dt  = t - lastT
      lastT <- t
      dt

    let private getC0 () =
      let c0 = GC.CollectionCount(0)
      let dC0 = c0 - lastC0
      lastC0 <- c0
      dC0

    let private getC1 () =
      let c1 = GC.CollectionCount(1)
      let dC1 = c1 - lastC1
      lastC1 <- c1
      dC1

    let private saveMeasures (x:_[,]) (y:_[]) sample iters =
      x.[sample,0] <- sample
      x.[sample,1] <- iters
      x.[sample,2] <- 1
      x.[sample,3] <- getC0 ()
      x.[sample,4] <- getC1 ()
      y.[sample]   <- getT ()


    let mutable private startT = 0.0
    let mutable private endT = 0.0

    let private maxIters = float Int32.MaxValue
    let private minQ = 2.0 / maxIters

    let private rand = new Random()
    let private nextIters totIters remainingSamples = 
      let remainingTime = endT - lastT |> max 1.0
      let elapsedTime = lastT - startT
      (*
      let remainingTimePerSample = remainingTime / float(remainingSamples)
      let avgTimePerIter = elapsedTime / float(totIters)
      let desiredItersPerSample = remainingTimePerSample / avgTimePerIter
      let p = 1.0 / desiredItersPerSample
      *)
      let p = float(remainingSamples) * elapsedTime / (float(totIters) * remainingTime)
      let q = 1.0 - p |> max minQ
      let n = Math.Log(1.0 - rand.NextDouble(), q)
      if n < 0.0 then
        printfn "%A %A" totIters remainingSamples 
        printfn "%A %A" elapsedTime remainingTime
        printfn "%A %A" q n
        sprintf "Unexpected error (total iters: %A, remaining samples: %A, elapsed ms: %A, remaining ms: %A; q: %A, n: %A"
          totIters remainingSamples elapsedTime remainingTime q n
        |> failwith
      else if n >= maxIters then
        Int32.MaxValue
      else
        int(n)

    let private run detailed raw_data (attr:BenchAttribute) (d:Action) =
      let samples = attr.Samples
      let r2_threshold = 0.95
      let ind_coeff_threshold = 0.01
      let ind_corr_threshold = 0.05
      if detailed then
        printfn "Benchmarking method '%s'..." attr.Name

      let x = Array2D.zeroCreate samples predictors
      let y = Array.zeroCreate samples

      d.Invoke()
      GC.Collect()
      saveMeasures x y 0 0
      startT <- lastT
      endT <- startT + attr.TargetTime * 1000.0

      let mutable iters = 1
      let mutable totIters = 0L
      for sample = 0 to samples-1 do
        for j = 1 to iters do
          d.Invoke()
        saveMeasures x y sample iters
        totIters <- totIters + int64(iters)
        iters <- samples - sample |> nextIters totIters

      let r2,beta,gamma,corr = analyze (x |> Array2D.map float) y

      if raw_data then
        for r = 0 to samples-1 do
          for c = 0 to predictors-1 do
            printf "%A, " x.[r,c]
          printfn "%A" y.[r]
        printfn "# R^2 = %A" r2
        printfn "# beta = %A" beta
        printfn "# gamma = %A" gamma
        printfn "# correlation = %A" corr
        printfn ""

      let mutable accurate = true

      if not (r2 > r2_threshold) then
        accurate <- false
        if detailed then
          printfn "Warning: the measurements do not seem to fit a linear model."
          printfn "The adjusted coefficient of determination (R^2) of the linear fit is %3.3g." r2
          printfn "This might mean that there are some aspects of the computation which are"
          printfn "currently not taken into account."
          printfn ""

      if not (abs beta.[0] < ind_coeff_threshold && abs corr.[0] < ind_corr_threshold) then
        accurate <- false
        if detailed then
          printfn "Warning: the measurements seem to depend on the benchmark iteration."
          printfn "There is a correlation of %3.3g %% between the index of the sample being" (corr.[0]*100.0)
          printfn "measured and the resulting time (the linear dependence has a coefficient"
          printfn "of %g). This might mean that the computation has some side-effects" beta.[0]
          printfn "which are not being benchmarked explicitly."
          printfn ""

      let avgT = (Array.map2 (*) beta gamma |> Array.sum) / gamma.[1]

      let labels = [ null; "invocations"; null; "minor GCs" ; "major GCs" ]
      if detailed then
        if not accurate then
          printfn "Warning: results might be inaccurate."
          printfn ""

        printfn "%4.3g %% of the time spent is explained by the model" (r2 * 100.0)
        printfn "Each iteration requires on average %8.3g ms" avgT

        printfn "\nThe time spent on each component of the computation is:"
        for v,name in Seq.zip beta labels do
          if name <> null then
            printfn " - %8.3g ms in each of the %s" v name

        printfn "\nEach invocation of the function corresponds (on average) to:"
        for v,name in Seq.zip gamma labels do
          if name <> null then
            printfn " - %8.3g %s" (v/gamma.[1]) name

        printfn "\nThe measured time is correlated with each component as follows:"
        for v,name in Seq.zip corr labels do
          if name <> null then
            printfn " - %8.3g %% with %s" (v*100.0) name

        printfn ""
      else if not raw then
        printf "%4.3g" (r2 * 100.0)
        printf " %8.3g" avgT
        for v,name in Seq.zip beta labels do
          if name <> null then
            printf " %8.3g" v
        for v,name in Seq.zip gamma labels do
          if name <> null then
            printf " %8.3g" (v/gamma.[1])
        for v,name in Seq.zip corr labels do
          if name <> null then
            printf " %8.3g" (v*100.0)
        if not accurate then
          printf " *"
        else
          printf "  "
        printfn " %s" attr.Name

    [<EntryPoint>]
    let main args =
      let mutable detailed = false
      let mutable raw_data = false

      try detailed <- Boolean.Parse(Environment.GetEnvironmentVariable("SHARPBENCH_DETAILED"))
      with | _ -> ()

      try raw_data <- Boolean.Parse(Environment.GetEnvironmentVariable("SHARPBENCH_RAW"))
      with | _ -> ()

      for name in args do
        let assembly = Assembly.LoadFrom(name)
        let benchs =
          assembly.GetTypes()
          |> Array.collect (fun t -> t.GetMethods(BindingFlags.Static + BindingFlags.Public))
          |> Array.filter (fun m -> m.GetCustomAttribute<BenchAttribute>() |> box <> null)
        for m in benchs do
          let attr = m.GetCustomAttribute<BenchAttribute>()
          let d = m.CreateDelegate(typeof<Action>) :?> Action
          run detailed raw_data attr d
      0
