module Performance =
    open System

    [<Struct>]
    type GCState = {
        totalMemory : int64
        gc0 : int
        gc1 : int
        gc2 : int
        }

    module GCState =
        let zero = {
            totalMemory = 0L
            gc0 = 0
            gc1 = 0
            gc2 = 0
            }

        let retrieve() = {
            totalMemory = GC.GetTotalMemory(false)
            gc0 = GC.CollectionCount(0)
            gc1 = GC.CollectionCount(1)
            gc2 = GC.CollectionCount(2)
            }

        let isNonZeroGC state =
            state.gc0 > 0 ||
            state.gc1 > 0 ||
            state.gc2 > 0

        let getDelta a b = {
            totalMemory = b.totalMemory - a.totalMemory
            gc0 = b.gc0 - a.gc0
            gc1 = b.gc1 - a.gc1
            gc2 = b.gc2 - a.gc2
            }
            
        let formatWithDelta current delta =
            sprintf "gen0 %d (%+d), gen1 %d (%+d), gen2 %d (%+d), mem %d (%+d)" 
                current.gc0 delta.gc0 current.gc1 delta.gc1 current.gc2 delta.gc2 
                current.totalMemory delta.totalMemory

        let runMonitor() =
            let interval = 1000
            let startTime = DateTime.UtcNow
            let mutable priorTime = startTime
            let mutable prior = retrieve()
            printfn "%s" <| formatWithDelta prior zero
            new System.Threading.Timer((fun _ ->             
                let state = retrieve()
                let delta = getDelta prior state
                if isNonZeroGC delta then
                    let time = DateTime.UtcNow
                    let deltaTime = time - priorTime
                    let elapsedTime = time - startTime
                    printfn "GC ms %d (%+d): %s%s" 
                        (int elapsedTime.TotalMilliseconds)
                        (int deltaTime.TotalMilliseconds)
                        (formatWithDelta state delta)
                        (if delta.gc2 > 0 then " **" elif delta.gc1 > 0 then " *" else "")
                    priorTime <- time
                    prior <- state
                ), null, interval, interval)
            :> IDisposable

    type FpsMonitor() =
        let fpsInterval = 1000
        let mutable lastTime = DateTime.UtcNow
        let mutable elapsed = 0
        let mutable count = 0
        let mutable maxDuration = 0
        member c.Update() =
            let time = DateTime.UtcNow
            let deltaTime = int (time - lastTime).TotalMilliseconds
            lastTime <- time
            count <- count + 1
            elapsed <- elapsed + deltaTime
            maxDuration <- max maxDuration deltaTime
            if elapsed >= fpsInterval then
                printfn "FPS: %d, max: %d ms" count maxDuration
                count <- 0
                elapsed <- elapsed - fpsInterval
                maxDuration <- 0

module Build =
    open System.IO

    let init projectDir =
        let is64Bit = System.IntPtr.Size = 8
        let archDir = if is64Bit then "win-x64" else "win-x86"
        let runtimeDir = Path.Combine(projectDir, "../../packages/MonoGame.Framework.DesktopGL.Core/runtimes/" + archDir + "/native")
        let outputDir = Path.Combine(projectDir, "output")
        Directory.CreateDirectory(outputDir) |> ignore
        for file in Directory.EnumerateFiles(runtimeDir) do
            let dest = Path.Combine(outputDir, Path.GetFileName(file))
            if not (File.Exists(dest)) then
                printfn "Copying %s to %s" file dest
                File.Copy(file, dest, false)
        System.Environment.CurrentDirectory <- outputDir

Build.init __SOURCE_DIRECTORY__

#r "netstandard"
#r "../../packages/MonoGame.Framework.DesktopGL.Core/lib/netstandard2.0/MonoGame.Framework.dll"
