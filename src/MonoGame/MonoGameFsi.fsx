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
