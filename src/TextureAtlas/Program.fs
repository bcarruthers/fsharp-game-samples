module AtlasUtility

open System
open System.IO
open System.Drawing
open System.Drawing.Imaging
open System.Runtime.InteropServices
open Argu
open Numerics
open Texturing

module Encoding =
    let private lockForWrite (bmp : Bitmap) write =
//        if bmp.PixelFormat <> PixelFormat.Format32bppArgb then 
//            failwithf "Pixel format not supported: %A" bmp.PixelFormat
        let bmpData =
            bmp.LockBits(
                Rectangle(0, 0, bmp.Width, bmp.Height),
                ImageLockMode.WriteOnly, bmp.PixelFormat);
        write bmpData
        bmp.UnlockBits(bmpData)        

    let writeBitmapPixelData bmp (source : byte[]) = 
        lockForWrite bmp <| fun bmpData ->
            let pixelSize = Image.GetPixelFormatSize(bmp.PixelFormat) / 8
            let rowLength = bmp.Width * pixelSize
            for y = 0 to bmp.Height - 1 do
                let ptr = IntPtr.Add(bmpData.Scan0, bmpData.Stride * y)
                Marshal.Copy(source, y * rowLength, ptr, rowLength)

    let private lockForRead (bmp : Bitmap) read =
//        if bmp.PixelFormat <> PixelFormat.Format32bppArgb then 
//            failwithf "Pixel format not supported: %A" bmp.PixelFormat
        let bmpData =
            bmp.LockBits(
                Rectangle(0, 0, bmp.Width, bmp.Height),
                ImageLockMode.ReadOnly, bmp.PixelFormat);
        let result = read bmpData
        bmp.UnlockBits(bmpData)        
        result

    let getBitmapPixelData bmp = 
        lockForRead bmp <| fun bmpData ->
            let pixelSize = Image.GetPixelFormatSize(bmp.PixelFormat) / 8
            let rowLength = bmp.Width * pixelSize
            let output = Array.zeroCreate<byte>(rowLength * bmp.Height)
            for y = 0 to bmp.Height - 1 do
                let ptr = IntPtr.Add(bmpData.Scan0, bmpData.Stride * y)
                Marshal.Copy(ptr, output, y * rowLength, rowLength);
            output

module Padding =
    /// Destination size is source with one pixel margin
    let copyWithRepeat (source : _[]) (dest : _[]) (size : Vec2) pixelSize =
        let destSize = size + Vec2.one * 2
        let mutable sourceIndex = 0
        let mutable destIndex = destSize.x // after one row
        // first, copy interior rows
        for y = 0 to size.y - 1 do
            // copy repeated area at beginning of row
            Array.Copy(source, sourceIndex * pixelSize, dest, destIndex * pixelSize, pixelSize)
            destIndex <- destIndex + 1
            // copy interior
            Array.Copy(source, sourceIndex * pixelSize, dest, destIndex * pixelSize, size.x * pixelSize)
            sourceIndex <- sourceIndex + size.x
            destIndex <- destIndex + size.x
            // copy repeated area at end of row
            Array.Copy(source, (sourceIndex - 1) * pixelSize, dest, destIndex * pixelSize, pixelSize)
            destIndex <- destIndex + 1
        // next, copy from second to first row, then the same for bottom row
        let destArea = destSize.x * destSize.y
        Array.Copy(dest, destSize.x * pixelSize, dest, 0, destSize.x * pixelSize)
        Array.Copy(dest, (destArea - destSize.x * 2) * pixelSize, 
            dest, (destArea - destSize.x) * pixelSize, 
            destSize.x * pixelSize)

    let addPadding (tex : Bitmap) =
        let size = Vec2.init tex.Width tex.Height
        let paddedSize = size + Vec2.one * 2
        let source = Encoding.getBitmapPixelData tex
        let pixelSize = Image.GetPixelFormatSize(tex.PixelFormat) / 8
        let padded = Array.zeroCreate<byte>(paddedSize.x * paddedSize.y * pixelSize)
        copyWithRepeat source padded size pixelSize
        let paddedTex = new Bitmap(paddedSize.x, paddedSize.y, tex.PixelFormat)
        Encoding.writeBitmapPixelData paddedTex padded
        paddedTex

type Arguments =
    | Source of string
    | Output of string
    | Name of string
    | Width of int
    | Height of int
    | Margin of int
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Source _ -> "source directory with images to pack"
            | Output _ -> "output directory to write atlas image and bounds files"
            | Name _ -> "name of output image and atlas files"
            | Width _ -> "width of atlas"
            | Height _ -> "height of atlas"
            | Margin _ -> "margin around each texture"

let packTexture pack file = 
    printfn "Reading %s" file
    use img = Bitmap.FromFile(file) :?> Bitmap
    use padded = Padding.addPadding img
    printfn "  Padded from %dx%d to %dx%d" img.Width img.Height padded.Width padded.Height
    let size = Vec2.init padded.Width padded.Height
    let rect = pack size
    printfn "  Packed"
    match rect with
    | Some r -> 
        //g.DrawImage(padded, Rectangle(r.ix.min, r.iy.min, r.ix.len, r.iy.len))
        printfn "  Packed to %A" r
        Some 
            {   texName = System.IO.Path.GetFileName(file)
                texBounds = r 
                padding = 1 }
    | None -> 
        printfn "  Could not pack"
        None

let runFromParser<'a when 'a :> IArgParserTemplate> argv run =
    printfn "%A" argv
    let parser = ArgumentParser.Create<'a>()
    let results = parser.ParseCommandLine(argv, raiseOnUsage = false)
    if results.IsUsageRequested 
        then printf "%s" (parser.PrintUsage())
        else 
            try
                run results
            with 
            | ex -> printfn "%s" ex.Message

let run argv =
    runFromParser argv (fun results ->
        let source = results.GetResult (<@ Source @>)
        let outputPath = results.GetResult (<@ Output @>, defaultValue = ".")
        let name = results.GetResult (<@ Name @>, defaultValue = "Atlas")
        let width = results.GetResult (<@ Width @>, defaultValue = 1024)
        let height = results.GetResult (<@ Height @>, defaultValue = 1024)
        let margin = results.GetResult (<@ Height @>, defaultValue = 0)        
        // Output is x.png, x.json
        let atlasSize = Vec2.init width height
        let pack = createPacker atlasSize
        let bmp = new Bitmap(width, height)
        use g = Graphics.FromImage(bmp)
        let atlas = {
            atlasSize = atlasSize
            undefinedTexName = "Undefined.png"
            atlasTextures = 
                System.IO.Directory.EnumerateFiles(source)
                |> Seq.choose (fun file -> 
                    printfn "Reading %s" file
                    use img = Bitmap.FromFile(file) :?> Bitmap
                    let padded = Padding.addPadding img
                    printfn "  Padded from %dx%d to %dx%d" img.Width img.Height padded.Width padded.Height
                    let size = Vec2.init padded.Width padded.Height
                    let rect = pack size
                    match rect with
                    | Some r -> 
                        g.DrawImage(padded, Rectangle(r.ix.min, r.iy.min, r.ix.len, r.iy.len))
                        printfn "  Packed to %A" r
                        Some 
                            { texName = System.IO.Path.GetFileName(file)
                              texBounds = r 
                              padding = 1 }
                    | None -> 
                        printfn "  Could not pack"
                        None)
                |> Seq.toList }
        let jsonFile = System.IO.Path.Combine(outputPath, name + ".json")
        let imageFile = System.IO.Path.Combine(outputPath, name + ".png")
        printfn "Writing atlas to %s" jsonFile
        Texturing.writeTextureAtlas jsonFile atlas
        printfn "Writing atlas to %s" imageFile
        bmp.Save(imageFile)
        )

[<EntryPoint>]
let main argv = 
    run argv
    0
