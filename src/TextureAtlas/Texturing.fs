module Texturing

open System
open System.Collections.Generic
open Newtonsoft.Json
open Numerics

// http://wiki.unity3d.com/index.php?title=MaxRectsBinPack
module private Packing =
    let private isContainedIn (a : Rect) (b : Rect) =
        a.ix.min >= b.ix.min && 
        a.iy.min >= b.iy.min &&
        a.ix.max <= b.ix.max &&
        a.iy.max <= b.iy.max
    
    let private isOverlapping (a : Rect) (b : Rect) =
        a.ix.min >= b.ix.max ||
        a.ix.max <= b.ix.min ||
        a.iy.min >= b.iy.max ||
        a.iy.max <= b.iy.min

    type MaxRectsBinPack(size : Vec2) =
        let w = max 0 size.x
        let h = max 0 size.y
        let n = Rect.bounds Vec2.zero (Vec2.init w h)
        let usedRects = List<Rect>()
        let freeRects = List<Rect>([ n ])    
 
        member c.Insert(size : Vec2) =
            if size.x <= 0 || size.y <= 0 then None
            else
                c.FindPositionForNewNodeBestAreaFit(size)
                |> Option.map c.PlaceRect

        member private c.PlaceRect(newNode) =
            let mutable n = freeRects.Count
            let mutable i = 0
            while i < n do
                if c.SplitFreeNode(freeRects.[i], newNode) then
                    freeRects.RemoveAt(i)
                    i <- i - 1
                    n <- n - 1
                i <- i + 1
            c.PruneFreeList()
            usedRects.Add(newNode)
            newNode
            
        member private c.FindPositionForNewNodeBestAreaFit(size : Vec2) = //(int width, int height, ref int bestAreaFit, ref int bestShortSideFit) 
            let mutable bestNode = None 
            let mutable bestAreaFit = Int64.MaxValue 
            let mutable bestShortSideFit = Int32.MaxValue 
            for rect in freeRects do
                let areaFit = int64 rect.ix.len * int64 rect.iy.len - int64 size.x * int64 size.y 
                // Try to place the rectangle in upright (non-flipped) orientation.
                if (rect.ix.len >= size.x && rect.iy.len >= size.y) then
                    let leftoverHoriz = abs (rect.ix.len - size.x)
                    let leftoverVert = abs (rect.iy.len - size.y)
                    let shortSideFit = min leftoverHoriz leftoverVert 
                    if areaFit < bestAreaFit || (areaFit = bestAreaFit && shortSideFit < bestShortSideFit) then
                        bestNode <- Some (Rect.sized rect.min size)
                        bestShortSideFit <- shortSideFit
                        bestAreaFit <- areaFit                                      
            bestNode
 
        member private c.SplitFreeNode(free : Rect, used : Rect) =
            // Test with SAT if the rectangles even intersect.
            if isOverlapping used free then false
            else 
                if (used.ix.min < free.ix.max && used.ix.max > free.ix.min) then
                    // New node at the top side of the used node.
                    if (used.iy.min > free.iy.min && used.iy.min < free.iy.max) then
                        freeRects.Add(Rect.init free.ix (Interval.sized free.iy.min (used.iy.min - free.iy.min)))
                    // New node at the bottom side of the used node.
                    if (used.iy.max < free.iy.max) then
                        freeRects.Add(Rect.init free.ix (Interval.sized used.iy.max (free.iy.max - (used.iy.max))))
                if (used.iy.min < free.iy.max && used.iy.max > free.iy.min) then
                    // New node at the left side of the used node.
                    if (used.ix.min > free.ix.min && used.ix.min < free.ix.max) then
                        freeRects.Add(Rect.init (Interval.sized free.ix.min (used.ix.min - free.ix.min)) free.iy)
                    // New node at the right side of the used node.
                    if (used.ix.max < free.ix.max) then
                        freeRects.Add(Rect.init (Interval.sized used.ix.max (free.ix.max - (used.ix.max))) free.iy)
                true
 
        member private c.PruneFreeList() =
            let mutable isDone = false
            let mutable i = 0
            while not isDone && i < freeRects.Count do
                let mutable j = i + 1
                while not isDone && j < freeRects.Count do
                    if isContainedIn freeRects.[i] freeRects.[j] then
                        freeRects.RemoveAt(i)
                        i <- i - 1
                        isDone <- true                
                    elif isContainedIn freeRects.[j] freeRects.[i] then
                        freeRects.RemoveAt(j)
                        j <- j - 1
                    j <- j + 1
                i <- i + 1

/// Stateful
let createPacker atlasSize =
    Packing.MaxRectsBinPack(atlasSize).Insert    

let packItems atlasSize itemSizes =
    let pack = createPacker atlasSize
    itemSizes |> Seq.map (fun (item, size) -> item, pack size)

let pack atlasSize sizes =
    let pack = createPacker atlasSize
    sizes |> Seq.map pack

type TextureEntry = {
    texName : string
    texBounds : Rect
    padding : int }

type TextureAtlas = {
    atlasSize : Vec2
    undefinedTexName : string
    atlasTextures : TextureEntry list }

type TextureLookup = string -> Rectf

let emptyTextureAtlas = {
    atlasSize = Vec2.zero
    undefinedTexName = ""
    atlasTextures = List.empty }

type JsonTextureEntry = {
    name : string
    x : int
    y : int
    width : int
    height : int
    padding : int
}

type JsonTextureAtlas = {
    width : int
    height : int
    undefinedName : string
    textures : JsonTextureEntry list
    }



let private jsonToAtlas (j : JsonTextureAtlas) =     
    { atlasSize = Vec2.init j.width j.height
      undefinedTexName = j.undefinedName
      atlasTextures = 
        j.textures
        |> Seq.map (fun j ->
            { texName = j.name
              padding = j.padding
              texBounds = 
                Rect.sized 
                  (Vec2.init j.x j.y)
                  (Vec2.init j.width j.height)
            })
        |> Seq.toList }

let private texEntryToJson tex =
    { name = tex.texName
      x = tex.texBounds.ix.min
      y = tex.texBounds.iy.min
      width = tex.texBounds.ix.len
      height = tex.texBounds.iy.len
      padding = tex.padding
    }

let private atlasToJson a =
    { width = a.atlasSize.x
      height = a.atlasSize.y
      undefinedName = a.undefinedTexName
      textures = a.atlasTextures |> List.map texEntryToJson 
    }

let getTexBounds atlasSize (tex : TextureEntry) =
    let size = atlasSize |> Vec2f.fromInt
    let padding = Vec2.init tex.padding tex.padding
    let p0 = tex.texBounds.min + padding |> Vec2f.fromInt
    let p1 = tex.texBounds.max - padding |> Vec2f.fromInt
    let x0 = p0.x / size.x
    let y0 = p0.y / size.y
    let x1 = p1.x / size.x
    let y1 = p1.y / size.y
    Rectf.bounds (Vec2f.init x0 y1) (Vec2f.init x1 y0)
    
let getTexBoundsByName atlas name =
    atlas.atlasTextures
    |> List.pick (fun t -> 
        if t.texName = name
        then getTexBounds atlas.atlasSize t |> Some
        else None)

// Converts int coords to float
let atlasToLookup atlas =
    let lut = Dictionary<_,_>()
    for tex in atlas.atlasTextures do
        let nameLower = tex.texName.ToLowerInvariant()
        let bounds = getTexBounds atlas.atlasSize tex
        lut.[nameLower] <- bounds
        lut.[tex.texName] <- bounds
    let undefinedBounds = 
        match lut.TryGetValue atlas.undefinedTexName with
        | true, x -> x
        | false, _ -> Rectf.zero
    fun (key : string) -> 
        match lut.TryGetValue key with
        | true, x -> x
        | false, _ -> undefinedBounds            

let readTextureAtlas file = 
    JsonConvert.DeserializeObject<JsonTextureAtlas>(System.IO.File.ReadAllText(file))
    |> jsonToAtlas

let writeTextureAtlas file atlas = 
    let j = atlas |> atlasToJson
    let str = JsonConvert.SerializeObject(j, Formatting.Indented)
    System.IO.File.WriteAllText(file, str)
