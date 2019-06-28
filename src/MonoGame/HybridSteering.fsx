#load "MonoGameFsi.fsx"

// ----------------------------------------------------------------
// Math types

[<Struct>]
type Vector = {
    x : float
    y : float
}

// ----------------------------------------------------------------
// Math functions

module Scalar =
    let tolerance = 1e-9

    let clamp s0 s1 s =
        s |> max s0 |> min s1

    let linearStep s0 s1 s =
        let length = s1 - s0
        if abs length < tolerance then 0.0
        else clamp 0.0 1.0 ((s - s0) / length)

    let smoothStep s0 s1 s =
        let x = linearStep s0 s1 s
        x * x * (3.0 - 2.0 * x)

module Vector =
    let init x y = {
        x = x
        y = y
    }

    let zero = init 0.0 0.0

    let add a b = {
        x = a.x + b.x
        y = a.y + b.y
    }
    
    let subtract a b = {
        x = a.x - b.x
        y = a.y - b.y
    }

    let multiply c v = {
        x = v.x * c
        y = v.y * c
    }

    let lengthSquared v =
        v.x * v.x + v.y * v.y

    let length v = 
        sqrt (lengthSquared v)

    let divideOrZero c v =
        if abs c > Scalar.tolerance then multiply (1.0 / c) v else init 0.0 0.0

    let normalizeOrZero v =
        divideOrZero (length v) v

    let truncateOrZero maxLength v =
        let lengthSqr = lengthSquared v
        if lengthSqr <= maxLength * maxLength then v
        else normalizeOrZero v |> multiply maxLength

    let radians v =
        atan2 v.y v.x

    let fromRadians r =
        init (cos r) (sin r)

    let rotate a b =
        init (b.x * a.x - b.y * a.y) (b.x * a.y + b.y * a.x)

// ----------------------------------------------------------------
// Domain types

[<Struct>]
type Team =
    | Red
    | Orange
    | Yellow
    | Green
    | Cyan
    | Blue
    | Purple

[<Struct>]
type Vehicle = {
    team : Team
    position : Vector
    direction : Vector
    speed : float
    radius : float
    maxSpeed : float
    trailLifespan : float
}

[<Struct>]
type Neighbor = {
    vehicle : Vehicle
    directionToNeighbor : Vector
    distance : float
    teamWeight : float
}

[<Struct>]
type Neighborhood = {
    current : Vehicle
    neighbors : Neighbor seq
}

[<Struct>]
type Trail = {
    team : Team
    position : Vector
    radians : float
    lifespan : float
}

type SteeringSettings = {
    forwardWeight : float
    cohesionWeight : float
    tetherWeight : float
    separationWeight : float
    alignmentWeight : float
    maxAlignmentDistance : float
    maxSeparationDistance : float
    maxCohesionDistance : float
    maxTetherDistance : float
}

type WorldSettings = {
    seed : int
    spawnRange : float
    vehicleCount : int
    maxVehicleSpeed : float
    trailLifespan : float
    steering : SteeringSettings
}

type World = {
    vehicles : Vehicle seq
    trails : Trail seq
}

// ----------------------------------------------------------------
// Domain functions

module Team =
    let teams = [|
        Red
        Orange
        Yellow
        Green
        Cyan
        Blue
        Purple
    |]

module Trail =
    let update deltaTime trail = 
        if trail.lifespan <= 0.0 then None
        else Some {
            trail with
                radians = trail.radians + deltaTime * 0.1
                lifespan = trail.lifespan - deltaTime
        }

module Vehicle =
    let getVelocity vehicle =
        Vector.multiply vehicle.speed vehicle.direction

    let setVelocity newVelocity vehicle =
        let newSpeed = Vector.length newVelocity
        { vehicle with
            speed = newSpeed 
            direction = Vector.divideOrZero newSpeed newVelocity }

    let updatePosition deltaTime vehicle =
        let velocity = getVelocity vehicle
        let delta = Vector.multiply deltaTime velocity
        { vehicle with position = Vector.add vehicle.position delta }

    let spawnTrail (vehicle : Vehicle) = {
        team = vehicle.team
        position = vehicle.position
        radians = Vector.radians vehicle.direction
        lifespan = 0.6
    }

module Neighborhood =
    let getForwardForce neighborhood =
        neighborhood.current.direction

    let getTetherForce maxDistance neighborhood =
        let tetherPoint = Vector.init 0.0 0.0
        let toTether = Vector.subtract tetherPoint neighborhood.current.position
        let distance = Vector.length toTether
        let scale = Scalar.smoothStep 0.0 maxDistance distance
        Vector.divideOrZero distance toTether
        |> Vector.multiply scale

    let getCohesionForce minDistance maxDistance neighborhood =
        let mutable sum = Vector.zero
        for neighbor in neighborhood.neighbors do
            let weight = Scalar.smoothStep minDistance maxDistance neighbor.distance
            sum <- Vector.add sum <| Vector.multiply (neighbor.teamWeight * weight) neighbor.directionToNeighbor
        Vector.normalizeOrZero sum

    let getSeparationForce maxDistance neighborhood =
        let mutable sum = Vector.zero
        for neighbor in neighborhood.neighbors do
            let weight = Scalar.smoothStep maxDistance 0.0 neighbor.distance
            sum <- Vector.add sum <| Vector.multiply -weight neighbor.directionToNeighbor
        sum

    let getAlignmentForce maxDistance neighborhood =
        let mutable sum = Vector.zero
        for neighbor in neighborhood.neighbors do
            let weight = Scalar.smoothStep maxDistance 0.0 neighbor.distance
            sum <- Vector.add sum <| Vector.multiply -(neighbor.teamWeight * weight) neighbor.vehicle.direction
        Vector.normalizeOrZero sum
    
    let getForceCalculator steering =
        let calculators = [|
            getForwardForce >> (Vector.multiply steering.forwardWeight)
            getTetherForce steering.maxTetherDistance >> (Vector.multiply steering.tetherWeight)
            getCohesionForce steering.maxSeparationDistance steering.maxCohesionDistance >> (Vector.multiply steering.cohesionWeight)
            getSeparationForce steering.maxSeparationDistance >> (Vector.multiply steering.separationWeight)
            getAlignmentForce steering.maxAlignmentDistance >> (Vector.multiply steering.alignmentWeight)
            |]
        fun neighborhood ->
            let mutable sum = Vector.zero
            for calc in calculators do
                sum <- Vector.add sum <| calc neighborhood
            Vector.normalizeOrZero sum

open System.Collections.Generic

type WritableWorld(settings) =
    let trails = List<Trail>()
    let getSteeringForce = Neighborhood.getForceCalculator settings.steering

    let vehicles =
        let rand = System.Random(settings.seed)
        let nextCoord() =
            (rand.NextDouble() - 0.5) * settings.spawnRange
        List<_>(Seq.init settings.vehicleCount (fun _ -> {
            maxSpeed = settings.maxVehicleSpeed
            trailLifespan = settings.trailLifespan
            team = Team.teams.[rand.Next Team.teams.Length]
            position = Vector.init (nextCoord()) (nextCoord())
            direction = Vector.init 0.0 1.0
            speed = 0.0
            radius = 1.0 
            }))

    let addNeighbors index (list : List<_>) =
        let current = vehicles.[index]
        for i = 0 to vehicles.Count - 1 do
            if i <> index then
                let vehicle = vehicles.[i]
                let offset = Vector.subtract vehicle.position current.position
                let distance = Vector.length offset
                list.Add { 
                    vehicle = vehicle
                    teamWeight = if current.team = vehicle.team then 1.0 else 0.0
                    directionToNeighbor = Vector.divideOrZero distance offset
                    distance = distance
                }

    let applySteeringForces steering deltaTime =
        let neighbors = List<_>()
        for i = 0 to vehicles.Count - 1 do
            addNeighbors i neighbors
            let neighborhood = {
                current = vehicles.[i]
                neighbors = neighbors
                }
            let newVelocity = getSteeringForce neighborhood |> Vector.multiply neighborhood.current.maxSpeed
            vehicles.[i] <- Vehicle.setVelocity newVelocity neighborhood.current
            neighbors.Clear()

    let updatePositions deltaTime =
        for i = 0 to vehicles.Count - 1 do
            vehicles.[i] <- Vehicle.updatePosition deltaTime vehicles.[i]

    let updateTrails deltaTime =
        let mutable i = 0
        while i < trails.Count do
            match Trail.update deltaTime trails.[i] with
            | Some trail -> 
                trails.[i] <- trail
            | None -> 
                trails.[i] <- trails.[trails.Count - 1]
                trails.RemoveAt(trails.Count - 1)
                i <- i - 1
            i <- i + 1

    let spawnTrails() =
        for vehicle in vehicles do
            trails.Add (Vehicle.spawnTrail vehicle)

    member c.World = {
        vehicles = vehicles :> seq<_>
        trails = trails :> seq<_>
    }
    
    member c.Update deltaTime =
        updateTrails deltaTime
        spawnTrails()
        applySteeringForces settings.steering deltaTime
        updatePositions deltaTime

// ----------------------------------------------------------------
// View types

[<Struct>]
type SpriteType =
    | Triangle
    | Hex
    | Square

[<Struct>]
type Rgba = {
    red : float
    green : float
    blue : float
    alpha : float
}

[<Struct>]
type Sprite = {
    radians : float
    spriteType : SpriteType
    center : Vector
    size : float
    color : Rgba
}

type View = {
    zoom : float
    sprites : Sprite seq
}

// ----------------------------------------------------------------
// View functions

module Rgba =
    let init r g b a = { 
        red = r
        green = g
        blue = b
        alpha = a 
        }

    let rgb r g b = init r g b 1.0

    let multiplyAlpha c color =
        { color with alpha = color.alpha * c }

module View =
    let teamToColor = function
        | Red -> Rgba.init 1.0 0.0 0.2 1.0
        | Orange -> Rgba.init 1.0 0.4 0.0 1.0
        | Yellow -> Rgba.init 0.6 1.0 0.0 1.0
        | Green -> Rgba.init 0.0 1.0 0.1 1.0
        | Cyan -> Rgba.init 0.0 0.8 0.6 1.0
        | Blue -> Rgba.init 0.0 0.4 1.0 1.0
        | Purple -> Rgba.init 0.6 0.0 1.0 1.0

    let getTrailSprites world =
        world.trails 
        |> Seq.map (fun trail -> {
            radians = trail.radians
            spriteType = Hex
            center = trail.position
            size = trail.lifespan * 0.3
            color = teamToColor trail.team |> Rgba.multiplyAlpha (trail.lifespan * 0.3)
        })

    let getVehicleSprites world =
        world.vehicles 
        |> Seq.map (fun vehicle -> {
            radians = Vector.radians vehicle.direction
            spriteType = Triangle
            center = vehicle.position
            size = 0.1
            color = teamToColor vehicle.team
        })

    let init world = { 
        zoom = 1.0
        sprites = 
            Seq.append 
                (getVehicleSprites world)
                (getTrailSprites world)
        }

// ----------------------------------------------------------------
// GUI/MonoGame

module MonoGameView =
    open System
    open System.IO
    open System.Collections.Generic
    open Microsoft.Xna.Framework
    open Microsoft.Xna.Framework.Graphics
    open Microsoft.Xna.Framework.Input

    module Conversion =
        let toVector v =
            Vector2(float32 v.x, float32 v.y)

        let toColor c =
            Color(
                float32 c.red,
                float32 c.green,
                float32 c.blue,
                float32 c.alpha)

    type TextureEntry = {
        origin : Vector2
        texture : Texture2D
    }

    type TextureSet() =
        let lookup = Dictionary<_,_>()
        member c.Add(graphicsDevice, spriteType, path, origin : Vector2) =
            use fs = File.OpenRead(path)
            let tex = Texture2D.FromStream(graphicsDevice, fs)
            lookup.Add(spriteType, { 
                origin = Vector2(float32 tex.Width * origin.X, float32 tex.Height * origin.Y)
                texture = tex })
        member c.Draw(sb : SpriteBatch, sprite) =
            let entry = lookup.[sprite.spriteType]
            sb.Draw(
                texture = entry.texture,
                position = Conversion.toVector sprite.center,
                sourceRectangle = Nullable (Rectangle(0, 0, entry.texture.Width, entry.texture.Height)),
                color = Conversion.toColor sprite.color, 
                rotation = float32 sprite.radians, 
                origin = entry.origin,
                scale = Vector2(1.0f, 1.0f) * float32 sprite.size, 
                effects = SpriteEffects.None,
                layerDepth = 0.0f)

    type TestGame(settings) as c =
        inherit Game()
        do c.Content.RootDirectory <- "."
        let graphics = new GraphicsDeviceManager(c)
        let fps = MonoGameFsi.Performance.FpsMonitor()
        let viewportWidth = 800
        let viewportHeight = 600
        let mutable sb = null
        let mutable tileSet = TextureSet()
        let mutable model = WritableWorld(settings)
        override c.Initialize() =
            tileSet.Add(c.GraphicsDevice, Triangle, "../triangle.png", Vector2(0.3333f, 0.5f))
            tileSet.Add(c.GraphicsDevice, Hex, "../hex.png", Vector2(0.3333f, 0.5f))
            tileSet.Add(c.GraphicsDevice, Square, "../square.png", Vector2(0.5f, 0.5f))
            sb <- new SpriteBatch(c.GraphicsDevice)            
            graphics.SynchronizeWithVerticalRetrace <- false
            graphics.PreferredBackBufferWidth <- viewportWidth
            graphics.PreferredBackBufferHeight <- viewportHeight
            graphics.ApplyChanges()
            c.IsMouseVisible <- true
            c.IsFixedTimeStep <- false
        override c.Update gameTime = 
            fps.Update()
            model.Update gameTime.ElapsedGameTime.TotalSeconds
        override c.Draw gameTime = 
            let view = View.init model.World
            let xf = 
                Matrix.CreateScale(float32 view.zoom) *
                Matrix.CreateTranslation(Vector3(float32 viewportWidth * 0.5f, float32 viewportHeight * 0.5f, 0.0f))
            c.GraphicsDevice.Clear(Color(0.0f, 0.1f, 0.2f, 1.0f))
            sb.Begin(
                samplerState = SamplerState.LinearClamp, 
                blendState = BlendState.Additive,
                transformMatrix = Nullable xf)
            for sprite in view.sprites do
                tileSet.Draw(sb, sprite)
            sb.End()

// ----------------------------------------------------------------
// Execution

let run settings =
    use g = new MonoGameView.TestGame(settings)
    g.Run()

let settings = {
    seed = 1
    vehicleCount = 500
    spawnRange = 300.0
    maxVehicleSpeed = 50.0
    trailLifespan = 0.6
    steering = {
        forwardWeight = 20.0
        cohesionWeight = 3.0
        tetherWeight = 1.0
        separationWeight = 3.0
        alignmentWeight = 1.0
        maxAlignmentDistance = 100.0
        maxSeparationDistance = 70.0
        maxCohesionDistance = 400.0
        maxTetherDistance = 300.0
    }
}

run settings

let test() =
    let m = WritableWorld(settings)
    for i = 1 to 100 do
        m.Update 0.016
(*
#time
test()
#time
*)

open System

let generate seed count =
    let rand = Random(seed)
    Array.init count (fun _ -> 
        rand.Next())

generate 1 10


let getForce center vehicles =
    vehicles 
    |> Seq.map (fun vehicle -> Vector.subtract center vehicle.position)
    |> Seq.reduce Vector.add
    |> Vector.normalizeOrZero

let getForce2 center vehicles =
    let mutable sum = Vector.zero
    for vehicle in vehicles do
        let delta = Vector.subtract center vehicle.position
        sum <- Vector.add sum delta
    Vector.normalizeOrZero sum


type World2 = {
    vehicles : Vehicle list
}

type MutableWorld() =
    let vehicles = List<Vehicle>()
    member c.World = {
        vehicles = vehicles :> seq<_>
    }
