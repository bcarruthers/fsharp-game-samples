#load "MonoGameFsi.fsx"

// ----------------------------------------------------------------
// Math types

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
        let s0 = 20.0
        let s1 = 0.0
        let s = 5.0
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

// ----------------------------------------------------------------
// Domain types

type Team =
    | Blue
    | Red
    | Yellow
    | Cyan

type Vehicle = {
    team : Team
    position : Vector
    direction : Vector
    speed : float
    radius : float
}

type Neighbor = {
    vehicle : Vehicle
    directionToNeighbor : Vector
    distance : float
    teamWeight : float
}

type Neighborhood = {
    current : Vehicle
    neighbors : Neighbor seq
}

type World = {
    vehicles : Vehicle list
}

type SteeringForce =
    | Forward
    | Cohesion
    | Separation
    | Alignment
    | Tether

type Model = {
    //steeringForces : SteeringForce list
    world : World
}

type Message =
    | Reset of int
    | Step of float

// ----------------------------------------------------------------
// Domain functions

module Team =
    let teams = [|
        Blue
        Red
        Yellow
        Cyan
    |]

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

module Neighborhood =
    let getForwardForce neighborhood =
        neighborhood.current.direction

    let smoothStepDistance minDistance maxDistance offset =
        let distance = Vector.length offset
        let scale = Scalar.smoothStep minDistance maxDistance distance
        Vector.divideOrZero distance offset
        |> Vector.multiply scale

    let getTetherForce neighborhood =
        let tetherPoint = Vector.init 0.0 0.0
        let toTether = Vector.subtract tetherPoint neighborhood.current.position
        smoothStepDistance 0.0 300.0 toTether

    let getCohesionForce neighborhood =
        neighborhood.neighbors 
        |> Seq.map (fun neighbor -> 
            let weight = Scalar.smoothStep 50.0 200.0 neighbor.distance
            Vector.multiply (neighbor.teamWeight * weight) neighbor.directionToNeighbor) 
        |> Seq.reduce Vector.add
        |> Vector.normalizeOrZero

    let getSeparationForce neighborhood =
        neighborhood.neighbors 
        |> Seq.map (fun neighbor -> 
            let weight = if neighbor.distance < 50.0 then 1.0 else 0.0//Scalar.smoothStep 10.0 0.0 neighbor.distance
            Vector.multiply -weight neighbor.directionToNeighbor)
        |> Seq.reduce Vector.add
        //|> Vector.normalizeOrZero

    let getAlignmentForce neighborhood =
        neighborhood.neighbors 
        |> Seq.map (fun neighbor -> 
            let weight = Scalar.smoothStep 100.0 0.0 neighbor.distance
            Vector.multiply -(neighbor.teamWeight * weight) neighbor.vehicle.direction)
        |> Seq.reduce Vector.add
        |> Vector.normalizeOrZero
    
    let forceCalculators = [
        20.0, getForwardForce
        //1.0, getTetherForce
        1.0, getCohesionForce
        1.0, getSeparationForce
        1.0, getAlignmentForce
    ]

    let getSteeringForce neighborhood =
        forceCalculators
        |> List.map (fun (weight, calc) -> 
            calc neighborhood
            |> Vector.multiply weight)
        |> Seq.reduce Vector.add
        |> Vector.normalizeOrZero
        |> Vector.multiply 30.0

    let applySteeringForce deltaTime neighborhood =
        let desiredVelocity = getSteeringForce neighborhood
        Vehicle.setVelocity desiredVelocity neighborhood.current

module World =
    let empty = { 
        vehicles = List.empty 
        }

    let getNeighborhoods world =
        world.vehicles 
        |> List.mapi (fun i current -> {
            current = current
            neighbors =
                world.vehicles
                |> Seq.mapi (fun ni vehicle -> ni, vehicle)
                |> Seq.filter (fun (ni, vehicle) -> ni <> i)
                |> Seq.map (fun (ni, vehicle) -> 
                    let offset = Vector.subtract vehicle.position current.position
                    let distance = Vector.length offset
                    { vehicle = vehicle
                      teamWeight = if current.team = vehicle.team then 1.0 else 0.0
                      directionToNeighbor = Vector.divideOrZero distance offset
                      distance = distance
                      })
        })

    let applySteeringForces deltaTime world =
        { world with 
            vehicles = 
                getNeighborhoods world 
                |> List.map (Neighborhood.applySteeringForce deltaTime)
        }

    let updatePositions deltaTime world =
        { world with vehicles = world.vehicles |> List.map (Vehicle.updatePosition deltaTime) }

    let addVehicle vehicle world =
        { world with vehicles = vehicle :: world.vehicles }

    let update deltaTime world =
        world
        |> applySteeringForces deltaTime
        |> updatePositions deltaTime

    let create seed vehicleCount = 
        let rand = System.Random(seed)
        let range = 300.0
        let nextCoord() =
            (rand.NextDouble() - 0.5) * range
        { vehicles =
            List.init vehicleCount (fun _ -> {
                team = Team.teams.[rand.Next Team.teams.Length]
                position = Vector.init (nextCoord()) (nextCoord())
                direction = Vector.init 0.0 1.0
                speed = 0.0
                radius = 1.0 })
        }

module Model =
    let empty = {
        world = World.empty
    }

    let update msg model =
        match msg with
        | Reset seed ->
            { model with 
                world = World.create seed 100
                }
        | Step deltaTime ->
            { model with 
                world = World.update deltaTime model.world
                }

// ----------------------------------------------------------------
// View types

type SpriteType =
    | Triangle
    | Hex
    | Square

type Rgba = {
    red : float
    green : float
    blue : float
    alpha : float
}

type Sprite = {
    radians : float
    spriteType : SpriteType
    center : Vector
    size : float
    color : Rgba
}

type View = {
    zoom : float
    sprites : Sprite list
}

// ----------------------------------------------------------------
// View functions

module Color =
    let rgba r g b a = { 
        red = r
        green = g
        blue = b
        alpha = a 
        }

    let rgb r g b = rgba r g b 1.0

    let multiplyAlpha c color =
        { color with alpha = color.alpha * c }

    let red = rgb 1.0 0.0 0.0

module View =
    let teamToColor = function
        | Blue -> Color.rgba 0.0 0.0 1.0 1.0
        | Red -> Color.rgba 1.0 0.0 0.0 1.0
        | Yellow -> Color.rgba 1.0 1.0 0.0 1.0
        | Cyan -> Color.rgba 0.0 1.0 1.0 1.0

    let init model = { 
        zoom = 1.0
        sprites = 
            model.world.vehicles 
            |> List.map (fun vehicle -> {
                radians = Vector.radians vehicle.direction
                spriteType = Triangle
                center = vehicle.position
                size = 0.1
                color = teamToColor vehicle.team
            })
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
        
        // /// Returns radians of perpendicular dir
        // let toRotation v =
        //     // let x = -v.y
        //     // let y = v.x
        //     float32 (atan2 v.y v.x)

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

    type TestGame() as c =
        inherit Game()
        do c.Content.RootDirectory <- "."
        let graphics = new GraphicsDeviceManager(c)
        let fps = MonoGameFsi.Performance.FpsMonitor()
        let viewportWidth = 800
        let viewportHeight = 600
        let mutable sb = null
        let mutable tileSet = TextureSet()
        let mutable model = Model.empty |> Model.update (Reset 1)
        override c.Initialize() =
            tileSet.Add(c.GraphicsDevice, Triangle, "../triangle.png", Vector2(0.3333f, 0.5f))
            tileSet.Add(c.GraphicsDevice, Hex, "../hex.png", Vector2(0.3333f, 0.5f))
            tileSet.Add(c.GraphicsDevice, Square, "../square.png", Vector2(0.5f, 0.5f))
            sb <- new SpriteBatch(c.GraphicsDevice)            
            graphics.PreferredBackBufferWidth <- viewportWidth
            graphics.PreferredBackBufferHeight <- viewportHeight
            graphics.ApplyChanges()
            c.IsMouseVisible <- true
        override c.Update gameTime = 
            fps.Update()
            let msg = Step gameTime.ElapsedGameTime.TotalSeconds
            model <- Model.update msg model
        override c.Draw gameTime = 
            let view = View.init model
            let xf = 
                Matrix.CreateScale(float32 view.zoom) *
                Matrix.CreateTranslation(Vector3(float32 viewportWidth * 0.5f, float32 viewportHeight * 0.5f, 0.0f))
            c.GraphicsDevice.Clear(Color(0.0f, 0.1f, 0.2f, 1.0f))
            sb.Begin(samplerState = SamplerState.PointClamp, transformMatrix = Nullable xf)
            for sprite in view.sprites do
                tileSet.Draw(sb, sprite)
            sb.End()

// ----------------------------------------------------------------
// Execution

let gcMonitor = MonoGameFsi.Performance.GCState.runMonitor()
let g = new MonoGameView.TestGame()
//let thread = System.Threading.Thread(fun () ->
g.Run()
g.Dispose()
gcMonitor.Dispose()
// )
// thread.Start()


// thread.Join()

// #time

// #time


