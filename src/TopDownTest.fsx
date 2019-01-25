// ----------------------------------------------------------------
// Script dependencies
// Although not needed for initial type definitions and logic, we 
// declare this first so we can easily re-execute parts below in
// F# interactive without including this. 

#load "MonoGameFsi.fsx"

// ----------------------------------------------------------------
// Misc math and numerical types
[<AutoOpen>]
module Numerics =
    [<Struct>]
    type Vector = {
        x : int
        y : int
    }

    module Vector =
        let init x y = { x = x; y = y }
        let zero = init 0 0
        let left = init -1 0
        let right = init 1 0
        let up = init 0 -1
        let down = init 0 1

    /// Note by declaring within same module, these are effectively part
    /// of the type. We declare them separately in order to use Vector
    /// module functions.
    type Vector with
        static member inline (~-) v = Vector.init -v.x -v.y
        static member inline (+) (a, b) = Vector.init (a.x + b.x) (a.y + b.y)
        static member inline (-) (a, b) = Vector.init (a.x - b.x) (a.y - b.y)
        static member inline (*) (a, c) = Vector.init (a.x * c) (a.y * c)
        static member inline (*) (a, b) = Vector.init (a.x * b.x) (a.y * b.y)
        static member inline (/) (a, c) = Vector.init (a.x / c) (a.y / c)
        static member inline (/) (a, b) = Vector.init (a.x / b.x) (a.y / b.y)

// ----------------------------------------------------------------
// Domain types
// We can start by modeling the domain using types. The more specific
// the types, the more we'll be constrained when writing logic later, 
// which helps avoid bugs.
// Note that all these types are value types, which requires more code
// in the form of [<Struct>] attributes and defining values for enum
// types. Using value types helps eliminate allocations and subsequent
// garbage collection, but we sacrifice safety by not making full use
// of the algebraic type system. For this game, we expect some real-time
// requirements and potentially large changing state, so defining value
// types here is a precaution.
// Whether this is premature optimization or necessary to avoid a rewrite
// later depends on the scope of the game.

[<AutoOpen>]
module Types =
    type Medium =
    | Chasm
    | Floor
    | Wall

    type EntityId = 
    | EntityId of int

    type EntityType =
    | Player
    | Minion
    | Archer

    type ActionType =
    | Wait
    | Move
    | Push
    | Grab
    | Throw
    | Strike

    type EntityState = {
        entityType : EntityType
        location : Vector
        heldEntityId : EntityId option
        hits : int
        maxHits : int
    }

    type Entity = {
        state : EntityState
        entityId : EntityId
    }

    type CellState = {
        medium : Medium
        occupant : Entity option
    }

    type Locale = {
        left : CellState
        right : CellState
        up : CellState
        down : CellState
    }

    type Direction =
    | Left
    | Right
    | Up
    | Down

    type EntityAction =
    | Wait
    | Move of Direction
    | Grab of Direction

    type Command =
    | PlayerAction of EntityAction
    | Reset

    type ActionContext = {
        command : Command
        locale : Locale
        entityState : EntityState       
    }

    /// Unlike the other records defined above, WorldState is not
    /// pure data due to the function members. The functions make it
    /// similar to an interface, so we can use it to wrap another 
    /// representation of world without making a separate copy.
    type WorldState = {
        getCell : Vector -> CellState
        entityCount : int
        getEntityId : int -> EntityId
        getEntity : EntityId -> EntityState
    }

// ----------------------------------------------------------------
// Domain logic
// These are all pure functions organized by convention of module names
// matching a domain type they are most associated with.
// Again, we have some concern about garbage creation, so we choose
// imperative implementations (look for mutable keyword) in some cases.

[<AutoOpen>]
module Functions =
    module Medium =
        let isSolid = function
            | Chasm
            | Wall -> true
            | Floor -> false

        let isPassable = isSolid >> not

        let charMappings = 
            [
                Chasm, ' '
                Floor, '.'
                Wall, '#'
            ]

        let toChar =
            let map = charMappings |> Map.ofSeq
            fun key -> Map.find key map

        let fromChar =
            let map =
                charMappings
                |> Seq.map (fun (key, value) -> value, key)
                |> Map.ofSeq
            fun key -> Map.find key map

    module EntityType =
        let charMappings = 
            [
                Player, '@'
                Minion, 'm'
                Archer, 'a'
            ]

        let toChar =
            let map = charMappings |> Map.ofSeq
            fun key -> Map.find key map

        let tryFromChar =
            let map =
                charMappings
                |> Seq.map (fun (key, value) -> value, key)
                |> Map.ofSeq
            fun key -> Map.tryFind key map

    module CellState =
        let isVacant cell =
            Medium.isPassable cell.medium &&
            cell.occupant.IsNone

        let toChar cell =
            match cell.occupant with
            | Some entity -> EntityType.toChar entity.state.entityType
            | None -> Medium.toChar cell.medium

    module Direction =
        let getDelta = function
            | Left -> Vector.left
            | Right -> Vector.right
            | Up -> Vector.up
            | Down -> Vector.down

    module EntityState =
        let init entityType location = {
            entityType = entityType 
            location = location
            heldEntityId = None
            hits = 1
            maxHits = 1
            }

        let move entity delta =
            { entity with EntityState.location = entity.location + delta }

    module WorldState =
        let getLocale center world =
            {
                left = world.getCell (center + Vector.left)
                right = world.getCell (center + Vector.right)
                up = world.getCell (center + Vector.up)
                down = world.getCell (center + Vector.down)
            }

        let getActionContext entityId command world =
            let entity = world.getEntity entityId
            {
                command = command
                locale = getLocale entity.location world
                entityState = entity
            }

        let getEntityByIndex index world =
            let entityId = world.getEntityId index
            world.getEntity entityId

        let tryGetEntityByType entityType world =
            let mutable result = None
            for i = 0 to world.entityCount - 1 do
                let e = getEntityByIndex i world
                if e.entityType = entityType then
                    result <- Some {
                        entityId = world.getEntityId i
                        state = e
                    }
            result

        let getPlayerOrOrigin world =
            tryGetEntityByType EntityType.Player world
            |> Option.map (fun entity -> entity.state.location)
            |> Option.defaultValue Vector.zero

// ----------------------------------------------------------------
// Mutable data structures
// So far we've avoided state mutation code except as an implementation 
// detail of some (externally pure) functions above. But now we have more
// complex state to store and manipulate, and to do that we turn to an
// OOP approach to encapsulate that state within classes.
// The reason we don't simply use immutable data structures is because
// we're expecting a large, changing world state and we want to avoid
// excessive allocations.

[<AutoOpen>]
module Containers =
    open System
    open System.Collections.Generic

    /// Sparse terrain grid. For a more compact representation and to
    /// allow for batch operations, we could instead store chunks.
    type Terrain() =
        let cells = Dictionary<Vector, Medium>()
        member c.Get p =
            match cells.TryGetValue p with
            | true, x -> x
            | false, _ -> Chasm
        member c.Set(p, cellType) =
            if cellType = Chasm then cells.Remove p |> ignore
            else cells.[p] <- cellType
        member c.Clear() =
            cells.Clear()

    /// Note add/remove operations are deferred until commit
    type Entities() =
        let addedStates = List<EntityState>()
        let removedIds = HashSet<EntityId>()
        let idList = List<EntityId>()
        let entities = Dictionary<EntityId, EntityState>()
        let locationToId = Dictionary<Vector, EntityId>()
        let mutable nextEid = 1
        member c.Count = idList.Count
        member c.GetEntityId index =
            idList.[index]
        member c.TryGetEntityId p =
            match locationToId.TryGetValue p with
            | true, id -> Some id
            | false, _ -> None
        member c.GetEntity entityId =
            entities.[entityId]
        /// Deferred
        member c.Add state =
            addedStates.Add state
        /// Deferred
        member c.Remove entityId =
            removedIds.Add entityId
        /// Updates in place
        member c.Update(entityId, state) =
            let prior = entities.[entityId]
            entities.[entityId] <- state
            locationToId.Remove prior.location |> ignore
            locationToId.Add(state.location, entityId)
        /// Adds and removes pending entities
        member c.Commit() =
            // apply removals
            let mutable i = 0
            while i < idList.Count do
                let entityId = idList.[i]
                if removedIds.Contains entityId then
                    let state = entities.[entityId]
                    locationToId.Remove state.location |> ignore
                    entities.Remove entityId |> ignore
                    idList.[i] <- idList.[idList.Count - 1]
                    idList.RemoveAt(idList.Count - 1)
                else i <- i + 1
            removedIds.Clear()
            // apply additions
            // TODO: either reserve locations on add or handle
            // collisions here
            for state in addedStates do
                let entityId = EntityId nextEid
                nextEid <- nextEid + 1
                entities.Add(entityId, state)
                locationToId.Add(state.location, entityId)
                idList.Add entityId
            addedStates.Clear()
        /// Immediately clears all, including pending
        member c.Clear() =
            removedIds.Clear()
            addedStates.Clear()
            idList.Clear()
            entities.Clear()
            locationToId.Clear()
            nextEid <- 1

    /// Holds all world state, accessible via WorldState, which serves as
    /// a read-only view.
    type World() =
        let entities = Entities()
        let terrain = Terrain()
        let state = {
            getCell = fun p -> {
                medium = terrain.Get p
                occupant =
                    entities.TryGetEntityId p
                    |> Option.map (fun entityId -> {
                        entityId = entityId
                        state = entities.GetEntity entityId
                        })
                }
            entityCount = 0
            getEntityId = fun i -> entities.GetEntityId i
            getEntity = fun entityId -> entities.GetEntity entityId
        }
        member c.State = 
            { state with entityCount = entities.Count }
        member c.Add state =
            entities.Add state
        member c.Remove entityId =
            entities.Remove entityId
        member c.Update(entityId, state) =
            entities.Update(entityId, state)
        member c.Set(location, medium) =
            terrain.Set(location, medium)
        member c.Commit() =
            entities.Commit()
        member c.Clear() =
            terrain.Clear()
            entities.Clear()

    // Here we separate out any methods which can be defined in terms of existing
    // public methods (i.e. no reason they need to define them as part of the
    // class itself and give access to private members).
    [<AutoOpen>]
    module Extensions =
        open System

        type World with
            member c.Load str =
                c.Load(Vector.zero, str)

            /// Loads a multiline string of cells
            /// Note empty lines are not supported
            member c.Load(offset, str : string) =
                let lines = str.Split([| '\n'; '\r' |], System.StringSplitOptions.RemoveEmptyEntries)
                for y = 0 to lines.Length - 1 do
                    let line = lines.[y]
                    for x = 0 to line.Length - 1 do
                        let p = Vector.init x y + offset
                        let ch = line.[x]
                        match EntityType.tryFromChar ch with
                        | Some entityType ->
                            let entity = EntityState.init entityType p
                            c.Add entity |> ignore
                        | None ->
                            let medium = Medium.fromChar ch
                            c.Set(p, medium)
                c.Commit()

            /// Apply an action by an entity, including all side-effects on world
            member c.Apply(entityId, action) =
                let entity = c.State.getEntity entityId
                match action with
                | Wait ->
                    c.Step()
                | Move dir ->
                    let delta = Direction.getDelta dir
                    let target = entity.location + delta
                    let targetCell = c.State.getCell target
                    if CellState.isVacant targetCell then
                        c.Update(entityId, { entity with location = target })
                        c.Step()
                | Grab dir ->
                    c.Step()

            member c.Step() =
                let state = c.State
                for i = 0 to state.entityCount - 1 do
                    let entityId = state.getEntityId i
                    let entity = state.getEntity entityId
                    match entity.entityType with
                    | EntityType.Player -> ()
                    | _ -> ()                    
                c.Commit()

            member c.Apply action =
                match WorldState.tryGetEntityByType EntityType.Player c.State with
                | None -> ()
                | Some entity ->
                    c.Apply(entity.entityId, action)
                    c.Commit()

            member c.Apply actions =
                for action in actions do
                    c.Apply action

[<AutoOpen>]
module Infrastructure =
    open System
    open System.Collections.Generic

    /// Provides pub/sub semantics for a specific event type.
    /// Subscriptions are unordered.
    /// Not thread/reentrant-safe.
    type EventPublisher<'a>() =
        let subscribers = List<Action<'a>>()
        let namedSubscribers = Dictionary<string, Action<'a>>()
        /// Replaces existing subscription if a name is provided
        member c.Subscribe(name, action) =
            if String.IsNullOrEmpty name 
                then subscribers.Add action
                else namedSubscribers.[name] <- action
        member c.Unsubscribe name =
            namedSubscribers.Remove name
        member c.Publish<'a> (event : 'a) =
            for sub in subscribers do
                sub.Invoke event
            for sub in namedSubscribers.Values do
                sub.Invoke event

    /// Provides pub/sub semantics for any event type.
    /// Subscriptions are unordered.
    /// Not thread/reentrant-safe
    type EventPublisher() =
        let publishers = Dictionary<Type, obj>()
        member c.Get<'a>() =
            match publishers.TryGetValue(typeof<'a>) with
            | true, x -> x :?> EventPublisher<'a>
            | false, _ ->
                let pub = EventPublisher<'a>()
                publishers.Add(typeof<'a>, pub)
                pub

    [<AutoOpen>]
    module Extensions =
        type EventPublisher with
            member c.On<'a> (action : 'a -> unit) =
                c.Get<'a>().Subscribe("", Action<'a>(action))

            member c.Publish<'a> (event : 'a) =
                c.Get<'a>().Publish event

// ----------------------------------------------------------------
// Map data

module Data =
    let segment1 = @"
     ........  
    ... ...m...
    .......... 
     ....@.....
    ........ ..
    ... .......
    ....m.m.  .
     ....... ..
    "
// ----------------------------------------------------------------
// Tests/scratch code for testing non-presentation logic.

module Testing =        
    let formatWorldExtent (extent : Vector) world =
        let sb = System.Text.StringBuilder()
        let offset = WorldState.getPlayerOrOrigin world - extent / 2
        for y = 0 to extent.y do
            for x = 0 to extent.x do
                let p = Vector.init x y + offset
                let cell = world.getCell p
                sb.Append(CellState.toChar cell) |> ignore
            if y < extent.y then
                sb.AppendLine() |> ignore
        sb.ToString()

    let formatWorld world =
        formatWorldExtent (Vector.init 30 12) world
   
    let testLoading() =
        let world = World()
        world.Load Data.segment1        
        world.State |> formatWorld |> printfn "\n%s"
        [
            Move Left, 5
        ] 
        |> Seq.collect (fun (action, count) -> 
            Seq.replicate count action)
        |> Seq.iter (fun action ->
            world.Apply action
            world.State |> formatWorld |> printfn "\n%s")
        

// ----------------------------------------------------------------
// GUI/MonoGame

[<AutoOpen>]
module Presentation =
    open System
    open System.IO
    open System.Collections.Generic
    open Microsoft.Xna.Framework
    open Microsoft.Xna.Framework.Graphics
    open Microsoft.Xna.Framework.Input

    [<Struct>]
    type Tile = {
        tileChar : char
        color : Color
        position : Vector
    }
    
    [<Struct>]
    type KeyBinding = {
        modifier : Keys option
        key : Keys
        command : Command
    }

    [<Struct>]
    type Initialize = {
        game : Game
        graphics : GraphicsDeviceManager
    }

    [<Struct>]
    type Update = {
        updateTime : GameTime
    }

    [<Struct>]
    type Draw = {
        drawTime : GameTime
    }

    let keyBindings = 
        [|
        None, Keys.Space, PlayerAction Wait 
        None, Keys.Left, PlayerAction (Move Left)
        None, Keys.Right, PlayerAction (Move Right)
        None, Keys.Up, PlayerAction (Move Up)
        None, Keys.Down, PlayerAction (Move Down)
        Some Keys.LeftShift, Keys.Left, PlayerAction (Grab Left)
        Some Keys.LeftShift, Keys.Right, PlayerAction (Grab Right)
        Some Keys.LeftShift, Keys.Up, PlayerAction (Grab Up)
        Some Keys.LeftShift, Keys.Down, PlayerAction (Grab Down)
        None, Keys.R, Reset
        |]
        |> Array.map (fun (modifier, key, command) -> {
            key = key
            command = command 
            modifier = modifier
            })

    [<AutoOpen>]
    module Settings =
        let viewExtent = Vector.init 33 21
        let viewMargin = Vector.init 1 1
        let windowExtent = viewExtent + viewMargin * 2
        let tileScaling = 3
        let stepDuration = TimeSpan.FromMilliseconds 250.0

    /// 16x16 tileset
    type TileSet(graphicsDevice, path) =
        let tex = 
            use fs = File.OpenRead(path)
            Texture2D.FromStream(graphicsDevice, fs)
        let tileSize = Vector.init (tex.Width / 16) (tex.Height / 16)
        member c.TileSize = tileSize
        member c.Texture = tex
        member c.GetSourceRectangle (ch : char) =
            let index = int ch
            let tx = index % 16
            let ty = index / 16
            Rectangle(tx * tileSize.x, ty * tileSize.y, tileSize.x, tileSize.y)

    let drawTiles (tileSet : TileSet) (sb : SpriteBatch) world focus = 
        let destSize = tileSet.TileSize * tileScaling
        let offset = focus - viewExtent / 2
        let color = Color(0.7f, 0.7f, 0.7f, 1.0f)            
        for y = 0 to viewExtent.y do
            for x = 0 to viewExtent.x do
                let p = Vector.init x y
                let cell = world.getCell (p + offset)
                let tileChar = CellState.toChar cell
                if tileChar <> ' ' then
                    let dest = Rectangle(p.x * destSize.x, p.y * destSize.y, destSize.x, destSize.y)
                    let src = tileSet.GetSourceRectangle tileChar
                    sb.Draw(tileSet.Texture, dest, Nullable src, color)
    
    let tryGetCommand (keyboardState : KeyboardState) =
        let mutable command = None
        let mutable i = 0
        while i < keyBindings.Length && command = None do
            let binding = keyBindings.[i]
            if keyboardState.IsKeyDown binding.key then
                let isModifierDown =
                    match binding.modifier with
                    | Some key -> keyboardState.IsKeyDown key
                    | None -> true
                if isModifierDown then
                    command <- Some binding.command
            i <- i + 1
        command

    let registerWorld (events : EventPublisher) =
        events.On<Initialize> <| fun e ->
            let game = e.game
            // model
            let world = World()
            world.Load Data.segment1
            // drawing
            let tileSet = TileSet(game.GraphicsDevice, "../drake_10x10.png")
            let sb = new SpriteBatch(game.GraphicsDevice)            
            // init window
            let windowSize = windowExtent * tileSet.TileSize * tileScaling
            e.graphics.PreferredBackBufferWidth <- windowSize.x
            e.graphics.PreferredBackBufferHeight <- windowSize.y
            e.graphics.ApplyChanges()
            game.IsMouseVisible <- true
            //game.Window.IsBorderless <- true
            // event handlers
            let mutable priorCommand = None
            events.On<Update> <| fun e ->
                let keyboardState = Input.Keyboard.GetState()
                let command = tryGetCommand keyboardState
                if command <> priorCommand then
                    priorCommand <- command
                    match command with
                    | None -> ()
                    | Some command ->
                        match command with
                        | Reset ->
                            world.Clear()
                            world.Load Data.segment1
                        | PlayerAction action ->
                            printfn "%A" command
                            world.Apply action
                            world.Step()
            events.On<Draw> <| fun e ->
                game.GraphicsDevice.Clear(Color(0.0f, 0.1f, 0.2f, 1.0f))
                sb.Begin(samplerState = SamplerState.PointClamp)
                let focus = WorldState.getPlayerOrOrigin world.State
                drawTiles tileSet sb world.State focus
                sb.End()

    /// Generic game implementation where behavior is deferred to event 
    /// handlers.
    type EventBasedGame(events : EventPublisher) as c =
        inherit Game()
        do c.Content.RootDirectory <- "."
        let graphics = new GraphicsDeviceManager(c)
        override c.Initialize() =
            // Normally we'd need to assign resources and other class
            // member state upon initialization here, but that requires 
            // extra mutable member declarations.
            // Instead we can avoid the mutation by simply publishing an
            // event and assuming subscriber will subscribe to other
            // events upon initialization.
            events.Publish<Initialize> { 
                game = c 
                graphics = graphics
                }
        override c.Update(gt) = 
            events.Publish<Update> { updateTime = gt }
        override c.Draw(gt) = 
            events.Publish<Draw> { drawTime = gt }

    let run() =
        let events = EventPublisher()
        registerWorld events
        let game = new EventBasedGame(events)
        game.Run()
        game

// ----------------------------------------------------------------
// Execution

let g = run()
g.Dispose()
