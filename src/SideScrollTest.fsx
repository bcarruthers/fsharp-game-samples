// ----------------------------------------------------------------
// Script dependencies
// Although not needed for initial type definitions and logic, we 
// declare this first so we can easily re-execute parts below in
// F# interactive without including this. 

#load "MonoGameFsi.fsx"
#load "Numerics.fs"
#load "SideScrollData.fs"

open Numerics

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
    type MediumType =
    | None = 0uy
    | Dirt = 1uy
    | Sand = 2uy
    | Rock = 3uy
    | Grass = 4uy
    | Vine = 5uy
    | Tree = 6uy
    | Root = 7uy
    | Water = 8uy
    | Snow = 9uy
    | Ice = 10uy
    | Lava = 11uy

    type MediumFlags =
    | None = 0
    | Solid = 1
    | Ladder = 2

    [<Struct>]
    type Medium = {
        mediumFlags : MediumFlags
        mediumChar : char
    }

    [<Struct>]
    type EntityId = {
        eid : int
    }

    type Facing =
    | None = 0uy
    | Left = 1uy
    | Right = 2uy
    | Up = 3uy
    | Down = 4uy

    type Stance =
    | Standing = 0uy
    | Crouching = 1uy
    | Blocking = 2uy
    | Falling = 3uy
    | Swimming = 4uy

    type ContentType =
    | None = 0uy
    | Moss = 1uy
    | Debris = 2uy
    | Grass = 3uy

    type EntityType =
    | None = 0uy
    | Rock = 1uy
    | Altar = 3uy
    | Chest = 4uy
    | Player = 100uy
    | Grunt = 101uy
    | Archer = 102uy
    | Alchemist = 103uy

    type Traits =
    | None = 0
    | Lunging = 0x1
    | Charging = 0x2
    | Climbing = 0x4
    | Floating = 0x8
    | Swimming = 0x10
    | Flying = 0x20
    | Swiping = 0x40
    | Piercing = 0x1000
    | Blasting = 0x2000
    | Bashing = 0x4000

    // [<Struct>]
    // type Inventory = {
    //     primary : ItemType
    //     secondary : ItemType
    //     torso
    // | None = 0uy
    // | Sword = 0x01uy
    // | Spear = 0x02uy
    // | Shield = 0x21uy

    type ActionType =
    | None = 0uy
    | Wait = 0x01uy
    | Crouch = 0x02uy
    | Stand = 0x03uy
    | Block = 0x04uy
    | Move = 0x21uy
    | Climb = 0x22uy
    | Descend = 0x23uy
    | Mantle = 0x24uy
    | Lower = 0x25uy
    | Campus = 0x26uy
    | Jump = 0x31uy
    | Lunge = 0x32uy
    | Stamp = 0x33uy
    | Charge = 0x34uy
    | Swim = 0x35uy
    | Strike = 0x41uy
    | Invoke = 0x42uy
    | Push = 0x43uy
    | Break = 0x44uy
    | Blast = 0x45uy
    | Grab = 0x51uy
    | Throw = 0x52uy
    | Drop = 0x53uy
    | Use = 0x54uy
    | Inspect = 0x61uy
    | Swap = 0x62uy
    //| Talk = 24uy

    [<Struct>]
    type SoundEvent = {
        epicenter : Vector
        loudness : int
    }

    [<Struct>]
    type FlashEvent = {
        epicenter : Vector
        brightness : int
    }

    [<Struct>]
    type Action = {
        actionType : ActionType
        actionFacing : Facing
    }

    [<Struct>]
    type ActionHistory = {
        action0 : Action
        action1 : Action
        action2 : Action
        action3 : Action
        action4 : Action
        action5 : Action
        action6 : Action        
        action7 : Action
    }

    [<Struct>]
    type EntityState = {
        location : Vector
        history : ActionHistory
        heldEntityId : EntityId
        traits : Traits
        facing : Facing
        entityType : EntityType
        stance : Stance
        hits : byte
        maxHits : byte
        energy : byte
        maxEnergy : byte
    }

    [<Struct>]
    type Entity = {
        state : EntityState
        entityId : EntityId
    }

    [<Struct>]
    type CellState = {
        medium : MediumType
        occupantType : EntityType
        occupantId : EntityId
        occupantFacing : Facing
    }

    [<Struct>]
    type Locale = {
        direction : CellState
        directionAbove : CellState
        directionBelow : CellState
        facing : CellState
        above : CellState       
        below : CellState       
    }

    type Command =
    | None = 0
    | Step = 1
    | Left = 2
    | Right = 3
    | Up = 4
    | Down = 5
    | Primary = 6
    | Secondary = 7
    | Special = 8
    | Reset = 9

    [<Struct>]
    type ActionContext = {
        command : Command
        locale : Locale
        entityState : EntityState       
    }

    // type EventType =
    //     | None = 0
    //     | Entity = 1
    //     | Terrain = 2

    // [<Struct>]
    // type Event = {
    //     eventType : EventType
    //     entityId : EntityId
    //     newState : EntityState
    //     location : Vector
    //     newMedium : MediumType
    //     }

    [<Struct>]
    type TargetedAction = {
        action : Action
        energyCost : byte
        relativeTarget : Vector
    }

    [<Struct>]
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
    module EntityId =
        let undefined = { eid = 0 }
        let isDefined entityId = entityId <> undefined
        let isUndefined entityId = entityId = undefined

    module Lookup =
        let toLookupTable defaultValue mappings =
            let arr = Array.create 256 defaultValue
            for key : byte, value in mappings do
                arr.[int key] <- value
            arr

    module Medium =
        let init ch flags = {
            mediumChar = ch
            mediumFlags = flags
            }

        let empty = init ' ' MediumFlags.None

        let mediums = [
            MediumType.Rock, init '#' MediumFlags.Solid
            MediumType.Lava, init '&' MediumFlags.Solid
            MediumType.Dirt, init '%' MediumFlags.Solid
            MediumType.Water, init '~' MediumFlags.Solid
            MediumType.Ice, init '=' MediumFlags.Solid
        ]

        let private typeLookup =
            mediums
            |> Seq.map (fun (key, value) -> byte key, value)
            |> Lookup.toLookupTable empty

        let fromType (medium : MediumType) = 
            typeLookup.[int medium]

    module MediumType =
        let isSolid = function
            | MediumType.Rock -> true
            | _ -> false

        let isPassable = isSolid >> not

        let isUndefined = function
            | MediumType.None -> true
            | _ -> false

        let private typeLookup =
            Medium.mediums
            |> Seq.map (fun (key, value) -> byte value.mediumChar, key)
            |> Lookup.toLookupTable MediumType.None    

        let toChar (medium : MediumType) = 
            (Medium.fromType medium).mediumChar

        let fromChar (ch : char) = 
            typeLookup.[int ch]

    module EntityType =
        let isUndefined = function
            | EntityType.None -> true
            | _ -> false

        let isDefined = isUndefined >> not

        let charMappings = 
            [
                EntityType.Player, '@'
            ]

        let private charLookup = 
            charMappings
            |> Seq.map (fun (key, value) -> byte key, value)
            |> Lookup.toLookupTable ' '

        let private typeLookup =
            charMappings
            |> Seq.map (fun (key, value) -> byte value, key)
            |> Lookup.toLookupTable EntityType.None

        let toChar (entityType : EntityType) = 
            charLookup.[int entityType]

        let fromChar (ch : char) = 
            typeLookup.[int ch]


    module Cell =
        let isVacant cell =
            MediumType.isPassable cell.medium &&
            EntityType.isUndefined cell.occupantType

        let isUndefined cell =
            MediumType.isUndefined cell.medium &&
            EntityType.isUndefined cell.occupantType

        let isDefined = isUndefined >> not

        let toChar cell =
            match cell.occupantFacing with
            | Facing.Left -> '<'
            | Facing.Right -> '>'
            | _ ->
                max (EntityType.toChar cell.occupantType)
                    (MediumType.toChar cell.medium)

    module Facing =
        let getDirection = function
            | Facing.Left -> Vector.left
            | Facing.Right -> Vector.right
            | Facing.Up -> Vector.up
            | Facing.Down -> Vector.down
            | _ -> Vector.zero

        let invert = function
            | Facing.Left -> Facing.Right
            | Facing.Right -> Facing.Left
            | Facing.Up -> Facing.Down
            | Facing.Down -> Facing.Up
            | _ -> Facing.None

    module Command =
        let getFacing = function
            | Command.Up -> Facing.Up
            | Command.Down -> Facing.Down
            | Command.Left -> Facing.Left
            | Command.Right -> Facing.Right
            | _ -> Facing.None

        let isMove = function
            | Command.Left
            | Command.Right
            | Command.Up
            | Command.Down -> true
            | _ -> false

        let getDirection command =
            let facing = getFacing command
            Facing.getDirection facing

        let commands = [|
            Command.Step
            Command.Left
            Command.Right
            Command.Up
            Command.Down
            Command.Primary
            Command.Secondary
            Command.Special
            |]

    module Action =
        let facing actionType dir = {
            actionType = actionType
            actionFacing = dir
            }

        let init actionType = facing actionType Facing.None

        let none = init ActionType.None
        let move facingDir = facing ActionType.Move facingDir
        // let climbUp = facing ActionType.Climb Facing.Up
        // let climbUp = facing ActionType.Climb Facing.Up

    module TargetedAction =
        let init action cost target = {
            action = action
            energyCost = cost
            relativeTarget = target
            } 

        let undefined = init Action.none 0uy Vector.zero
        let wait = init (Action.init ActionType.Wait) 0uy Vector.zero

        let isUndefined action = action = undefined
        let isDefined = isUndefined >> not

        let tryClimb context =
            if Command.isMove context.command &&
                MediumType.isSolid context.locale.direction.medium &&
                MediumType.isSolid context.locale.directionAbove.medium &&
                Cell.isVacant context.locale.above 
                then
                    let facing = Command.getFacing context.command
                    if facing <> Facing.invert context.entityState.facing then
                        let action = Action.facing ActionType.Climb facing
                        init action 5uy Vector.up
                    else undefined
                else undefined

        let tryMantle context =
            if Command.isMove context.command &&
                MediumType.isSolid context.locale.direction.medium &&
                Cell.isVacant context.locale.directionAbove &&
                Cell.isVacant context.locale.above 
                then
                    let facing = Command.getFacing context.command
                    if facing <> Facing.invert context.entityState.facing then
                        let dir = Facing.getDirection facing
                        let action = Action.facing ActionType.Mantle facing
                        init action 5uy (dir + Vector.up)
                    else undefined
                else undefined

        let tryLower context =
            if Command.isMove context.command &&
                MediumType.isSolid context.locale.below.medium &&
                Cell.isVacant context.locale.direction &&
                Cell.isVacant context.locale.directionBelow 
                then
                    let facing = Command.getFacing context.command
                    let dir = Facing.getDirection facing
                    let newFacing = Facing.invert facing
                    let action = Action.facing ActionType.Lower newFacing
                    init action 5uy (dir + Vector.down)
                else undefined

        let tryMove context =
            if Command.isMove context.command &&
                Cell.isVacant context.locale.direction 
                then 
                    let facing = Command.getFacing context.command
                    let dir = Facing.getDirection facing
                    init (Action.move facing) 1uy dir
                else undefined

        let evaluators = 
            [|
            tryClimb
            tryMantle
            tryLower
            tryMove
            |]

        let getAction context =
            let mutable action = undefined
            let mutable i = 0
            while i < evaluators.Length && isUndefined action do
                action <- evaluators.[i] context
                i <- i + 1
            if isUndefined action then wait else action

    module ActionHistory =
        let empty = {
            action0 = Action.none
            action1 = Action.none
            action2 = Action.none
            action3 = Action.none
            action4 = Action.none
            action5 = Action.none
            action6 = Action.none   
            action7 = Action.none
        }

    module EntityState =
        let undefined = {
            location = Vector.zero
            entityType = EntityType.None
            history = ActionHistory.empty
            heldEntityId = EntityId.undefined
            traits = Traits.None
            facing = Facing.None
            stance = Stance.Standing
            hits = 0uy
            maxHits = 0uy
            energy = 0uy
            maxEnergy = 0uy        
        }

        let isUndefined entity =
            EntityType.isUndefined entity.entityType

        let isDefined = isUndefined >> not

        let init entityType location =
            { undefined with 
                entityType = entityType 
                location = location
                }

        let move entity delta =
            { entity with EntityState.location = entity.location + delta }

        let toChar (entity : EntityState) =
            match entity.facing with
            | Facing.Left -> '<'
            | Facing.Right -> '>'
            | _ -> EntityType.toChar entity.entityType

    module Entity =
        let undefined = { 
            entityId = EntityId.undefined
            state = EntityState.undefined
        }

        let isUndefined entity = EntityId.isUndefined entity.entityId
        let isDefined = isUndefined >> not

    module WorldState =
        let getLocale center facing actionFacing world =
            let facingDir = Facing.getDirection facing
            let actionDir = Facing.getDirection actionFacing
            let dirPos = center + actionDir
            {
                direction = world.getCell dirPos
                directionAbove = world.getCell (dirPos + Vector.up)
                directionBelow = world.getCell (dirPos + Vector.down)
                facing = world.getCell (center + facingDir)
                above = world.getCell (center + Vector.up)
                below = world.getCell (center + Vector.down)       
            }

        let getActionContext entityId command world =
            let entity = world.getEntity entityId
            let actionFacing = Command.getFacing command
            {
                command = command
                locale = getLocale entity.location entity.facing actionFacing world
                entityState = entity
            }

        let getEntityByIndex index world =
            let entityId = world.getEntityId index
            world.getEntity entityId

        let getEntityByType entityType world =
            let mutable result = Entity.undefined
            for i = 0 to world.entityCount - 1 do
                let e = getEntityByIndex i world
                if e.entityType = entityType then
                    result <- {
                        entityId = world.getEntityId i
                        state = e
                    }
            result

        let getPlayerAction command world =
            let entity = getEntityByType EntityType.Player world
            if Entity.isUndefined entity then TargetedAction.undefined
            else
                let context = getActionContext entity.entityId command world
                TargetedAction.getAction context

        let getPlayerOrOrigin world =
            let entity = getEntityByType EntityType.Player world
            if Entity.isUndefined entity then Vector.zero else entity.state.location

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
        let cells = Dictionary<Vector, MediumType>()
        member c.Get p =
            match cells.TryGetValue p with
            | true, x -> x
            | false, _ -> MediumType.None
        member c.Set(p, cellType) =
            if cellType = MediumType.None then cells.Remove p |> ignore
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
        member c.GetEntityId p =
            match locationToId.TryGetValue p with
            | true, id -> id
            | false, _ -> EntityId.undefined
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
                let entityId = { eid = nextEid }
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
            getCell = fun p ->
                let entityId = entities.GetEntityId p
                let state = 
                    if EntityId.isUndefined entityId 
                        then EntityState.undefined
                        else entities.GetEntity entityId
                {
                    medium = terrain.Get p
                    occupantId = entityId
                    occupantType = state.entityType
                    occupantFacing = state.facing
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
                        let entityType = EntityType.fromChar ch
                        if EntityType.isDefined entityType then
                            let entity = EntityState.init entityType p
                            c.Add entity |> ignore
                        else
                            let medium = MediumType.fromChar ch
                            c.Set(p, medium)
                c.Commit()

            /// Apply an action by an entity, including all side-effects on world
            member c.Apply(entityId, action) =
                let entity = c.State.getEntity entityId
                let target = entity.location + action.relativeTarget
                match action.action.actionType with
                | ActionType.Move
                | ActionType.Climb
                | ActionType.Mantle
                | ActionType.Lower ->
                    c.Update(entityId, { 
                        entity with 
                            location = target 
                            facing = action.action.actionFacing
                            })
                | _ -> ()

            member c.Step() =
                let state = c.State
                for i = 0 to state.entityCount - 1 do
                    let entityId = state.getEntityId i
                    let entity = state.getEntity entityId
                    match entity.entityType with
                    | EntityType.Player -> ()
                    | _ -> ()                    
                c.Commit()

            member c.Apply command =
                let entity = WorldState.getEntityByType EntityType.Player c.State
                if Entity.isDefined entity then
                    let context = WorldState.getActionContext entity.entityId command c.State
                    let action = TargetedAction.getAction context
                    c.Apply(entity.entityId, action)
                    c.Commit()

            member c.Apply commands =
                for command in commands do
                    c.Apply command


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
                sb.Append(Cell.toChar cell) |> ignore
            if y < extent.y then
                sb.AppendLine() |> ignore
        sb.ToString()

    let formatWorld world =
        formatWorldExtent (Vector.init 30 12) world

    let formatAvailableActions world =
        System.String.Join("\n",
            Command.commands
            |> Seq.map (fun command ->
                let action = WorldState.getPlayerAction command world
                sprintf "%A: %A %A" command action.action.actionType 
                    action.action.actionFacing))
                
    let testLoading() =
        let world = World()
        world.Load Data.segment1        
        world.State |> formatWorld |> printfn "\n%s"
        [
            Command.Left, 10
        ] 
        |> Seq.collect (fun (command, count) -> 
            Seq.replicate count command)
        |> Seq.iter (fun command ->
            world.State |> formatAvailableActions |> printfn "Actions:\n%s"
            world.Apply command
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
        Keys.Space, Command.Step
        Keys.Left, Command.Left
        Keys.Right, Command.Right
        Keys.Up, Command.Up
        Keys.Down, Command.Down
        Keys.Z, Command.Primary
        Keys.X, Command.Secondary
        Keys.C, Command.Special
        Keys.R, Command.Reset
        |]
        |> Array.map (fun (key, command) ->
            { key = key; command = command })

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
        // member c.DrawTile(sb : SpriteBatch, tile) =
        //     let dest = Rectangle(tile.position.x * destSize.x, tile.position.y * destSize.y, destSize.x, destSize.y)
        //     let src = c.GetSourceRectangle tile.tileChar
        //     sb.Draw(tex, dest, Nullable src, tile.color)

    type EntityDrawer() =
        let mutable stepTime = TimeSpan.Zero
        let mutable prior = Dictionary<EntityId, EntityState>()
        let mutable current = Dictionary<EntityId, EntityState>()
        member c.Update(world : WorldState, time) =
            stepTime <- time
            // move current to prior
            let temp = prior
            prior <- current
            current <- temp
            // add all to current set
            current.Clear()
            for i = 0 to world.entityCount - 1 do
                let entityId = world.getEntityId i
                let state = world.getEntity entityId
                current.Add(entityId, state)
                // also add to prior if nonexistent so we cover all by
                // iterating through prior when animating
                if not (prior.ContainsKey entityId) then
                    prior.Add(entityId, EntityState.undefined)
        member c.Draw(tileSet : TileSet, sb : SpriteBatch, focus, t) =
            let destSize = tileSet.TileSize * tileScaling
            let offset = -focus + viewExtent / 2
            for kvp in prior do
                let entityId = kvp.Key
                let priorState = kvp.Value
                let currentState =
                    match current.TryGetValue entityId with
                    | true, state -> state
                    | false, _ -> EntityState.undefined
                let animatedState = currentState
                let p = animatedState.location + offset
                let tileChar = EntityState.toChar animatedState
                let dest = Rectangle(p.x * destSize.x, p.y * destSize.y, destSize.x, destSize.y)
                let src = tileSet.GetSourceRectangle tileChar
                let color = Color.White
                sb.Draw(tileSet.Texture, dest, Nullable src, color)

    type TerrainDrawer() =
        member c.Draw(tileSet : TileSet, sb : SpriteBatch, world, focus) = 
            let destSize = tileSet.TileSize * tileScaling
            let offset = focus - viewExtent / 2
            let color = Color(0.7f, 0.7f, 0.7f, 1.0f)            
            for y = 0 to viewExtent.y do
                for x = 0 to viewExtent.x do
                    let p = Vector.init x y
                    let cell = world.getCell (p + offset)
                    if MediumType.isSolid cell.medium then
                        let tileChar = MediumType.toChar cell.medium
                        let dest = Rectangle(p.x * destSize.x, p.y * destSize.y, destSize.x, destSize.y)
                        let src = tileSet.GetSourceRectangle tileChar
                        sb.Draw(tileSet.Texture, dest, Nullable src, color)

    type BackgroundDrawer() =
        member c.Draw(tileSet : TileSet, sb : SpriteBatch, focus) = 
            let destSize = tileSet.TileSize * tileScaling
            let offset = focus - viewExtent / 2
            let src = tileSet.GetSourceRectangle (char 0xdb)
            for y = 0 to viewExtent.y do
                for x = 0 to viewExtent.x do
                    let p = Vector.init x y
                    let h = Noise.sample2 (float32 x) (float32 y) * 0.5f + 0.5f
                    let c = Hsva.init h 0.5f 0.5f 1.0f |> Hsva.toRgba
                    let dest = Rectangle(p.x * destSize.x, p.y * destSize.y, destSize.x, destSize.y)
                    let color = Color(c.r, c.g, c.b, c.a)
                    sb.Draw(tileSet.Texture, dest, Nullable src, color)
        
    let getCommand (keyboardState : KeyboardState) =
        let mutable command = Command.None
        let mutable i = 0
        while i < keyBindings.Length && command = Command.None do
            let binding = keyBindings.[i]
            if keyboardState.IsKeyDown binding.key then
                command <- binding.command
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
            let bgDrawer = BackgroundDrawer()
            let entityDrawer = EntityDrawer()
            let terrainDrawer = TerrainDrawer()
            let sb = new SpriteBatch(game.GraphicsDevice)            
            // init window
            let windowSize = windowExtent * tileSet.TileSize * tileScaling
            e.graphics.PreferredBackBufferWidth <- windowSize.x
            e.graphics.PreferredBackBufferHeight <- windowSize.y
            e.graphics.ApplyChanges()
            game.IsMouseVisible <- true
            //game.Window.IsBorderless <- true
            // event handlers
            let mutable priorCommand = Command.None
            events.On<Update> <| fun e ->
                let keyboardState = Input.Keyboard.GetState()
                let command = getCommand keyboardState
                if command <> priorCommand then
                    priorCommand <- command
                    match command with
                    | Command.None -> ()
                    | Command.Reset ->
                        world.Clear()
                        world.Load Data.segment1
                        entityDrawer.Update(world.State, e.updateTime.ElapsedGameTime)
                    | command ->
                        //printfn "%A" command
                        world.Apply command
                        world.Step()
                        entityDrawer.Update(world.State, e.updateTime.ElapsedGameTime)
            events.On<Draw> <| fun e ->
                game.GraphicsDevice.Clear(Color(0.0f, 0.1f, 0.2f, 1.0f))
                sb.Begin(samplerState = SamplerState.PointClamp)
                let focus = WorldState.getPlayerOrOrigin world.State
                //bgDrawer.Draw(tileSet, sb, focus)
                terrainDrawer.Draw(tileSet, sb, world.State, focus)
                entityDrawer.Draw(tileSet, sb, focus, e.drawTime.ElapsedGameTime)
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
