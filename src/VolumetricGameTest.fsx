[<AutoOpen>]
module Types =
    type Medium =
     | None = 0uy
     | Solid = 1uy

    [<Struct>]
    type EntityId = {
        eid : int
    }    

    [<Struct>]
    type Vec2 = {
        x : int
        y : int
    }    

    [<Struct>]
    type Vec3 = {
        x : int
        y : int
        z : int
    }

    type Facing =
    | None = 0uy
    | North = 1uy
    | South = 2uy
    | East = 3uy
    | West = 4uy
    | Up = 5uy
    | Down = 6uy

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
    | Rogue = 100uy
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
    | Wait = 1uy
    | Move = 2uy
    | Crouch = 3uy
    | Stand = 4uy
    | Block = 5uy
    | Invoke = 6uy
    | Climb = 7uy
    | Lower = 8uy
    | Strike = 9uy
    | Push = 10uy
    | Grab = 11uy
    | Throw = 12uy
    | Drop = 13uy
    | Use = 14uy
    | Jump = 15uy
    | Lunge = 16uy
    | Stamp = 17uy
    | Swim = 18uy
    | Inspect = 19uy
    | Swap = 20uy
    | Charge = 21uy
    | Break = 22uy
    | Blast = 23uy
    | Talk = 24uy

    [<Struct>]
    type SoundEvent = {
        epicenter : Vec3
        loudness : int
    }

    [<Struct>]
    type FlashEvent = {
        epicenter : Vec3
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
        location : Vec3
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
        location : Vec3
        entityId : EntityId
    }

    [<Struct>]
    type CellState = {
        medium : Medium
        occupantType : EntityType
        occupantId : EntityId
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
    //     location : Vec3
    //     newMedium : Medium
    //     }

    [<Struct>]
    type TargetedAction = {
        action : Action
        energyCost : byte
        relativeTarget : Vec3
    }

    [<Struct>]
    type WorldState = {
        getCell : Vec3 -> CellState
        entityCount : int
        getEntityId : int -> EntityId
        getEntity : EntityId -> EntityState
    }

    type ProjectionFocus =
        | Location = 0
        | Rogue = 1

    type ProjectionMode =
        | None = 0
        | Entities = 1
        | Terrain = 2
        | Composite = 3
        | Depth = 4

    [<Struct>]
    type Orientation = {
        dx : Facing
        dy : Facing
        dz : Facing
    }

    [<Struct>]
    type WorldProjection = {
        mode : ProjectionMode
        focus : ProjectionFocus
        offset : Vec3
        orientation : Orientation
        semiExtent : Vec3
    }

    [<Struct>]
    type SlabCells = {
        cell0 : CellState
        cell1 : CellState
        cell2 : CellState
        cell3 : CellState
        cell4 : CellState
    }    

    [<Struct>]
    type Rgba = {
        r : byte
        g : byte
        b : byte
        a : byte
    }    

    [<Struct>]
    type Tile = {
        fg : Rgba
        bg : Rgba
        tileChar : char
    }    

module EntityId =
    let undefined = { eid = 0 }
    let isDefined entityId = entityId <> undefined
    let isUndefined entityId = entityId = undefined

module Vec3 =
    let init x y z = { x = x; y = y; z = z }
    let zero = init 0 0 0
    let east = init 1 0 0
    let west = init -1 0 0
    let north = init 0 -1 0
    let south = init 0 1 0
    let up = init 0 0 1
    let down = init 0 0 -1

    let multiply a c = init (a.x * c) (a.y * c) (a.z * c)
    let add a b = init (a.x + b.x) (a.y + b.y) (a.z + b.z)

module Medium =
    let isSolid = function
        | Medium.Solid -> true
        | _ -> false

    let isAir = function
        | Medium.None -> true
        | _ -> false

module EntityType =
    let isUndefined = function
        | EntityType.None -> true
        | _ -> false

    let isDefined = isUndefined >> not

module Cell =
    let isVacant cell =
        Medium.isAir cell.medium &&
        EntityType.isUndefined cell.occupantType

module Facing =
    let getDirection = function
        | Facing.East -> Vec3.east
        | Facing.West -> Vec3.west
        | Facing.North -> Vec3.north
        | Facing.South -> Vec3.south
        | Facing.Up -> Vec3.up
        | Facing.Down -> Vec3.down
        | _ -> Vec3.zero

module Command =
    let getFacing = function
        | Command.Up -> Facing.North
        | Command.Down -> Facing.South
        | Command.Left -> Facing.West
        | Command.Right -> Facing.East
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
    let climbUp = facing ActionType.Climb Facing.Up

module TargetedAction =
    let init action cost target = {
        action = action
        energyCost = cost
        relativeTarget = target
        } 

    let undefined = init Action.none 0uy Vec3.zero
    let wait = init (Action.init ActionType.Wait) 0uy Vec3.zero

    let isUndefined action = action = undefined
    let isDefined = isUndefined >> not

    let tryClimb context =
        if Command.isMove context.command &&
            Medium.isSolid context.locale.direction.medium &&
            Medium.isSolid context.locale.directionAbove.medium &&
            Cell.isVacant context.locale.above 
            then
                let dir = Command.getDirection context.command
                init Action.climbUp 5uy (Vec3.add dir Vec3.up)
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
        tryMove
        |]

    let getAction context =
        let mutable action = undefined
        let mutable i = 0
        while i < evaluators.Length && isUndefined action do
            action <- evaluators.[i] context
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

module Entity =
    let undefined = {
        location = Vec3.zero
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

module WorldState =
    let getLocale center facing actionFacing world =
        let facingDir = Facing.getDirection facing
        let actionDir = Facing.getDirection actionFacing
        let dirPos = Vec3.add center actionDir
        {
            direction = world.getCell dirPos
            directionAbove = world.getCell (Vec3.add dirPos Vec3.up)
            directionBelow = world.getCell (Vec3.add dirPos Vec3.down)
            facing = world.getCell (Vec3.add center facingDir)
            above = world.getCell (Vec3.add center Vec3.up)
            below = world.getCell (Vec3.add center Vec3.down)       
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
                result <- e
        result
    
    let getRogueOrOrigin world =
        let rogue = getEntityByType EntityType.Rogue world
        if Entity.isUndefined rogue then Vec3.zero else rogue.location

    let getSlabCells center delta world = {
        cell0 = world.getCell (Vec3.add center (Vec3.multiply delta -2))
        cell1 = world.getCell (Vec3.add center (Vec3.multiply delta -1))
        cell2 = world.getCell center
        cell3 = world.getCell (Vec3.add center (Vec3.multiply delta 1))
        cell4 = world.getCell (Vec3.add center (Vec3.multiply delta 2))
        }
        
    // let project projection buffer :  world =
    //     let center = getRogueOrOrigin world
    //     for y = -semiExtent.y to semiExtent.y do
    //         for x = -semiExtent.x to semiExtent.x do
    //             for z = -semiExtent.z to semiExtent.z do
    //                 ()

    //     sb.ToString()

[<AutoOpen>]
module Shared =
    open System.Collections.Generic

    type Terrain() =
        let cells = Dictionary<Vec3, Medium>()
        member c.Get p =
            match cells.TryGetValue p with
            | true, x -> x
            | false, _ -> Medium.None
        member c.Set(p, cellType) =
            if cellType = Medium.None then cells.Remove p |> ignore
            else cells.[p] <- cellType

    type Entities() =
        let idList = List<EntityId>()
        let entities = Dictionary<EntityId, EntityState>()
        let ids = Dictionary<Vec3, EntityId>()
        let mutable nextEid = 1
        member c.Count = idList.Count
        member c.GetEntityId index =
            idList.[index]
        member c.GetEntityId p =
            match ids.TryGetValue p with
            | true, id -> id
            | false, _ -> EntityId.undefined
        member c.GetEntity entityId =
            entities.[entityId]
        member c.Add state =
            let entityId = { eid = nextEid }
            nextEid <- nextEid + 1
            entities.Add(entityId, state)
            ids.Add(state.location, entityId)
            idList.Add entityId
            entityId
        member c.Remove entityId =
            entities.Remove entityId |> ignore
        member c.Update(entityId, state) =
            entities.[entityId] <- state

    type World() =
        let entities = Entities()
        let terrain = Terrain()
        let state = {
            getCell = fun p ->
                let entityId = entities.GetEntityId p
                {
                    medium = terrain.Get p
                    occupantId = entityId
                    occupantType = 
                        if EntityId.isUndefined entityId then EntityType.None
                        else (entities.GetEntity entityId).entityType
                }
            entityCount = 0
            getEntityId = fun i -> entities.GetEntityId i
            getEntity = fun entityId -> entities.GetEntity entityId
        }
        member c.State = 
            { state with entityCount = entities.Count }
        member c.Terrain = terrain
        member c.Entities = entities


module Extensions =
    type World with
        member c.Apply(entityId, action) =
            let entity = c.Entities.GetEntity entityId
            let target = Vec3.add entity.location action.relativeTarget
            match action.action.actionType with
            | ActionType.Move ->
                c.Entities.Update(entityId, { entity with location = target })
                ()
            | _ -> ()




#load "MonoGameFsi.fsx"

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input

type TestGame() as x =
    inherit Game()
    do x.Content.RootDirectory <- "."
    let graphics = new GraphicsDeviceManager(x)
    let world = World()
    override x.Initialize() =
        //graphics.PreferredBackBufferWidth <- settings.graphics.screenWidth
        //graphics.PreferredBackBufferHeight <- settings.graphics.screenHeight
        //graphics.ApplyChanges()
        x.IsMouseVisible <- true
    override x.Update(gt) = 
        ()
        // runTime Pass.fixedUpdate gt.ElapsedGameTime
        // runTime Pass.update gt.ElapsedGameTime
    override x.Draw(gt) = 
        x.GraphicsDevice.Clear(Color.Blue)
        // sb.Begin(samplerState = SamplerState.PointClamp)
        // for y = 0 to vh - 1 do
        //     for x = 0 to vw - 1 do
        //         let i = y * vw + x
        //         let tile = cameraToScreenTile tileBuffer.[i]
        //         //drawTile x y (int ' ') tile.bg 0
        //         drawTile x y (int tile.fg) Rgba32.max 1

        // runTime Pass.draw gt.ElapsedGameTime

let g = new TestGame()
g.Run()
g.Dispose()

