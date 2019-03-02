// Domain types

type Location = {
    x : int
    y : int
}

type Direction =
    | East
    | West
    | North
    | South
    
type Terrain =
    | Floor
    | Wall

type EntityType =
    | Player
    | Minion

type Entity = {
    entityType : EntityType
    hits : int
}

type Tile = {
    terrain : Terrain
    entity : Entity option
}

type World = {
    turn : int
    randomState : uint64
    tiles : Map<Location, Tile>
}

type Action =
    | Move of Direction

type MovedEvent = {
    sourceLoc : Location
    movedLoc : Location
}

type AttackedEvent = {
    attackerLoc : Location
    targetLoc : Location
    damage : int
}

type Event =
    | Moved of MovedEvent
    | Attacked of AttackedEvent
    | Destroyed of Location

// Domain logic

module Location =
    let init x y = { x = x; y = y }
    let origin = init 0 0

    let getNextLocation loc dir =
        match dir with
        | East -> { loc with x = loc.x + 1 } 
        | West -> { loc with x = loc.x - 1 }
        | North -> { loc with y = loc.y - 1 }
        | South -> { loc with y = loc.y + 1 }

module Tile =
    let getChar tile =
        match tile.entity with
        | Some e ->
            match e.entityType with
            | Player -> '@'
            | Minion -> 'm'
        | None ->
            match tile.terrain with
            | Floor -> '.'
            | Wall -> '#' 

    let getMoveEvents loc nextLoc tile = seq {
        match tile.entity with
        | Some entity -> 
            if entity.hits = 1 then yield Destroyed nextLoc
            else
                yield Attacked {
                    attackerLoc = loc
                    targetLoc = nextLoc
                    damage = 1      
                    }
        | None -> yield Moved {
            sourceLoc = loc
            movedLoc = nextLoc
            }
        }
    
    let addEntity entity tile =
        { tile with entity = Some entity }

    let removeEntity tile =
        { tile with entity = None }

module Entity =
    let player = {
        entityType = Player
        hits = 3
    }

    let minion = {
        entityType = Minion
        hits = 1
    }

    let applyDamage damage entity =
        { entity with hits = entity.hits - damage }

module World =
    let empty = {
        turn = 0
        randomState = 0UL
        tiles = Map.empty
    }

    let xSemiExtent = 20
    let ySemiExtent = 4

    let generate seed =
        let tiles = seq {
            let rand = System.Random seed
            for y = -ySemiExtent to ySemiExtent do
                for x = -xSemiExtent to xSemiExtent do
                    let loc = Location.init x y
                    yield loc, {
                        terrain = Floor
                        entity =
                            if loc = Location.origin then Some Entity.player
                            elif rand.Next 8 = 0 then Some Entity.minion
                            else None
                    }
            }
        { empty with 
            randomState = uint64 seed
            tiles = Map.ofSeq tiles
        }

    let getCharAt loc world =
        match Map.tryFind loc world.tiles with
        | Some tile -> Tile.getChar tile
        | None -> ' '

    let formatRegion min max world =
        let sb = System.Text.StringBuilder()
        for y = min.y to max.y do
            for x = min.x to max.x do
                let loc = Location.init x y
                let ch = getCharAt loc world
                sb.Append ch |> ignore
            sb.AppendLine() |> ignore
        sb.ToString()

    let tryGetEntity loc world =
        Map.tryFind loc world.tiles
        |> Option.bind (fun tile -> tile.entity)

    let mapTile map loc world =
        match Map.tryFind loc world.tiles with
        | Some tile -> { world with tiles = Map.add loc (map tile) world.tiles }
        | None -> world

    let mapEntity map loc world =
        mapTile (fun tile -> { tile with entity = Option.map map tile.entity }) loc world

    let addEntity loc entity world =
        mapTile (Tile.addEntity entity) loc world

    let removeEntity loc world =
        mapTile Tile.removeEntity loc world

    let moveEntity loc newLoc world =
        match tryGetEntity loc world with
        | Some entity ->
            world 
            |> removeEntity loc
            |> addEntity newLoc entity
        | None -> world

    let find entityType world =
        world.tiles
        |> Map.tryPick (fun loc tile ->
            tile.entity 
            |> Option.bind (fun e -> 
                if e.entityType = entityType then Some (loc, e) else None))

    let stepTurn world =
        { world with turn = world.turn + 1 }

module Action =
    let getEvents action loc world =
        match action with
        | Move dir ->
            let nextLoc = Location.getNextLocation loc dir
            match Map.tryFind nextLoc world.tiles with
            | Some tile -> Tile.getMoveEvents loc nextLoc tile
            | None -> Seq.empty

    let getPlayerEvents action world =
        match World.find EntityType.Player world with
        | Some (loc, _) -> getEvents action loc world
        | None -> Seq.empty

module Event =
    let applyEvent world event =
        let update =
            match event with
            | Attacked event -> World.mapEntity (Entity.applyDamage event.damage) event.targetLoc
            | Moved event -> World.moveEntity event.sourceLoc event.movedLoc
            | Destroyed loc -> World.removeEntity loc
        update world

// Interaction loop

module Program =
    open System

    let tryGetAction key =
        match key with
        | ConsoleKey.RightArrow -> Move East |> Some
        | ConsoleKey.LeftArrow -> Move West |> Some
        | ConsoleKey.UpArrow -> Move North |> Some
        | ConsoleKey.DownArrow -> Move South |> Some
        | _ -> None

    let readPlayerActions() = seq {
        let mutable isRunning = true
        while isRunning do
            let key = Console.ReadKey().Key
            match tryGetAction key with
            | Some action -> yield action
            | None -> isRunning <- key <> ConsoleKey.Escape
        }

    let printWorld world =
        let min = Location.init -World.xSemiExtent -World.ySemiExtent
        let max = Location.init World.xSemiExtent World.ySemiExtent
        printfn "Turn %d:\n%s" world.turn <| World.formatRegion min max world

    let stepWorld world action =
        let events = Action.getPlayerEvents action world
        let newWorld = 
            events
            |> Seq.fold Event.applyEvent world
            |> World.stepTurn
        newWorld

    let run world =
        readPlayerActions()
        |> Seq.scan stepWorld world
        |> Seq.iter printWorld

World.generate 1 |> Program.run
