#load "MonoGameFsi.fsx"

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input

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


module MonoGame =
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


type TestGame() as x =
    inherit Game()
    do x.Content.RootDirectory <- "."
    let graphics = new GraphicsDeviceManager(x)
    override x.Initialize() =
        x.IsMouseVisible <- true
    override x.Update gameTime = 
        ()
    override x.Draw gameTime = 
        x.GraphicsDevice.Clear(Color.Blue)

let g = new TestGame()
g.Run()
g.Dispose()

