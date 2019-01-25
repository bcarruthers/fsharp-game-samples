#load "MonoGameFsi.fsx"

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input

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

