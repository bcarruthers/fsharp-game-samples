# fsharp-game-samples

This is an assortment of .fsx game prototypes intended to run in F# interactive with minimal dependencies or project structure. Some of the code like SimpleRoguelike.fsx is intended to be idiomatic F#, but other code mixes in imperative/OOP code.

## Texture Atlas Utility

[ [Code](https://github.com/bcarruthers/fsharp-game-samples/tree/master/src/TextureAtlas) ]

This is a simple utility for packing textures into a single PNG texture and JSON descriptor. 

Instructions:

1. Install .NET Core SDK
2. Open command prompt in utility project folder
3. Example usage: dotnet run -- --source ./textures --output ./atlas