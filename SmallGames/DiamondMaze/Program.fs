open Grumpy.Common
open Grumpy.DiamondMaze

[<EntryPoint>]
let main argv = 
    use game = new CommonGame<TextureIdentifier,GameState,Context>(Layout.backBufferSize,Render.textureFileNames,GameState.create, Context.create, Render.drawGameState,Input.handleInput,Input.handleTime)
    game.Run()
    0
