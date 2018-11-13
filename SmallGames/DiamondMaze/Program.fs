open Grumpy.Common
open Grumpy.DiamondMaze

[<EntryPoint>]
let main argv = 
    use game = new CommonGame<TextureIdentifier,GameState>(Layout.backBufferSize,Render.textureFileNames,GameState.create,Render.drawGameState,Input.handleInput,Input.handleTime)
    game.Run()
    0
