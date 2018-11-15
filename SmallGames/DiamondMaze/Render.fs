namespace Grumpy.DiamondMaze
type TextureIdentifier =
    | RomFont


module Render =
    open Grumpy.Common
    open Microsoft.Xna.Framework
    open Microsoft.Xna.Framework.Graphics

    let textureFileNames = 
        [(RomFont, "Content/romfont8x8.png")]

    let outputRect (position:Position) : Rectangle =
        new Rectangle((position |> fst) * Layout.cellWidth,(position |> snd) * Layout.cellHeight,Layout.cellWidth,Layout.cellHeight)

    let inputRect (texture:Texture2D) (position:Position) : Rectangle =
        new Rectangle((position |> fst) * texture.Width/16,(position |> snd) * texture.Height/16,texture.Width/16,texture.Height/16)

    let renderCell (texture:Texture2D) (position:Position) (cell:RenderCell) (spriteBatch:SpriteBatch) : unit =
        //draw background
        spriteBatch.Draw(texture, position |> outputRect, (0xb,0xd) |> inputRect texture |> Some |> Option.toNullable, cell.background)
        //draw foreground
        spriteBatch.Draw(texture, position |> outputRect, ((cell.character |> int) % 16,(cell.character |> int) / 16) |> inputRect texture |> Some |> Option.toNullable, cell.foreground)

    let renderCells =
        [(CellState.Wall, (Color.Black, Color.Gray, 0xb2uy) |||> RenderCell.makeRenderCell);
        (CellState.Empty Hall, (Color.Black, Color.Black, 0x00uy) |||> RenderCell.makeRenderCell);
        (CellState.Empty DeadEnd, (Color.Black, Color.Black, 0x00uy) |||> RenderCell.makeRenderCell);
        (CellState.Door RedKey, (Color.Black, Color.Red, 0xb1uy) |||> RenderCell.makeRenderCell);
        (CellState.Avatar, (Color.Black, Color.LightPink, 0x02uy) |||> RenderCell.makeRenderCell);
        (CellState.Outside, (Color.Black, Color.Blue, 0xb0uy) |||> RenderCell.makeRenderCell)]
        |> Map.ofList

    let drawGameState (textures:Map<TextureIdentifier,Texture2D>) (spriteBatch:SpriteBatch) (gameState:GameState) : unit =
        //draw the frame
        Layout.fieldFrameCells
        |> Map.iter
            (fun k v ->
                renderCell textures.[RomFont] k v spriteBatch)

        let renderGameState = 
            gameState
            |> GameState.setCellState CellState.Avatar gameState.avatar
        Layout.fieldPositions
        |> List.iter
            (fun (p, boardOffset) ->
                renderGameState
                |> GameState.getCellState (boardOffset |> Position.add gameState.avatar)
                |> Option.map
                    (fun x -> renderCells.[x])
                |> Option.iter
                    (fun cell -> spriteBatch |> renderCell textures.[RomFont] p cell))

