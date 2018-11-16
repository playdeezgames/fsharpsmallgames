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
        [(CellState.Wall,         (Color.Black, Color.Gray,      0xb2uy) |||> RenderCell.makeRenderCell);
        (CellState.Empty Hall,    (Color.Black, Color.Black,     0x00uy) |||> RenderCell.makeRenderCell);
        (CellState.Empty DeadEnd, (Color.Black, Color.Black,     0x00uy) |||> RenderCell.makeRenderCell);
        (CellState.Door RedKey,   (Color.Black, Color.Red,       0xdbuy) |||> RenderCell.makeRenderCell);
        (CellState.Door BlueKey,  (Color.Black, Color.Blue,      0xdbuy) |||> RenderCell.makeRenderCell);
        (CellState.Door GreenKey, (Color.Black, Color.Green,     0xdbuy) |||> RenderCell.makeRenderCell);
        (CellState.Door CyanKey,  (Color.Black, Color.Cyan,      0xdbuy) |||> RenderCell.makeRenderCell);
        (CellState.Lock RedKey,   (Color.Black, Color.Red,       0x08uy) |||> RenderCell.makeRenderCell);
        (CellState.Lock BlueKey,  (Color.Black, Color.Blue,      0x08uy) |||> RenderCell.makeRenderCell);
        (CellState.Lock GreenKey, (Color.Black, Color.Green,     0x08uy) |||> RenderCell.makeRenderCell);
        (CellState.Lock CyanKey,  (Color.Black, Color.Cyan,      0x08uy) |||> RenderCell.makeRenderCell);
        (CellState.Key RedKey,    (Color.Black, Color.Red,       0x07uy) |||> RenderCell.makeRenderCell);
        (CellState.Key BlueKey,   (Color.Black, Color.Blue,      0x07uy) |||> RenderCell.makeRenderCell);
        (CellState.Key GreenKey,  (Color.Black, Color.Green,     0x07uy) |||> RenderCell.makeRenderCell);
        (CellState.Key CyanKey,   (Color.Black, Color.Cyan,      0x07uy) |||> RenderCell.makeRenderCell);
        (CellState.Avatar,        (Color.Black, Color.LightPink, 0x02uy) |||> RenderCell.makeRenderCell);
        (CellState.Diamond,       (Color.Black, Color.Yellow,    0x04uy) |||> RenderCell.makeRenderCell);
        (CellState.Outside,       (Color.Black, Color.Blue,      0xb0uy) |||> RenderCell.makeRenderCell)]
        |> Map.ofList

    let inventoryCells =
        [for row=0 to (Layout.inventoryRows-1) do
            for column=0 to (Layout.inventoryColumns-1) do
                yield (Layout.inventoryOffsetX + column,Layout.inventoryOffsetY + row)]

    let drawGameState (textures:Map<TextureIdentifier,Texture2D>) (spriteBatch:SpriteBatch) (gameState:GameState) : unit =
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

        inventoryCells
        |> List.splitAt gameState.inventory.Length
        |> fst
        |> List.zip gameState.inventory
        |> List.map
            (fun (cs,p) -> (renderCells.[cs], p))
        |> List.iter
            (fun (rc,p) -> 
                spriteBatch |> renderCell textures.[RomFont] p rc)
