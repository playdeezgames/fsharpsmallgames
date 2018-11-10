open Grumpy.Common
open Utility
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open System
open Microsoft.Xna.Framework

type CellState =
    | Dark
    | Light

type GameState =
    {random:Random;
    board:Map<Position,CellState>}

type TextureIdentifier =
    | RomFont
    | Empty
    | EmptyNW
    | EmptyNE
    | EmptySW
    | EmptySE
    | PieceDark
    | PieceLight

let cellColumns = 8
let cellRows = 8
let cellWidth = 96
let cellHeight = 96

let backBufferSize = (cellColumns * cellWidth,cellRows * cellHeight)
let outputSize = (16, 16)

let textureFileNames = 
    [(RomFont,"Content/romfont8x8.png");
    (Empty,"Content/empty.png");
    (PieceDark,"Content/piece-dark.png");
    (PieceLight,"Content/piece-light.png");
    (EmptyNW,"Content/empty-nw.png");
    (EmptyNE,"Content/empty-ne.png");
    (EmptySW,"Content/empty-sw.png");
    (EmptySE,"Content/empty-se.png")]

let boardBackground =
    [for column=0 to (cellColumns-1) do
        for row=0 to (cellRows-1) do
            yield (column,row)]
    |> List.map (fun p -> (p,Empty))
    |> Map.ofList
    |> Map.add (1,1) EmptySE
    |> Map.add (1,2) EmptyNE
    |> Map.add (2,1) EmptySW
    |> Map.add (2,2) EmptyNW
    |> Map.add (cellColumns - 3,1) EmptySE
    |> Map.add (cellColumns - 3,2) EmptyNE
    |> Map.add (cellColumns - 2,1) EmptySW
    |> Map.add (cellColumns - 2,2) EmptyNW
    |> Map.add (1,cellRows - 3) EmptySE
    |> Map.add (1,cellRows - 2) EmptyNE
    |> Map.add (2,cellRows - 3) EmptySW
    |> Map.add (2,cellRows - 2) EmptyNW
    |> Map.add (cellColumns - 3,cellRows - 3) EmptySE
    |> Map.add (cellColumns - 3,cellRows - 2) EmptyNE
    |> Map.add (cellColumns - 2,cellRows - 3) EmptySW
    |> Map.add (cellColumns - 2,cellRows - 2) EmptyNW

let initialBoard =
    Map.empty
    |> Map.add (3,3) Dark
    |> Map.add (4,4) Dark
    |> Map.add (3,4) Light
    |> Map.add (4,3) Light

let createGameState () =
    {random = new Random();
    board = initialBoard}

let cellStateTextureIdentifiers =
    [(Dark, PieceDark);
    (Light, PieceLight)]
    |>Map.ofList

let drawGameState (textures:Map<TextureIdentifier,Texture2D>) (spriteBatch:SpriteBatch) (gameState:GameState) : unit =
    let destRect (position:Position) : Rectangle =
        new Rectangle((position|>fst)*cellWidth,(position|>snd)*cellHeight,cellWidth,cellHeight)

    boardBackground
    |> Map.iter
        (fun position cell ->
            spriteBatch.Draw(textures.[cell],position |> destRect, Color.White))

    gameState.board
    |> Map.iter
        (fun position cell ->
            spriteBatch.Draw(textures.[cellStateTextureIdentifiers.[cell]],position |> destRect, Color.White))


let wasKeyPressed (key:Keys) (oldState:KeyboardState,newState:KeyboardState) : bool =
    (key |> oldState.IsKeyDown |> not) && (key |> newState.IsKeyDown)

let handleInput (keyboardStates:KeyboardState*KeyboardState) (gameState:GameState) : GameState =
    gameState


let handleTime (delta:GameTime) (gameState:GameState) : GameState =
    gameState
    

[<EntryPoint>]
let main argv = 
    use game = new CommonGame<TextureIdentifier,GameState>(backBufferSize,textureFileNames,createGameState,drawGameState,handleInput,handleTime)
    game.Run()
    0
