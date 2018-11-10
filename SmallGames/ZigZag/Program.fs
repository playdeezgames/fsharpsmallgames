open Grumpy.Common
open Utility
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open System

type CellState =
    | Empty

type GameState =
    {random:Random;
    score:int;
    runLength:int;
    blocks:Position list;
    tail:Position list;
    direction:int;
    frameCounter:float;
    gameOver:bool}

type TextureIdentifier =
    | RomFont

let cellColumns = 40
let cellRows = 30
let cellWidth = 16
let cellHeight = 16
let tailLength = 6
let millisecondsPerFrame = 100.0

let backBufferSize = (cellColumns * cellWidth,cellRows * cellHeight)
let outputSize = (cellWidth, cellHeight)

let textureFileNames = [(RomFont,"Content/romfont8x8.png")]

let initialBlocks =
    [for row=0 to (cellRows-1) do yield (0,row)]

let initialTail =
    [for row=0 to (tailLength-1) do yield (cellColumns/2,row)]

let createGameState () =
    {random = new Random();
    score=0;
    runLength=0;
    gameOver=true;
    blocks=initialBlocks;
    direction=1;
    frameCounter=millisecondsPerFrame;
    tail=initialTail}

let drawGameState (textures:Map<TextureIdentifier,Texture2D>) (spriteBatch:SpriteBatch) (gameState:GameState) : unit =
    let writeText p = 
        writeText textures.[RomFont] spriteBatch outputSize (p|>Position.multiply outputSize)

    gameState.blocks
    |> List.iter
        (fun p ->
            writeText (p) "\u00DB" Color.White)


    [0..(cellRows-1)]
    |> List.iter
        (fun y -> 
            writeText (0,y) "\u00DB" Color.Blue
            writeText (cellColumns-1,y) "\u00DB" Color.Blue)

    let tailBlocks, headBlocks =
        gameState.tail
        |> List.splitAt (tailLength-1)
    tailBlocks
    |> List.iter
        (fun p ->
            writeText p "\u00DB" Color.Yellow)
    headBlocks
    |> List.iter
        (fun p ->
            writeText p "\u00DB" Color.Red)

    writeText (1,0) (gameState.score |> sprintf "%d") Color.Green

let resetGame (gameState:GameState) : GameState =
    {gameState with 
        blocks = initialBlocks;
        tail = initialTail;
        score=0;
        frameCounter=1.0;
        direction=1}

let startGame (gameState:GameState) : GameState =
    {gameState with
        gameOver=false}

let wasKeyPressed (key:Keys) (oldState:KeyboardState,newState:KeyboardState) : bool =
    (key |> oldState.IsKeyDown |> not) && (key |> newState.IsKeyDown)

let handleInput (keyboardStates:KeyboardState*KeyboardState) (gameState:GameState) : GameState =
    if gameState.gameOver then
        if keyboardStates |> wasKeyPressed Keys.Space then
            gameState
            |> resetGame
            |> startGame
        else
            gameState
    else
        if keyboardStates |> wasKeyPressed Keys.Left && gameState.direction<> (-1) then
            {gameState with 
                direction=(-1);
                score = gameState.score + (gameState.runLength * (gameState.runLength + 1) / 2);
                runLength=0}
        elif keyboardStates |> wasKeyPressed Keys.Right && gameState.direction<> 1 then
            {gameState with 
                direction=1;
                score = gameState.score + (gameState.runLength * (gameState.runLength + 1) / 2);
                runLength=0}
        else
            gameState

let advanceGame (gameState:GameState) : GameState =
    let blocks = 
        [(gameState.random.Next(cellColumns-2)+1,cellRows-1)]
        |> List.append
            ((gameState.blocks 
            |> List.splitAt 1) 
            |> snd 
            |> List.map (fun (x,y)->(x,y-1)))
    let tail =
        gameState.tail 
        |> List.splitAt 1
        |> snd
        |> List.map (fun (x,y)->(x,y-1))
    let head =
        gameState.tail 
        |> List.splitAt (tailLength-1)
        |> snd
        |> List.map (fun (x,y) -> (x+gameState.direction,y))
    {gameState with
        blocks = blocks;
        tail = List.append tail head;
        runLength = gameState.runLength+1;
        gameOver = blocks |> List.tryFind (fun x -> x = head.Head) |> Option.isSome || (head.Head |> fst) = 0 || (head.Head |> fst) = (cellColumns-1)}

let handleTime (delta:GameTime) (gameState:GameState) : GameState =
    if gameState.gameOver then
        gameState
    else
        match gameState.frameCounter - delta.ElapsedGameTime.TotalMilliseconds with
        | x when x<=0.0 ->
            {gameState with frameCounter = x+millisecondsPerFrame}
            |> advanceGame 
        | x ->
            {gameState with frameCounter = x}
    

[<EntryPoint>]
let main argv = 
    use game = new CommonGame<TextureIdentifier,GameState>(backBufferSize,textureFileNames,createGameState,drawGameState,handleInput,handleTime)
    game.Run()
    0
