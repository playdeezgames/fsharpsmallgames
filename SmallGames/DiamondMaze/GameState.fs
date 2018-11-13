namespace Grumpy.DiamondMaze

open System
open System.ComponentModel
open Grumpy.Common
open System.Globalization

//items in game and character codes
//dude = 0x02
//diamond = 0x04
//monster = 0x01
//wall = 0xdb
//doors = 0x08
//locks = 0x07
//heart = 0x03
//health potion = 0x09
//freeze potion = 0x0a
//treasure potion = 0x0b
//invulnerabilty potion = 0x0c
//bullet = 0x2a
//base = 0x7f

[<RequireQualifiedAccess>]
type CellState =
    | Empty
    | Diamond
    | Wall
    | Door
    | Lock
    | Key
    | Heart
    | HealthPotion
    | FreezePotion
    | TreasurePotion
    | InvulnerabilityPotion
    | Base

type GameState =
    {random:Random;
    board:Map<Position, CellState>;
    avatar:Position}

module GameState =
    let boardColumns = 256
    let boardRows    = 256
    let boardSize = (boardColumns, boardRows)

    let boardPositions =
        [for column = 0 to (boardColumns-1) do
            for row=0 to (boardRows-1) do
                yield (column,row)]
    
    let clear (gameState:GameState) : GameState =
        let board =
            (Map.empty, boardPositions)
            ||> List.fold 
                (fun acc p ->
                    acc
                    |> Map.add p CellState.Empty) 
        {gameState with board = board}

    let setCellState (cellState:CellState) (position:Position) (gameState:GameState) : GameState =
        let board =
            gameState.board
            |> Map.add (position |> Position.wrap boardSize) cellState
        {gameState with board = board}

    let getCellState (position:Position) (gameState:GameState) : CellState option=
        position
        |> Position.wrap boardSize
        |> gameState.board.TryFind

    let populator =
        [(CellState.Empty, 100);
        (CellState.Wall,10)]
        |> Map.ofList

    let populate (gameState:GameState) : GameState =
        boardPositions
        |> List.fold
            (fun acc p ->
                acc
                |> setCellState (populator |> Utility.generate gameState.random) p) (gameState |> clear)

    let create () : GameState =
        {random = new Random();
        board=Map.empty;
        avatar=(boardColumns/2,boardRows/2)}
        |> clear
        |> populate

    let restart (_) : GameState =
        create()

    let moveAvatar (direction:Direction) (gameState:GameState) : GameState =
        let nextPosition = direction |> Direction.toPosition |> Position.add gameState.avatar
        {gameState with avatar = nextPosition}



