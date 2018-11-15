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

type EmptinessType =
    | DeadEnd
    | Hall

type KeyType =
    | RedKey
    | GreenKey
    | BlueKey
    | CyanKey

type PotionType =
    | Health
    | Freeze
    | Invulnerability
    | Treasure

[<RequireQualifiedAccess>]
type CellState =
    | Empty of EmptinessType
    | Diamond
    | Wall
    | Door of KeyType
    | Lock of KeyType
    | Key of KeyType
    | Potion of PotionType
    | Base
    | Outside
    | Avatar
    | Monster

type GameState =
    {random:Random;
    board:Map<Position, CellState>;
    avatar:Position}

module GameState =
    open Grumpy.Common

    let boardColumns = 256
    let boardRows    = 256
    let boardSize    = (boardColumns, boardRows)

    let mazeColumns  = 16
    let mazeRows     = 16
    let mazeSize     = (mazeColumns, mazeRows)

    let mazeCellColumns = boardColumns / mazeColumns
    let mazeCellRows    = boardRows    / mazeRows
    let mazeCellSize    = (mazeCellColumns, mazeCellRows)

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
                    |> Map.add p (CellState.Empty Hall)) 
        {gameState with board = board}

    let setCellState (cellState:CellState) (position:Position) (gameState:GameState) : GameState =
        let board =
            gameState.board
            |> Map.add (position |> Position.wrap boardSize) cellState
        {gameState with board = board}

    let getCellState (position:Position) (gameState:GameState) : CellState option=
        match position |> gameState.board.TryFind with
        | None -> Some CellState.Outside 
        | x -> x

    let private mazeCellNorthDoor =
        [1..(mazeCellColumns-2)]
        |> List.map (fun column->(column,0))

    let private mazeCellWestDoor =
        [1..(mazeCellRows-2)]
        |> List.map (fun row->(0,row))

    let private mazeCellSouthDoor =
        [1..(mazeCellColumns-2)]
        |> List.map (fun column->(column,mazeCellRows-1))

    let private mazeCellEastDoor =
        [1..(mazeCellRows-2)]
        |> List.map (fun row->(mazeCellColumns-1,row))

    let private mazeCellDoorTable =
        Map.empty
        |> Map.add North mazeCellNorthDoor
        |> Map.add South mazeCellSouthDoor
        |> Map.add East  mazeCellEastDoor
        |> Map.add West  mazeCellWestDoor

    let private mazeCellNorthWall =
        [1..(mazeCellColumns-2)]
        |> List.map (fun column->(column,0))

    let private mazeCellWestWall =
        [1..(mazeCellRows-2)]
        |> List.map (fun row->(0,row))

    let private mazeCellSouthWall =
        [1..(mazeCellColumns-2)]
        |> List.map (fun column->(column,mazeCellRows-1))

    let private mazeCellEastWall =
        [1..(mazeCellRows-2)]
        |> List.map (fun row->(mazeCellColumns-1,row))

    let private mazeCellCorners = 
        [(0,0);(0,mazeCellRows-1);(mazeCellColumns-1,0);(mazeCellColumns-1,mazeCellRows-1)]
        |> List.append mazeCellNorthWall
        |> List.append mazeCellSouthWall
        |> List.append mazeCellEastWall
        |> List.append mazeCellWestWall

    let private mazeCellWallTable =
        Map.empty
        |> Map.add North mazeCellNorthWall
        |> Map.add South mazeCellSouthWall
        |> Map.add East mazeCellEastWall
        |> Map.add West mazeCellWestWall

    let private mazeCellInterior =
        [for column=1 to mazeCellColumns-2 do
            for row = 1 to mazeCellRows-2 do
                yield (column,row)]

    let makeTranslateTransform (position:Position) =
        position 
        |> Position.multiply mazeCellSize
        |> Position.add

    let private mazeCellFold acc position mazeCell =
        let translate = 
            position 
            |> makeTranslateTransform
        let doors =
            mazeCell
            |> Set.fold
                (fun acc direction ->
                    mazeCellWallTable.[direction]
                    |> List.append acc) []
        let (interior, lockedDoor) =
            match mazeCell with
            | IsDeadEnd -> (mazeCellInterior, mazeCellDoorTable.[mazeCell |> Set.toList |> List.head])
            | _ -> ([],[])
        let painter (cs:CellState) (l:Position list) =
            List.foldBack
                (fun v acc -> 
                    acc
                    |> setCellState cs v) (l |> List.map translate)
        acc
        |> painter CellState.Wall mazeCellCorners
        |> painter (CellState.Empty DeadEnd) interior
        |> painter (CellState.Empty Hall) doors
        |> painter (CellState.Door RedKey) lockedDoor

    let private placeMazeWalls (maze:Maze<Direction>) (gameState:GameState) : GameState =
        let deadEnds =
            maze
            |> Map.filter 
                (fun _ v ->
                    match v with 
                    | IsDeadEnd -> true
                    | _ -> false)
            |> Map.map
                (fun k v -> v |> Set.toList |> List.head)
        let gameState =
            (gameState, maze)
            ||> Map.fold mazeCellFold
        (gameState, deadEnds)
        ||> Map.fold
            (fun acc position direction -> 
                let translate = makeTranslateTransform position
                //TODO: choose a keytype and place a door in position
                //TODO: select a "lock" cell for the door
                //TODO: place the key of the appropriate type in a "hall" cell
                acc)

    let private placeAvatar (gameState:GameState) : GameState =
        let avatarPosition =
            gameState.board
            |> Map.filter 
                (fun _ v ->
                    v = CellState.Empty Hall)
            |> Map.toList
            |> List.map fst
            |> List.sortBy (fun _ -> gameState.random.Next())
            |> List.head
        {gameState with avatar = avatarPosition}

    let populate (gameState:GameState) : GameState =
        let maze =
            Maze.generate gameState.random Direction.set Direction.opposite Direction.walk mazeSize

        let gameState =
            boardPositions
            |> List.fold
                (fun acc p ->
                    acc
                    |> setCellState (CellState.Empty Hall) p) (gameState |> clear)

        gameState
        |> placeMazeWalls maze
        |> placeAvatar

    let create () : GameState =
        let random = new Random()

        {random = random;
        board   = Map.empty;
        avatar  = (0, 0)}
        |> clear
        |> populate

    let restart = Utility.parameterShim create

    let moveAvatar (direction:Direction) (gameState:GameState) : GameState =
        let nextPosition = direction |> Direction.toPosition |> Position.add gameState.avatar
        match gameState |> getCellState nextPosition with
        | Some (CellState.Empty _) -> {gameState with avatar = nextPosition}
        | _ -> gameState



