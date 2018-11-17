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
    avatar:Position;
    inventory:CellState list}

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

    let private painter (translate:Position->Position) (cs:CellState) (l:Position list) =
        List.foldBack
            (fun v acc -> 
                acc
                |> setCellState cs v) (l |> List.map translate)

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
        let interior =
            match mazeCell with
            | IsDeadEnd -> mazeCellInterior
            | _ -> []
        acc
        |> painter translate CellState.Wall mazeCellCorners
        |> painter translate (CellState.Empty DeadEnd) interior
        |> painter translate (CellState.Empty Hall) doors

    let keyTypes = [(RedKey,1);(BlueKey,1);(GreenKey,1);(CyanKey,1)] |> Map.ofList

    let private placeKeys (gameState:GameState) (keyList:CellState list): GameState =
        gameState.board
        |> Map.filter
            (fun _ v -> v = (CellState.Empty Hall))
        |> Map.toList
        |> List.map fst
        |> List.sortBy (fun _ -> gameState.random.Next())
        |> List.splitAt (keyList.Length)
        |> fst
        |> List.zip keyList
        |> List.fold
            (fun acc (cs,p) -> 
                acc
                |> setCellState cs p) gameState

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
        ((gameState,[]), deadEnds)
        ||> Map.fold
            (fun (acc:GameState,x) position direction -> 
                let translate = makeTranslateTransform position
                let keyType = keyTypes |> Utility.generate gameState.random
                let lock, door = 
                    mazeCellDoorTable.[direction]
                    |> List.sortBy (fun _ -> gameState.random.Next())
                    |> List.splitAt 1
                (acc
                |> painter translate (CellState.Lock keyType) lock
                |> painter translate (CellState.Door keyType) door),
                x |> List.append [CellState.Key keyType])
        ||> placeKeys

    let private findCellStates (filter:CellState->bool) (gameState:GameState) : Position list =
        gameState.board
        |> Map.filter 
            (fun _ v ->
                v |> filter)
        |> Map.toList
        |> List.map fst
        |> List.sortBy (fun _ -> gameState.random.Next())

    let private placeAvatar (gameState:GameState) : GameState =
        let avatarPosition =
            gameState
            |> findCellStates (fun cs -> cs = CellState.Empty Hall)
            |> List.head
        {gameState with avatar = avatarPosition}

    let deadEndDiamondCount = 256
    let hallDiamondCount = 256
    let baseCount = 16
    let potionCount = 8
    let potions =
        [for index = 1 to potionCount do
            yield [Health;Freeze;Invulnerability;Treasure]]
        |> List.reduce (@)

    let placeItems  (gameState:GameState) : GameState =
        let (deadEndDiamondPositions,_) =
            gameState
            |> findCellStates (fun cs -> cs = CellState.Empty DeadEnd)
            |> List.splitAt deadEndDiamondCount
        let (hallDiamondPositions, remainingHallPositions) =
            gameState
            |> findCellStates (fun cs -> cs = CellState.Empty Hall)
            |> List.splitAt hallDiamondCount
        let (basePositions, remainingHallPositions) =
            remainingHallPositions
            |> List.splitAt baseCount
        let potionPositions =
            remainingHallPositions
            |> List.splitAt potions.Length
            |> fst
            |> List.zip potions
        gameState
        |> List.foldBack
            (fun p acc-> 
                acc
                |> setCellState CellState.Diamond p) (hallDiamondPositions |> List.append deadEndDiamondPositions)
        |> List.foldBack
            (fun p acc-> 
                acc
                |> setCellState CellState.Base p) basePositions
        |> List.foldBack
            (fun (pt,p) acc-> 
                acc
                |> setCellState (CellState.Potion pt) p) potionPositions

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
        |> placeItems

    let create () : GameState =
        let random = new Random()

        {random = random;
        board   = Map.empty;
        avatar  = (0, 0);
        inventory = []}
        |> clear
        |> populate

    let restart = Utility.parameterShim create

    let isInventoryItem (cellState: CellState): bool =
        match cellState with
        | CellState.Diamond
        | CellState.Key _ -> true
        | _ -> false

    let inventoryColumns = 9
    let inventoryRows = 4
    let inventoryCapacity = inventoryColumns * inventoryRows

    let canPickupItem (gameState:GameState) : bool =
        gameState.inventory.Length < inventoryCapacity

    let addInventoryItem (item:CellState) (gameState:GameState) :GameState =
        {gameState with inventory = gameState.inventory |> List.append [item]}

    let moveAvatar (direction:Direction) (gameState:GameState) : GameState =
        let nextPosition = direction |> Direction.toPosition |> Position.add gameState.avatar
        match gameState |> getCellState nextPosition with
        | Some (CellState.Empty _) -> 
            {gameState with avatar = nextPosition}
        | Some x when x |> isInventoryItem -> 
            if gameState |> canPickupItem then
                {gameState with avatar = nextPosition}
                |> setCellState (CellState.Empty Hall) nextPosition
                |> addInventoryItem x
            else
                gameState
        | _ -> gameState



