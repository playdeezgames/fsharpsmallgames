open Grumpy.Common
open Utility
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open System
open Microsoft.Xna.Framework
open System.Globalization

type CellState =
    | Dark
    | Light

let getOpposing (cellState:CellState option) : CellState option=
    match cellState with
    | Some Dark  -> Some Light
    | Some Light -> Some Dark
    | None       -> None

type GameState =
    {random:Random;
    board:Map<Position,CellState>;
    currentPlayer:CellState option;
    cursorPosition:Position}

type TextureIdentifier =
    | RomFont
    | Empty
    | EmptyNW
    | EmptyNE
    | EmptySW
    | EmptySE
    | PieceDark
    | PieceLight
    | CursorDark
    | CursorLight
    | GhostDark
    | GhostLight

let cellColumns = 8
let cellRows = 8
let cellWidth = 96
let cellHeight = 96

let backBufferSize = (cellColumns * cellWidth,cellRows * cellHeight)
let outputSize = (16, 16)

let textureFileNames = 
    [
    (RomFont     , "Content/romfont8x8.png"   );
    (Empty       , "Content/empty.png"        );
    (PieceDark   , "Content/piece-dark.png"   );
    (PieceLight  , "Content/piece-light.png"  );
    (GhostDark   , "Content/ghost-dark.png"   );
    (GhostLight  , "Content/ghost-light.png"  );
    (CursorDark  , "Content/cursor-dark.png"  );
    (CursorLight , "Content/cursor-light.png" );
    (EmptyNW     , "Content/empty-nw.png"     );
    (EmptyNE     , "Content/empty-ne.png"     );
    (EmptySW     , "Content/empty-sw.png"     );
    (EmptySE     , "Content/empty-se.png"     )]

let boardPositions = 
    [for column=0 to (cellColumns-1) do
        for row=0 to (cellRows-1) do
            yield (column,row)]

let boardBackground =
    boardPositions
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
    board = initialBoard;
    currentPlayer = Some Dark;
    cursorPosition = (0,0)}

let toPieceTextureIdentifier (cellState:CellState) : TextureIdentifier =
    match cellState with
    | Dark -> PieceDark
    | Light -> PieceLight

let toCursorTextureIdentifier (cellState:CellState) : TextureIdentifier =
    match cellState with
    | Dark -> CursorDark
    | Light -> CursorLight

let toGhostTextureIdentifier (cellState:CellState) : TextureIdentifier =
    match cellState with
    | Dark -> GhostDark
    | Light -> GhostLight

type Direction = 
    | North
    | NorthEast
    | East
    | SouthEast
    | South
    | SouthWest
    | West
    | NorthWest

let toDeltaPosition (direction:Direction) : Position =
    match direction with
    | North     -> ( 0,-1)
    | NorthEast -> ( 1,-1)
    | East      -> ( 1, 0)
    | SouthEast -> ( 1, 1)
    | South     -> ( 0, 1)
    | SouthWest -> (-1, 1)
    | West      -> (-1, 0)
    | NorthWest -> (-1,-1)

let getCellState (position:Position) (gameState:GameState) : CellState option =
    gameState.board
    |> Map.tryPick 
        (fun k v -> if k = position then Some v else None)

let setCellState (cellState:CellState) (position:Position) (gameState:GameState) : GameState =
    {gameState with 
        board = gameState.board |> Map.add position cellState}

let rec validateRun (continueCellState:CellState option) (successCellState:CellState option) (position:Position) (direction:Direction) (gameState:GameState) : bool =
    let nextPosition  = direction |> toDeltaPosition |> Position.add position
    match gameState |> getCellState nextPosition with
    | x when x = successCellState  -> true
    | x when x = continueCellState -> gameState |> validateRun continueCellState successCellState nextPosition direction
    | _                            -> false


let isValidRun (position: Position) (direction: Direction) (gameState:GameState) : bool =
    let nextPosition  = direction |> toDeltaPosition |> Position.add position
    match gameState |> getCellState nextPosition with
    | x when x = (gameState.currentPlayer |> getOpposing) -> 
        gameState 
        |> validateRun x gameState.currentPlayer nextPosition direction
    | _ -> 
        false

let directions = [North;NorthEast;East;SouthEast;South;SouthWest;West;NorthWest]

let isOnBoard (position: Position) (gameState:GameState) : bool =
    boardPositions |> List.exists (fun x-> x=position)

let isValidMove (position:Position) (gameState:GameState) : bool =
    if boardPositions |> List.exists (fun x-> x=position) |> not then   
        false
    elif position |> gameState.board.ContainsKey then
        false
    else
        directions
        |> List.exists 
            (fun direction ->
                isValidRun position direction gameState)

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
            spriteBatch.Draw(textures.[cell |> toPieceTextureIdentifier],position |> destRect, Color.White))

    gameState.currentPlayer
    |> Option.iter 
        (fun player ->
            boardPositions
            |> List.iter
                (fun position -> 
                    if gameState |> isValidMove position then
                        spriteBatch.Draw(textures.[player |> toGhostTextureIdentifier], position |> destRect, Color.White))
            spriteBatch.Draw(textures.[player |> toCursorTextureIdentifier], gameState.cursorPosition |> destRect, Color.White))

let wasKeyPressed (key:Keys) (oldState:KeyboardState,newState:KeyboardState) : bool =
    (key |> oldState.IsKeyDown |> not) && (key |> newState.IsKeyDown)

let moveCursor (direction:Direction) (gameState:GameState) : GameState =
    let nextPosition = direction |> toDeltaPosition |> Position.add gameState.cursorPosition
    match boardPositions |> List.tryFind (fun x -> x = nextPosition) with
    | Some x -> {gameState with cursorPosition = nextPosition}
    | _ -> gameState

let rec continueRun (continueCellState:CellState option) (successCellState:CellState option) (position:Position) (direction:Direction) (gameState:GameState) : GameState =
    let nextPosition  = direction |> toDeltaPosition |> Position.add position
    match gameState |> getCellState nextPosition with
    | x when x = continueCellState -> 
        gameState 
        |> continueRun continueCellState successCellState nextPosition direction
    | _ -> 
        gameState
    |> setCellState successCellState.Value position

let applyRun (direction:Direction) (gameState:GameState) : GameState =
    let nextPosition  = direction |> toDeltaPosition |> Position.add gameState.cursorPosition
    match gameState |> getCellState nextPosition with
    | x when x = (gameState.currentPlayer |> getOpposing) -> 
        gameState 
        |> continueRun x gameState.currentPlayer nextPosition direction
    | _ -> 
        gameState

let hasAnyValidMoves (gameState: GameState) : bool =
    boardPositions
    |> List.exists
        (fun p ->
            gameState |> isValidMove p)

let nextPlayer (gameState:GameState) : GameState =
    let opposing = gameState.currentPlayer |> getOpposing
    let nextPlayer =
        if {gameState with currentPlayer = opposing} |> hasAnyValidMoves then
            opposing
        elif gameState |> hasAnyValidMoves then
            gameState.currentPlayer
        else
            None
    {gameState with 
        currentPlayer = nextPlayer}

let makeMove (gameState:GameState) : GameState =
    if (gameState.cursorPosition, gameState) ||> isValidMove then
        directions
        |> List.filter
            (fun d -> 
                (gameState.cursorPosition, d, gameState)
                |||> isValidRun)
        |> List.fold
            (fun acc d ->
                acc
                |> applyRun d) gameState
        |> setCellState gameState.currentPlayer.Value gameState.cursorPosition
    else
        gameState
    //TODO: move to next player!

let inputHandlers: Map<Keys,GameState->GameState> =
    [(Keys.Up, moveCursor North);
    (Keys.Down, moveCursor South);
    (Keys.Right, moveCursor East);
    (Keys.Left, moveCursor West);
    (Keys.Space, makeMove)]
    |> Map.ofList


let handleInput (keyboardStates:KeyboardState*KeyboardState) (gameState:GameState) : GameState =
    inputHandlers
    |> Map.filter (fun k _ -> keyboardStates |> wasKeyPressed k)
    |> Map.fold (fun acc _ v -> acc |> v) gameState

let handleTime (delta:GameTime) (gameState:GameState) : GameState =
    gameState
    

[<EntryPoint>]
let main argv = 
    use game = new CommonGame<TextureIdentifier,GameState>(backBufferSize,textureFileNames,createGameState,drawGameState,handleInput,handleTime)
    game.Run()
    0
