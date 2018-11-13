open System
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open System.IO
open Grumpy.Common

let cellWidth = 96
let cellHeight = 96
let cellColumns = 8
let cellRows = 8

let backBufferWidth = cellWidth * cellColumns
let backBufferHeight = cellRows * cellHeight
let backBufferSize = (backBufferWidth, backBufferHeight)

type SquareState = 
    | UNVISITED
    | OCCUPIED
    | VISITED

type Board = Map<Position, SquareState>

type GameState =
    {board:Board;
    cursorPosition:Position}

let legalMoves = 
    [(-2, 1);
     ( 2, 1);
     (-2,-1);
     ( 2,-1);
     (-1, 2);
     ( 1, 2);
     (-1,-2);
     ( 1,-2)]

let addPosition (first:Position) (second:Position) : Position =
    ((first |> fst) + (second |> fst), (first |> snd) + (second |> snd))

let makeBoard () : GameState =
    let columnFold (board:Board) (column:int) : Board =
        (board, [0..(cellRows-1)])
        ||> List.fold 
            (fun board row -> 
                board |> Map.add (column,row) UNVISITED)
    let board = 
        (Map.empty, [0..(cellColumns-1)])
        ||> List.fold columnFold
    {board = board; cursorPosition=(0,0)}

let board = makeBoard()

let isBoardStarting (board:Board) : bool =
    board
    |> Map.forall (fun _ v -> v = UNVISITED)

let isBoardInPlay (board:Board):bool =
    board
    |> Map.exists (fun _ v -> v = OCCUPIED)

let (|UNSOLVABLE|STARTING|INPLAY|) (board:Board) =
    match board with
    | x when x |> isBoardStarting -> STARTING
    | x when x |> isBoardInPlay -> INPLAY
    | _ -> UNSOLVABLE

let isLegalMove (position: Position) (board:Board) : bool =
    match board with 
    | STARTING -> board |> Map.containsKey position
    | INPLAY -> 
        legalMoves
        |> List.exists 
            (fun x -> 
                let destination = x |> addPosition position
                board |> Map.exists (fun k v -> k = destination && v = OCCUPIED))
    | _ -> false

let makeMove (gameState:GameState) : GameState =
    let board = 
        match gameState.board with
        | STARTING ->
            gameState.board
            |> Map.add gameState.cursorPosition OCCUPIED
        | INPLAY when gameState.board |> Map.exists (fun k v -> k=gameState.cursorPosition && v = UNVISITED)->
            let origin = 
                legalMoves
                |> List.map (addPosition gameState.cursorPosition)
                |> List.tryPick
                    (fun p -> 
                        gameState.board
                        |> Map.tryPick(fun k v -> if k = p && v = OCCUPIED then Some k else None))
            match origin with
            | Some p ->
                gameState.board
                |> Map.add p VISITED
                |> Map.add (gameState.cursorPosition) OCCUPIED
            | None -> 
                gameState.board
        | _ -> 
            gameState.board
    {gameState with board = board}

type TextureIdentifier = 
    | DarkAllowed
    | DarkEmpty
    | DarkOccupied
    | DarkVisited
    | LightAllowed
    | LightEmpty
    | LightOccupied
    | LightVisited
    | Cursor

let (|DARK|LIGHT|) (position: Position) =
    match position with
    | (x,y) when ((x + y) % 2) = 1 -> DARK
    | _ -> LIGHT
        

let drawBoard (textures:Map<TextureIdentifier,Texture2D>) (spriteBatch:SpriteBatch) (board:Board) : unit =
    board
    |> Map.iter 
        (fun position state ->
            let textureIdentifier = 
                match (position, state, board |> isLegalMove position) with
                | LIGHT, UNVISITED, true ->
                    LightAllowed
                | LIGHT, UNVISITED, false ->
                    LightEmpty
                | LIGHT, OCCUPIED, _ ->
                    LightOccupied
                | LIGHT, VISITED, _ ->
                    LightVisited
                | DARK, UNVISITED, true ->
                    DarkAllowed
                | DARK, UNVISITED, false ->
                    DarkEmpty
                | DARK, OCCUPIED, _ ->
                    DarkOccupied
                | DARK, VISITED, _ ->
                    DarkVisited
            spriteBatch.Draw(textures.[textureIdentifier], new Rectangle((position |> fst)*cellWidth,(position |> snd)*cellHeight,cellWidth,cellHeight), Color.White))

let isKeyPressed (key:Keys) (oldKeyboardState:KeyboardState) (newKeyboardState:KeyboardState) : bool = 
    oldKeyboardState.IsKeyUp(key) && newKeyboardState.IsKeyDown(key)

let moveCursor (delta:Position) (gameState:GameState) : GameState =
    let cursor = 
        match addPosition delta gameState.cursorPosition with
        | (x,y) when x<0 || y<0 || x>=cellColumns || y>=cellRows -> gameState.cursorPosition
        | x -> x
    {gameState with cursorPosition=cursor}

let inputHandlers: Map<Keys,GameState->GameState> =
    [(Keys.F2, makeBoard |> Utility.parameterShim);
    (Keys.Space, makeMove);
    (Keys.Up, moveCursor (0,-1));
    (Keys.Down, moveCursor (0,1));
    (Keys.Left, moveCursor (-1,0));
    (Keys.Right, moveCursor (1,0))]
    |> Map.ofList

let handleInput (keyboardStates:KeyboardState*KeyboardState) (gameState:GameState) : GameState =
    inputHandlers
    |> Map.filter (fun k _ -> keyboardStates |> Utility.wasKeyPressed k)
    |> Map.fold (fun acc _ v -> acc |> v) gameState

let handleTime (delta:GameTime) (gameState:GameState) : GameState =
    gameState

let textureFileNames = 
    [(DarkAllowed,"Content/dark-allowed.png");
    ( DarkEmpty,"Content/dark-empty.png");
    ( DarkOccupied,"Content/dark-occupied.png");
    ( DarkVisited,"Content/dark-visited.png");
    ( LightAllowed,"Content/light-allowed.png");
    ( LightEmpty,"Content/light-empty.png");
    ( LightOccupied,"Content/light-occupied.png");
    ( LightVisited,"Content/light-visited.png");
    ( Cursor,"Content/cursor.png")]

let drawGameState (textures:Map<TextureIdentifier,Texture2D>) (spriteBatch:SpriteBatch) (gameState:GameState) : unit =
    gameState.board
    |> drawBoard textures spriteBatch

    spriteBatch.Draw(textures.[Cursor],new Rectangle((gameState.cursorPosition |> fst)*cellWidth,(gameState.cursorPosition |> snd)*cellHeight,cellWidth,cellHeight), Color.White)

[<EntryPoint>]
let main argv = 
    use game = new CommonGame<TextureIdentifier,GameState>(backBufferSize,textureFileNames,makeBoard,drawGameState,handleInput,handleTime)
    game.Run()
    0
