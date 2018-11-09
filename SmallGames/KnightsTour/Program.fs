open System
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open System.IO

let cellWidth = 96
let cellHeight = 96
let cellColumns = 8
let cellRows = 8

let backBufferWidth = cellWidth * cellColumns
let backBufferHeight = cellRows * cellHeight

type SquareState = 
    | UNVISITED
    | OCCUPIED
    | VISITED

type Position = int * int

type Board = Map<Position, SquareState>

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

let makeBoard () : Board =
    let columnFold (board:Board) (column:int) : Board =
        (board, [0..(cellRows-1)])
        ||> List.fold 
            (fun board row -> 
                board |> Map.add (column,row) UNVISITED)

    (Map.empty, [0..(cellColumns-1)])
    ||> List.fold columnFold

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

let makeMove (move:Position) (board:Board) : Board =
    match board with
    | STARTING ->
        board
        |> Map.add move OCCUPIED
    | INPLAY when board |> Map.exists (fun k v -> k=move && v = UNVISITED)->
        let origin = 
            legalMoves
            |> List.map (addPosition move)
            |> List.tryPick
                (fun p -> 
                    board
                    |> Map.tryPick(fun k v -> if k = p && v = OCCUPIED then Some k else None))
        match origin with
        | Some p ->
            board
            |> Map.add p VISITED
            |> Map.add move OCCUPIED
        | None -> 
            board
    | _ -> 
        board

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

let moveCursor (delta:Position) (cursor:Position) : Position =
    match addPosition delta cursor with
    | (x,y) when x<0 || y<0 || x>=cellColumns || y>=cellRows -> cursor
    | x -> x

type Game1() as this=
    inherit Game()

    do
        this.Content.RootDirectory <- "Content"

    let graphics = new GraphicsDeviceManager(this)

    let mutable spriteBatch: SpriteBatch = null
    let mutable textures: Map<TextureIdentifier,Texture2D> = Map.empty
    let mutable oldKeyboardState: KeyboardState = Keyboard.GetState()
    let mutable board: Board = makeBoard()
    let mutable cursorPosition: Position = (0,0)

    override this.Initialize() =
        graphics.PreferredBackBufferWidth <- backBufferWidth
        graphics.PreferredBackBufferHeight <- backBufferHeight
        graphics.ApplyChanges()
        spriteBatch <- new SpriteBatch(this.GraphicsDevice)
        textures <-
            [(DarkAllowed,"Content/dark-allowed.png");
            ( DarkEmpty,"Content/dark-empty.png");
            ( DarkOccupied,"Content/dark-occupied.png");
            ( DarkVisited,"Content/dark-visited.png");
            ( LightAllowed,"Content/light-allowed.png");
            ( LightEmpty,"Content/light-empty.png");
            ( LightOccupied,"Content/light-occupied.png");
            ( LightVisited,"Content/light-visited.png");
            ( Cursor,"Content/cursor.png")]
            |> List.fold 
                (fun acc (index,fileName) -> 
                    acc
                    |> Map.add index (Texture2D.FromStream(this.GraphicsDevice, new FileStream(fileName, FileMode.Open)))) textures

        base.Initialize()

    override this.LoadContent() =
        ()

    override this.Update delta =
        let state = Keyboard.GetState()
        if(state.IsKeyDown(Keys.Escape)) then
            this.Exit()
        if isKeyPressed Keys.Space oldKeyboardState state then
            board <- board |> makeMove cursorPosition
        elif isKeyPressed Keys.Up oldKeyboardState state then
            cursorPosition <- cursorPosition |> moveCursor (0,-1)
        elif isKeyPressed Keys.Down oldKeyboardState state then
            cursorPosition <- cursorPosition |> moveCursor (0,1)
        elif isKeyPressed Keys.Left oldKeyboardState state then
            cursorPosition <- cursorPosition |> moveCursor (-1,0)
        elif isKeyPressed Keys.Right oldKeyboardState state then
            cursorPosition <- cursorPosition |> moveCursor (1,0)
        oldKeyboardState <- state

    override this.Draw delta =
        Color.BlanchedAlmond
        |> this.GraphicsDevice.Clear
        spriteBatch.Begin()

        board
        |> drawBoard textures spriteBatch

        spriteBatch.Draw(textures.[Cursor],new Rectangle((cursorPosition |> fst)*cellWidth,(cursorPosition |> snd)*cellHeight,cellWidth,cellHeight), Color.White)

        spriteBatch.End()

[<EntryPoint>]
let main argv = 
    use game = new Game1()
    game.Run()
    0
