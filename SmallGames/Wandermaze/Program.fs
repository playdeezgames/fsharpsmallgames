open System
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open System.IO
open System.Text
open Grumpy.Common

let boardCellWidth = 16
let boardCellHeight = 16
let boardCellColumns = 32
let boardCellRows = 32

let boardOffsetX = 0
let boardOffsetY = 0
let boardWidth = boardCellWidth * boardCellColumns
let boardHeight = boardCellRows * boardCellHeight

let panelCellWidth = 16
let panelCellHeight = 16
let panelCellColumns = 16
let panelCellRows = 32

let panelOffsetX = boardOffsetX + boardWidth
let panelOffsetY = boardOffsetY
let panelWidth = panelCellColumns * panelCellWidth
let panelHeight = panelCellRows * panelCellHeight

let backBufferWidth = boardWidth + panelWidth
let backBufferHeight = boardHeight
let backBufferSize = (backBufferWidth, backBufferHeight)

type CellState = | Fire | Water | Earth | Gold | Player
type Board = Map<Position, CellState>
type GameState = {random: Random;board:Board;level:int;score:int;protection:int}

let isLevelComplete (gameState:GameState) : bool =
    gameState.board
    |> Map.exists (fun _ v -> v = Gold)
    |> not

let hasPlayer (gameState:GameState) : bool =
    gameState.board
    |> Map.exists (fun _ v -> v = Player)

let getPlayerPosition  (gameState:GameState) : Position option =
    gameState.board
    |> Map.tryFindKey (fun k v -> v = Player)

let getCellState (position:Position) (gameState:GameState) : CellState option =
    gameState.board |> Map.tryFind position

let (|GAMEOVER|INPLAY|LEVELCOMPLETE|) (gameState:GameState) =
    match gameState |> hasPlayer, gameState |> isLevelComplete with
    | true, false -> INPLAY
    | true, true -> LEVELCOMPLETE
    | _ -> GAMEOVER

let isKeyPressed (key:Keys) (oldKeyboardState:KeyboardState) (newKeyboardState:KeyboardState) : bool = 
    oldKeyboardState.IsKeyUp(key) && newKeyboardState.IsKeyDown(key)

type TextureIdentifier =
    | Empty
    | CrownCoin
    | Flame
    | Meeple
    | StoneWall
    | WaterDrop
    | RomFont

let setBoardCell (position:Position) (state:CellState) (gameState:GameState) : GameState =
    {gameState with board = gameState.board |> Map.add position state}

let clearBoard (gameState:GameState) : GameState =
    {gameState with board = Map.empty}

let addScore (score:int) (gameState:GameState) : GameState =
    {gameState with score = gameState.score + score}

let addProtection (protection:int) (gameState:GameState) : GameState =
    {gameState with protection = gameState.protection + protection}

let removeProtection (protection:int) = addProtection (-protection)

let boardPositions = 
    [for x = 0 to (boardCellColumns-1) do
        for y=0 to (boardCellRows-1) do 
            yield (x,y)]


let populateBoard (playerPosition:Position) (gameState:GameState) : GameState =
    let remaining =
        boardPositions
        |> List.filter (fun x-> x<>playerPosition)
        |> List.sortBy (fun _ -> gameState.random.Next())
    let goldPositions, remaining =
        remaining
        |> List.splitAt (64)
    let firePositions, remaining =
        remaining
        |> List.splitAt (128 + gameState.level * 32)
    let earthPositions, remaining =
        remaining
        |> List.splitAt (32 + 8 * gameState.level)
    let waterPositions, remaining =
        remaining
        |> List.splitAt (8 - gameState.level/4)
    (((((gameState,[playerPosition])
    ||> List.fold (fun acc v -> acc |> setBoardCell v Player), goldPositions)
    ||> List.fold (fun acc v -> acc |> setBoardCell v Gold), firePositions)
    ||> List.fold (fun acc v -> acc |> setBoardCell v Fire), earthPositions)
    ||> List.fold (fun acc v -> acc |> setBoardCell v Earth), waterPositions)
    ||> List.fold (fun acc v -> acc |> setBoardCell v Water)

let nextLevel (gameState:GameState) : GameState =
    let playerPosition = gameState |> getPlayerPosition |> Option.get
    {gameState with level = gameState.level+1}
    |> clearBoard
    |> populateBoard playerPosition

let createGameState () : GameState =
    let random = new Random()
    {random = random;
    board=Map.empty;
    level=0;
    score=0;
    protection=0}
    |> populateBoard (random.Next(boardCellColumns), random.Next(boardCellRows))

let cellStateTextures =
    [(None, Empty);
    (Some Fire, Flame);
    (Some Water, WaterDrop);
    (Some Earth, StoneWall);
    (Some Gold, CrownCoin);
    (Some Player, Meeple);
    ]|>Map.ofList

let drawBoard (textures:Map<TextureIdentifier, Texture2D>) (spriteBatch:SpriteBatch) (board:Board) : unit =
    boardPositions
    |> List.iter    
        (fun p -> 
            let cellState = 
                board
                |> Map.tryPick 
                    (fun k v -> if k=p then Some v else None)
            let column,row = p
            spriteBatch.Draw(textures.[cellStateTextures.[cellState]],new Rectangle(boardOffsetX + column * boardCellWidth, boardOffsetY + row * boardCellHeight, boardCellWidth, boardCellHeight),Color.White))

type Direction = 
    | North
    | East
    | South
    | West

let directionMap =
    [(North,(0,-1));(East,(1,0));(South,(0,1));(West,(-1,0))] |> Map.ofList

let makeMove (direction:Direction) (gameState: GameState) : GameState =
    match gameState with 
    | INPLAY ->
        let playerPosition = 
            gameState.board
            |> Map.findKey (fun _ v -> v = Player)
        let nextPosition = playerPosition |> Position.add (directionMap.[direction])
        if boardPositions |> List.tryFind (fun v -> v = nextPosition) |> Option.isSome then
            match gameState |> getCellState nextPosition with
            | Some Fire ->
                if gameState.protection>0 then
                    gameState
                    |> setBoardCell playerPosition Fire
                    |> setBoardCell nextPosition Player
                    |> removeProtection 1
                else
                    gameState
                    |> setBoardCell playerPosition Fire
            | Some Earth ->
                gameState
            | Some Gold ->
                let newState = 
                    gameState
                    |> setBoardCell playerPosition Fire
                    |> setBoardCell nextPosition Player
                    |> addScore 1
                match newState with
                | LEVELCOMPLETE -> 
                    newState
                    |> nextLevel
                | _ -> 
                    newState
            | Some Water ->
                gameState
                |> setBoardCell playerPosition Fire
                |> setBoardCell nextPosition Player
                |> addProtection 1
            | _ ->
                gameState
                |> setBoardCell playerPosition Fire
                |> setBoardCell nextPosition Player
        else
            gameState
    | _ ->
        gameState

let textureFileNames = 
    [(CrownCoin,"Content/crown-coin.png");
    ( Empty,"Content/empty.png");
    ( Flame,"Content/flame.png");
    ( Meeple,"Content/meeple.png");
    ( StoneWall,"Content/stone-wall.png");
    ( RomFont,"Content/romfont8x8.png");
    ( WaterDrop,"Content/water-drop.png")]

let drawGameState (textures:Map<TextureIdentifier,Texture2D>) (spriteBatch:SpriteBatch) (gameState:GameState) : unit =
    gameState.board
    |> drawBoard textures spriteBatch

    Utility.writeText textures.[RomFont] spriteBatch (panelCellWidth,panelCellHeight) (panelOffsetX,panelOffsetY)                   (gameState.score      |> sprintf "Score: %d")      Color.Yellow
    Utility.writeText textures.[RomFont] spriteBatch (panelCellWidth,panelCellHeight) (panelOffsetX,panelOffsetY+panelCellHeight)   (gameState.protection |> sprintf "Protection: %d") Color.Blue
    Utility.writeText textures.[RomFont] spriteBatch  (panelCellWidth,panelCellHeight)(panelOffsetX,panelOffsetY+panelCellHeight*2) ((gameState.level+1)  |> sprintf "Level: %d")      Color.Gray

let restartGame (_) : GameState =
    createGameState()

let inputHandlers: Map<Keys,GameState->GameState> =
    [(Keys.F2, restartGame);
    (Keys.Up, makeMove North);
    (Keys.Down, makeMove South);
    (Keys.Left, makeMove West);
    (Keys.Right, makeMove East)]
    |> Map.ofList

let handleInput (keyboardStates:KeyboardState*KeyboardState) (gameState:GameState) : GameState =
    inputHandlers
    |> Map.filter (fun k _ -> keyboardStates |> Utility.wasKeyPressed k)
    |> Map.fold (fun acc _ v -> acc |> v) gameState

let handleTime (delta:GameTime) (gameState:GameState) : GameState =
    gameState


[<EntryPoint>]
let main argv = 
    use game = new CommonGame<TextureIdentifier,GameState>(backBufferSize,textureFileNames,createGameState,drawGameState,handleInput,handleTime)
    game.Run()
    0
