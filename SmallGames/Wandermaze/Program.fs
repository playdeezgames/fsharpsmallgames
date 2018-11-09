open System
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open System.IO
open System.Globalization
open Utility
open System.Text

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

type CellState = | Fire | Water | Earth | Gold | Player
type Board = Map<Position, CellState>
type GameState = {board:Board;level:int;score:int;protection:int}

let hasPlayer (gameState:GameState) : bool =
    gameState.board
    |> Map.exists (fun _ v -> v = Player)

let getCellState (position:Position) (gameState:GameState) : CellState option =
    gameState.board |> Map.tryFind position

let (|GAMEOVER|INPLAY|) (gameState:GameState) =
    match gameState with
    | x when x |> hasPlayer -> INPLAY
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

let addScore (score:int) (gameState:GameState) : GameState =
    {gameState with score = gameState.score + score}

let addProtection (protection:int) (gameState:GameState) : GameState =
    {gameState with protection = gameState.protection + protection}

let removeProtection (protection:int) = addProtection (-protection)

let boardPositions = 
    [for x = 0 to (boardCellColumns-1) do
        for y=0 to (boardCellRows-1) do 
            yield (x,y)]

//TODO: allow for having the player already be placed
let populateBoard (random:Random) (gameState:GameState) : GameState =
    let playerPositions, remaining =
        boardPositions
        |> List.sortBy (fun _ -> random.Next())
        |> List.splitAt 1
    let goldPositions, remaining =
        remaining
        |> List.splitAt 64
    let firePositions, remaining =
        remaining
        |> List.splitAt 128
    let earthPositions, remaining =
        remaining
        |> List.splitAt 16
    let waterPositions, remaining =
        remaining
        |> List.splitAt 8
    (((((gameState,playerPositions)
    ||> List.fold (fun acc v -> acc |> setBoardCell v Player), goldPositions)
    ||> List.fold (fun acc v -> acc |> setBoardCell v Gold), firePositions)
    ||> List.fold (fun acc v -> acc |> setBoardCell v Fire), earthPositions)
    ||> List.fold (fun acc v -> acc |> setBoardCell v Earth), waterPositions)
    ||> List.fold (fun acc v -> acc |> setBoardCell v Water)


let makeGameState (random:Random) : GameState =
    {board=Map.empty;
    level=1;
    score=0;
    protection=0}
    |> populateBoard random

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
                gameState
                |> setBoardCell playerPosition Fire
            | Some Earth ->
                gameState
            | Some Gold ->
                gameState
                |> setBoardCell playerPosition Fire
                |> setBoardCell nextPosition Player
                |> addScore 1
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

type Wandermaze() as this=
    inherit Game()

    do
        this.Content.RootDirectory <- "Content"

    let graphics = new GraphicsDeviceManager(this)

    let mutable spriteBatch: SpriteBatch = null
    let mutable textures: Map<TextureIdentifier,Texture2D> = Map.empty
    let mutable oldKeyboardState: KeyboardState = Keyboard.GetState()
    let random:Random = new Random()
    let mutable gameState: GameState = makeGameState(random)

    override this.Initialize() =
        graphics.PreferredBackBufferWidth <- backBufferWidth
        graphics.PreferredBackBufferHeight <- backBufferHeight
        graphics.ApplyChanges()
        spriteBatch <- new SpriteBatch(this.GraphicsDevice)
        textures <-
            [(CrownCoin,"Content/crown-coin.png");
            ( Empty,"Content/empty.png");
            ( Flame,"Content/flame.png");
            ( Meeple,"Content/meeple.png");
            ( StoneWall,"Content/stone-wall.png");
            ( RomFont,"Content/romfont8x8.png");
            ( WaterDrop,"Content/water-drop.png")]
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
        if isKeyPressed Keys.Up oldKeyboardState state then
            gameState <- gameState |> makeMove North
        elif isKeyPressed Keys.Down oldKeyboardState state then
            gameState <- gameState |> makeMove South
        elif isKeyPressed Keys.Left oldKeyboardState state then
            gameState <- gameState |> makeMove West
        elif isKeyPressed Keys.Right oldKeyboardState state then
            gameState <- gameState |> makeMove East
        oldKeyboardState <- state

    member this.WriteText (position:Position,outputSize:Position,text:string,color:Color) :unit =
        let texture = textures.[RomFont]
        let inputSize = (texture.Width/16, texture.Height/16)
        (position, text |> Encoding.ASCII.GetBytes)
        ||> Array.fold 
            (fun p c -> 
                spriteBatch.Draw(texture,new Rectangle(p|>fst,p|>snd,outputSize|>fst,outputSize|>snd), new Rectangle((int(c % 16uy)) * (inputSize|>fst),(int(c / 16uy)) * (inputSize|>snd),inputSize|>fst,inputSize|>snd) |> Some |> Option.toNullable, color)
                ((p |> fst) + (outputSize |> fst), p|>snd)) 
        |>ignore


    override this.Draw delta =
        Color.Black
        |> this.GraphicsDevice.Clear
        spriteBatch.Begin()

        gameState.board
        |> drawBoard textures spriteBatch

        this.WriteText((panelOffsetX,panelOffsetY),(panelCellWidth,panelCellHeight),gameState.score |> sprintf "Score: %d",Color.Yellow)

        spriteBatch.End()

[<EntryPoint>]
let main argv = 
    use game = new Wandermaze()
    game.Run()
    0
