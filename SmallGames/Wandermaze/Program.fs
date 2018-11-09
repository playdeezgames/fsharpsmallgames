open System
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open System.IO
open System.Globalization
open Utility

let boardCellWidth = 16
let boardCellHeight = 16
let boardCellColumns = 32
let boardCellRows = 32

let boardOffsetX = 0
let boardOffsetY = 0
let boardWidth = boardCellWidth * boardCellColumns
let boardHeight = boardCellRows * boardCellHeight

let panelCellWidth = 32
let panelCellHeight = 32
let panelCellColumns = 8
let panelCellRows = 16

let panelOffsetX = boardOffsetX + boardWidth
let panelOffsetY = boardOffsetY
let panelWidth = panelCellColumns * panelCellWidth
let panelHeight = panelCellRows * panelCellHeight

let backBufferWidth = boardWidth + panelWidth
let backBufferHeight = boardHeight

type CellState = | Fire | Water | Earth | Gold | Player
type Board = Map<Position, CellState>
type GameState = {board:Board;level:int;score:int;protection:int}

let isKeyPressed (key:Keys) (oldKeyboardState:KeyboardState) (newKeyboardState:KeyboardState) : bool = 
    oldKeyboardState.IsKeyUp(key) && newKeyboardState.IsKeyDown(key)

type TextureIdentifier =
    | Empty
    | CrownCoin
    | Flame
    | Meeple
    | StoneWall
    | WaterDrop

let setBoardCell (position:Position) (state:CellState) (gameState:GameState) : GameState =
    {gameState with board = gameState.board |> Map.add position state}

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
    let playerPosition = 
        gameState.board
        |> Map.findKey (fun k v -> v = Player)
    let nextPosition = playerPosition |> Position.add (directionMap.[direction])
    gameState
    |> setBoardCell playerPosition Fire
    |> setBoardCell nextPosition Player

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

    override this.Draw delta =
        Color.BlanchedAlmond
        |> this.GraphicsDevice.Clear
        spriteBatch.Begin()

        gameState.board
        |> drawBoard textures spriteBatch

        //spriteBatch.Draw(textures.[Cursor],new Rectangle((cursorPosition |> fst)*cellWidth,(cursorPosition |> snd)*cellHeight,cellWidth,cellHeight), Color.White)

        spriteBatch.End()

[<EntryPoint>]
let main argv = 
    use game = new Wandermaze()
    game.Run()
    0
