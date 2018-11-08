open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open System.Numerics
open Microsoft.Xna.Framework.Input
open Microsoft.Xna.Framework
open System.IO
open System

let cellWidth = 128
let cellHeight = 128
let cellColumns = 4
let cellRows = 4
let holeIndex = cellColumns * cellRows - 1

let backBufferWidth = cellWidth * cellColumns
let backBufferHeight = cellRows * cellHeight

type Position = int * int

type GameState = Map<Position,int>

type MoveDirection = North | East | South | West

let (+) (first:Position) (second:Position) : Position =
    ((first |> fst) + (second |> fst), (first |> snd) + (second |> snd))


let directionDeltas =
    [(North, (0,-1));
    (East,  (1,0));
    (South, (0,1));
    (West,  (-1,0))]
    |> Map.ofList


let newGame () : GameState =
    [0..(cellColumns * cellRows-1)]
    |> List.map (fun x -> ((x % cellColumns, x/cellColumns),x))
    |> Map.ofList

let drawBoard (textures:Map<int,Texture2D>) (spriteBatch: SpriteBatch) (game:GameState) =
    game
    |> Map.iter
        (fun k v -> 
            let destination = new Rectangle((k |> fst)*cellWidth,(k |> snd)*cellHeight,cellWidth,cellHeight)
            spriteBatch.Draw(textures.[v], destination, Color.White))


let makeMove (direction: MoveDirection) (game:GameState) : GameState =
    let current =
        game
        |> Map.pick(fun k v -> if v = holeIndex then Some k else None)
    let next = 
        current + directionDeltas.[direction]
    if game |> Map.containsKey next then
        game
        |> Map.add current game.[next]
        |> Map.add next holeIndex
    else
        game

type Game1() as this=
    inherit Game()

    do
        this.Content.RootDirectory <- "Content"

    let graphics = new GraphicsDeviceManager(this)

    let mutable spriteBatch: SpriteBatch = null
    let mutable game = newGame()
    let mutable textures: Map<int,Texture2D> = Map.empty
    let mutable oldKeyboardState: KeyboardState = Keyboard.GetState()

    override this.Initialize() =
        graphics.PreferredBackBufferWidth <- backBufferWidth
        graphics.PreferredBackBufferHeight <- backBufferHeight
        graphics.ApplyChanges()
        spriteBatch <- new SpriteBatch(this.GraphicsDevice)
        textures <-
            [(0,"Content/01.png");
            ( 1,"Content/02.png");
            ( 2,"Content/03.png");
            ( 3,"Content/04.png");
            ( 4,"Content/05.png");
            ( 5,"Content/06.png");
            ( 6,"Content/07.png");
            ( 7,"Content/08.png");
            ( 8,"Content/09.png");
            ( 9,"Content/10.png");
            (10,"Content/11.png");
            (11,"Content/12.png");
            (12,"Content/13.png");
            (13,"Content/14.png");
            (14,"Content/15.png");
            (15,"Content/16.png")]
            |> List.fold 
                (fun acc (index,fileName) -> 
                    acc
                    |> Map.add index (Texture2D.FromStream(this.GraphicsDevice, new FileStream(fileName, FileMode.Open)))) textures

        base.Initialize()

    override this.LoadContent() =
        //this.Content.Load<
        ()

    override this.Update delta =
        let state = Keyboard.GetState()
        if(state.IsKeyDown(Keys.Escape)) then
            this.Exit()
        elif(state.IsKeyDown(Keys.Up) && oldKeyboardState.IsKeyDown(Keys.Up) |> not) then
            game <- game |> makeMove North
        elif(state.IsKeyDown(Keys.Down) && oldKeyboardState.IsKeyDown(Keys.Down) |> not) then
            game <- game |> makeMove South
        elif(state.IsKeyDown(Keys.Left) && oldKeyboardState.IsKeyDown(Keys.Left) |> not) then
            game <- game |> makeMove West
        elif(state.IsKeyDown(Keys.Right) && oldKeyboardState.IsKeyDown(Keys.Right) |> not) then
            game <- game |> makeMove East
        oldKeyboardState <- state

    override this.Draw delta =
        Color.BlanchedAlmond
        |> this.GraphicsDevice.Clear
        spriteBatch.Begin()

        game
        |> drawBoard textures spriteBatch

        spriteBatch.End()

[<EntryPoint>]
let main argv = 
    use game = new Game1()
    game.Run()
    0
