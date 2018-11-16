namespace Grumpy.Common

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open System.IO

type CommonGame<'TextureIdentifier, 'GameState, 'Context when 'TextureIdentifier: comparison>
        (backBufferSize:int*int, 
            textureFileNames: ('TextureIdentifier * string) list, 
            createGameState: unit -> 'GameState, 
            createContext: unit -> 'Context,
            drawBoard: Map<'TextureIdentifier,Texture2D>->SpriteBatch->'GameState->unit, 
            handleInput:(KeyboardState * KeyboardState)->('GameState * 'Context)->('GameState * 'Context), 
            handleDelta:GameTime->('GameState * 'Context)->('GameState * 'Context)) as this=
    inherit Game()

    do
        this.Content.RootDirectory <- "Content"

    let graphics = new GraphicsDeviceManager(this)

    let drawBoard = drawBoard
    let handleInput = handleInput
    let handleDelta = handleDelta
    let mutable spriteBatch: SpriteBatch = null
    let mutable textures: Map<'TextureIdentifier,Texture2D> = Map.empty
    let mutable oldKeyboardState: KeyboardState = Keyboard.GetState()
    let mutable gameState: 'GameState = createGameState()
    let mutable context: 'Context = createContext ()

    override this.Initialize() =
        graphics.PreferredBackBufferWidth <- backBufferSize |> fst
        graphics.PreferredBackBufferHeight <- backBufferSize |> snd
        graphics.ApplyChanges()
        spriteBatch <- new SpriteBatch(this.GraphicsDevice)
        textures <-
            textureFileNames
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
        let nextGameState, nextContext =
            (gameState,context) |> handleDelta delta |> handleInput (oldKeyboardState, state)
        gameState <- nextGameState
        context <- nextContext
        oldKeyboardState <- state

    override this.Draw delta =
        Color.Black
        |> this.GraphicsDevice.Clear
        spriteBatch.Begin()

        gameState
        |> drawBoard textures spriteBatch

        spriteBatch.End()
