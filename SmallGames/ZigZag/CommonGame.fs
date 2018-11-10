namespace Grumpy.Common

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open System
open System.IO

type CommonGame<'TextureIdentifier, 'GameState when 'TextureIdentifier: comparison>(backBufferSize:int*int, textureFileNames: ('TextureIdentifier * string) list, createGameState: unit->'GameState, drawBoard: Map<'TextureIdentifier,Texture2D>->SpriteBatch->'GameState->unit, handleInput:(KeyboardState * KeyboardState)->'GameState->'GameState, handleDelta:GameTime->'GameState->'GameState) as this=
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
        gameState <- gameState |> handleDelta delta |> handleInput (oldKeyboardState, state)
        oldKeyboardState <- state

    override this.Draw delta =
        Color.Black
        |> this.GraphicsDevice.Clear
        spriteBatch.Begin()

        gameState
        |> drawBoard textures spriteBatch

        spriteBatch.End()
