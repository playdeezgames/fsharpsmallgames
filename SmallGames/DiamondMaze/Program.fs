open Grumpy.Common
open Utility
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open System
open Microsoft.Xna.Framework
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


type GameState =
    {random:Random}

let createGameState () : GameState =
    {random = new Random()}

type TextureIdentifier =
    | RomFont

let cellColumns = 40
let cellRows = 30
let cellWidth = 16
let cellHeight = 16

//field frame (0,0,29,29)
let fieldFrameOffsetX = 0
let fieldFrameOffsetY = 0
let fieldFrameColumns = 29
let fieldFrameRows = 29
let fieldFrameWidth = fieldFrameColumns * cellWidth
let fieldFrameHeight = fieldFrameRows * cellHeight

//panel frame (29,0,1,29)
let panelFrameOffsetX = 29
let panelFrameOffsetY = 0
let panelFrameColumns = 11
let panelFrameRows = 29
let panelFrameWidth = panelFrameColumns * cellWidth
let panelFrameHeight = panelFrameRows * cellHeight

//field (1,1,27,27)
//status bar (0,29,40,1)
//minmap (30,1,9,9)

type RenderCell =
    {character:byte;
    foreground:Color;
    background:Color}

let makeRenderCell (background:Color) (foreground:Color) (character:byte) : RenderCell =
    {character=character;
    foreground=foreground;
    background=background}

let makeFrameRenderCell =
    makeRenderCell Color.Gray Color.Blue

let vline (cell:RenderCell) (position:Position) (height:int) (board:Map<Position,RenderCell>) : Map<Position,RenderCell> =
    height
    |> Seq.unfold 
        (fun s -> 
            if s=0 then
                None
            else
                ((0,s-1),s-1)|>Some)
    |> Seq.map
        (fun p -> 
            (p |> Position.add position, cell))
    |> Map.ofSeq
    |> Map.fold
        (fun acc k v -> acc |> Map.add k v) board

let hline (cell:RenderCell) (position:Position) (width:int) (board:Map<Position,RenderCell>) : Map<Position,RenderCell> =
    width
    |> Seq.unfold 
        (fun s -> 
            if s=0 then
                None
            else
                ((s - 1,0),s - 1)|>Some)
    |> Seq.map
        (fun p -> 
            (p |> Position.add position, cell))
    |> Map.ofSeq
    |> Map.fold
        (fun acc k v -> acc |> Map.add k v) board

let fieldFrameCells =
    Map.empty
    |> Map.add (fieldFrameOffsetX                        , fieldFrameOffsetY)                      (makeFrameRenderCell 0xc9uy)
    |> Map.add (fieldFrameOffsetX + fieldFrameColumns - 1, fieldFrameOffsetY)                      (makeFrameRenderCell 0xbbuy)
    |> Map.add (fieldFrameOffsetX                        , fieldFrameOffsetY + fieldFrameRows - 1) (makeFrameRenderCell 0xc8uy)
    |> Map.add (fieldFrameOffsetX + fieldFrameColumns - 1, fieldFrameOffsetY + fieldFrameRows - 1) (makeFrameRenderCell 0xbcuy)
    |> hline   (makeFrameRenderCell 0xcduy) (fieldFrameOffsetX + 1                  , fieldFrameOffsetY)                    (fieldFrameColumns - 2)
    |> hline   (makeFrameRenderCell 0xcduy) (fieldFrameOffsetX + 1                  , fieldFrameOffsetY + fieldFrameRows-1) (fieldFrameColumns - 2)
    |> vline   (makeFrameRenderCell 0xbauy) (fieldFrameOffsetX                      , fieldFrameOffsetY + 1)                (fieldFrameRows    - 2)
    |> vline   (makeFrameRenderCell 0xbauy) (fieldFrameOffsetX + fieldFrameColumns-1, fieldFrameOffsetY + 1)                (fieldFrameRows    - 2)
    |> Map.add (panelFrameOffsetX                        , panelFrameOffsetY)                      (makeFrameRenderCell 0xc9uy)
    |> Map.add (panelFrameOffsetX + panelFrameColumns - 1, panelFrameOffsetY)                      (makeFrameRenderCell 0xbbuy)
    |> Map.add (panelFrameOffsetX                        , panelFrameOffsetY + panelFrameRows - 1) (makeFrameRenderCell 0xc8uy)
    |> Map.add (panelFrameOffsetX + panelFrameColumns - 1, panelFrameOffsetY + panelFrameRows - 1) (makeFrameRenderCell 0xbcuy)
    |> hline   (makeFrameRenderCell 0xcduy) (panelFrameOffsetX + 1                  , panelFrameOffsetY)                    (panelFrameColumns - 2)
    |> hline   (makeFrameRenderCell 0xcduy) (panelFrameOffsetX + 1                  , panelFrameOffsetY + panelFrameRows-1) (panelFrameColumns - 2)
    |> vline   (makeFrameRenderCell 0xbauy) (panelFrameOffsetX                      , panelFrameOffsetY + 1)                (panelFrameRows    - 2)
    |> vline   (makeFrameRenderCell 0xbauy) (panelFrameOffsetX + panelFrameColumns-1, panelFrameOffsetY + 1)                (panelFrameRows    - 2)

let backBufferSize = (cellColumns * cellWidth,cellRows * cellHeight)

let textureFileNames = 
    [(RomFont, "Content/romfont8x8.png")]

let outputRect (position:Position) : Rectangle =
    new Rectangle((position |> fst) * cellWidth,(position |> snd) * cellHeight,cellWidth,cellHeight)

let inputRect (texture:Texture2D) (position:Position) : Rectangle =
    new Rectangle((position |> fst) * texture.Width/16,(position |> snd) * texture.Height/16,texture.Width/16,texture.Height/16)

let renderCell (texture:Texture2D) (position:Position) (cell:RenderCell) (spriteBatch:SpriteBatch) : unit =
    //draw background
    spriteBatch.Draw(texture, position |> outputRect, (0xb,0xd) |> inputRect texture |> Some |> Option.toNullable, cell.background)
    //draw foreground
    spriteBatch.Draw(texture, position |> outputRect, ((cell.character |> int) % 16,(cell.character |> int) / 16) |> inputRect texture |> Some |> Option.toNullable, cell.foreground)

let drawGameState (textures:Map<TextureIdentifier,Texture2D>) (spriteBatch:SpriteBatch) (gameState:GameState) : unit =
    //draw the frame
    fieldFrameCells
    |> Map.iter
        (fun k v ->
            renderCell textures.[RomFont] k v spriteBatch)

let restartGame (_) : GameState =
    createGameState()

let inputHandlers: Map<Keys,GameState->GameState> =
    [(Keys.F2, restartGame)]
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
