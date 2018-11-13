namespace Grumpy.DiamondMaze

module Input =
    open Microsoft.Xna.Framework.Input
    open Microsoft.Xna.Framework
    open Grumpy.Common

    let inputHandlers: Map<Keys,GameState->GameState> =
        [(Keys.F2, GameState.restart);
        (Keys.Up, GameState.moveAvatar North);
        (Keys.Down, GameState.moveAvatar South);
        (Keys.Left, GameState.moveAvatar West);
        (Keys.Right, GameState.moveAvatar East)]
        |> Map.ofList


    let handleInput (keyboardStates:KeyboardState*KeyboardState) (gameState:GameState) : GameState =
        inputHandlers
        |> Map.filter (fun k _ -> keyboardStates |> Utility.wasKeyPressed k)
        |> Map.fold (fun acc _ v -> acc |> v) gameState

    let handleTime (delta:GameTime) (gameState:GameState) : GameState =
        gameState

