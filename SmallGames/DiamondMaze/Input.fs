namespace Grumpy.DiamondMaze

module Input =
    open Microsoft.Xna.Framework.Input
    open Microsoft.Xna.Framework
    open Grumpy.Common
    open System

    let inputHandlers: Map<Keys,GameState->GameState> =
        [(Keys.F2, GameState.restart);
        (Keys.Up, GameState.moveAvatar North);
        (Keys.Down, GameState.moveAvatar South);
        (Keys.Left, GameState.moveAvatar West);
        (Keys.Right, GameState.moveAvatar East)]
        |> Map.ofList

    let keyCooldown = new TimeSpan(0,0,0,0,100)

    let handleInput (keyboardStates:KeyboardState*KeyboardState) (gameStateContext:GameState * Context) : GameState * Context =
        inputHandlers
        |> Map.filter (fun k _ -> keyboardStates |> Utility.wasKeyChanged k)
        |> Map.fold (fun (gameState,context) k v -> 
            if keyboardStates |> Utility.wasKeyPressed k then
                (gameState |> v,context |> Context.setCounter k TimeSpan.Zero)
            else
                (gameState,context |> Context.removeCounter k)) gameStateContext
    

    let handleTime (delta:GameTime) (gameState:GameState, context:Context) : GameState * Context =
        let deltaTs = new TimeSpan(0,0,0,0,delta.ElapsedGameTime.TotalMilliseconds |> int)
        context.counters
        |> Map.fold
            (fun (gs, ctx) k v -> 
                let nextV =
                    v + deltaTs
                if nextV >= keyCooldown then
                    (gs |> inputHandlers.[k], ctx |> Context.setCounter k (nextV - keyCooldown))
                else
                    (gs, ctx |> Context.setCounter k nextV)) (gameState, context)

