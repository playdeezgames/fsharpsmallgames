namespace Grumpy.DiamondMaze

type Direction =
    | North
    | East
    | South
    | West

module Direction =
    open Grumpy.Common

    let toPosition (direction:Direction) : Position = 
        match direction with
        | North -> ( 0, -1)
        | East  -> ( 1,  0)
        | South -> ( 0,  1)
        | West  -> (-1,  0)

    let list =
        [North; East; South; West]

    let set =
        list |> Set.ofList

    let opposite (direction:Direction) : Direction =
        match direction with
        | North -> South
        | South -> North
        | East  -> West
        | West  -> East

    let walk (direction:Direction) (position: Position) : Position =
        direction
        |> toPosition
        |> Position.add position
