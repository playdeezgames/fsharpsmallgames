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
