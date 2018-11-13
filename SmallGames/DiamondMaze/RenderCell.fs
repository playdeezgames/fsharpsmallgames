namespace Grumpy.DiamondMaze

open Microsoft.Xna.Framework

type RenderCell =
    {character:byte;
    foreground:Color;
    background:Color}

module RenderCell =
    open Grumpy.Common

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


