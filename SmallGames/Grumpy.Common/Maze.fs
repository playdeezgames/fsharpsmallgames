namespace Grumpy.Common

type MazeCell<'tdirection when 'tdirection: comparison> = 
    Set<'tdirection>

type Maze<'tdirection when 'tdirection: comparison> =
    Map<Position, MazeCell<'tdirection>>

[<AutoOpen>]
module MazeActivePatterns =
    let (|IsDeadEnd|IsHall|) (cell:MazeCell<'tdirection>) =
        if cell.Count < 2 then
            IsDeadEnd
        else
            IsHall

module MazeCell =
    let create () : MazeCell<'tdirection> =
        Set.empty

    let setDirection (direction: 'tdirection) (mazeCell: MazeCell<'tdirection>) : MazeCell<'tdirection> =
        mazeCell
        |> Set.add direction
   

module Maze =
    open System

    let setMazeCell (position:Position) (mazeCell:MazeCell<'tdirection>) (maze:Maze<'tdirection>) : Maze<'tdirection> =
        maze
        |> Map.add position mazeCell

    let create (size:Position) : Maze<'tdirection> =
        [for column=0 to ((size |> fst) - 1) do
            for row=0 to ((size |> snd) - 1) do
                yield ((column,row),MazeCell.create())]
        |> Map.ofList

    let rec private generateNext (random:Random) (directions:Set<'tdirection>) (opposite:'tdirection->'tdirection) (walker:'tdirection->Position->Position) (frontier:Set<Position>) (outside: Set<Position>) (maze: Maze<'tdirection>) : Maze<'tdirection> =
        if frontier.IsEmpty then
            maze
        else
            let start =
                frontier
                |> Utility.generateFromSet random
            
            let direction =
                directions
                |> Set.filter
                    (fun direction ->
                        start 
                        |> walker direction
                        |> maze.ContainsKey)
                |> Utility.generateFromSet random

            let startCell =
                MazeCell.create()
                |> MazeCell.setDirection direction

            let finish =
                start
                |> walker direction
            
            let finishCell =
                maze.[finish]
                |> MazeCell.setDirection (direction |> opposite)
                
            let additionalFrontier =
                directions
                |> Set.map
                    (fun direction -> start |> walker direction)
                |> Set.filter outside.Contains

            let newOutside =
                outside
                |> Set.filter (fun item -> item |> additionalFrontier.Contains |> not)

            let newFrontier =
                frontier
                |> Set.remove start
                |> Set.union additionalFrontier
            
            maze
            |> setMazeCell start startCell
            |> setMazeCell finish finishCell
            |> generateNext random directions opposite walker newFrontier newOutside


    let generate (random:Random) (directions:Set<'tdirection>) (opposite:'tdirection->'tdirection) (walker:'tdirection->Position->Position) (size:Position) : Maze<'tdirection> =
        let positions =
            [for column=0 to ((size |> fst) - 1) do
                for row=0 to ((size |> snd) - 1) do
                    yield (column,row)]
            |> Set.ofList
        let start =
            positions
            |> Utility.generateFromSet random
        let frontier =
            directions
            |> Set.map
                (fun direction -> start |> walker direction)
            |> Set.filter positions.Contains
        let outside =
            positions
            |> Set.filter
                (fun position -> 
                    (position <> start) && (position |> frontier.Contains |> not))
        Map.empty
        |> setMazeCell start (MazeCell.create())
        |> generateNext random directions opposite walker frontier outside

