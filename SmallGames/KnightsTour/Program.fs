open System

type SquareState = 
    | UNVISITED
    | OCCUPIED
    | VISITED

type Position = int * int

type Board = Map<Position, SquareState>

let cellTexts =
    [("a1",(1,1));
    ("a2",(1,2));
    ("a3",(1,3));
    ("a4",(1,4));
    ("a5",(1,5));
    ("a6",(1,6));
    ("a7",(1,7));
    ("a8",(1,8));
    ("b1",(2,1));
    ("b2",(2,2));
    ("b3",(2,3));
    ("b4",(2,4));
    ("b5",(2,5));
    ("b6",(2,6));
    ("b7",(2,7));
    ("b8",(2,8));
    ("c1",(2,1));
    ("c2",(2,2));
    ("c3",(2,3));
    ("c4",(2,4));
    ("c5",(2,5));
    ("c6",(2,6));
    ("c7",(2,7));
    ("c8",(2,8));
    ("d1",(2,1));
    ("d2",(2,2));
    ("d3",(2,3));
    ("d4",(2,4));
    ("d5",(2,5));
    ("d6",(2,6));
    ("d7",(2,7));
    ("d8",(2,8));
    ("e1",(2,1));
    ("e2",(2,2));
    ("e3",(2,3));
    ("e4",(2,4));
    ("e5",(2,5));
    ("e6",(2,6));
    ("e7",(2,7));
    ("e8",(2,8));
    ("f1",(2,1));
    ("f2",(2,2));
    ("f3",(2,3));
    ("f4",(2,4));
    ("f5",(2,5));
    ("f6",(2,6));
    ("f7",(2,7));
    ("f8",(2,8));
    ("g1",(2,1));
    ("g2",(2,2));
    ("g3",(2,3));
    ("g4",(2,4));
    ("g5",(2,5));
    ("g6",(2,6));
    ("g7",(2,7));
    ("g8",(2,8));
    ("h1",(2,1));
    ("h2",(2,2));
    ("h3",(2,3));
    ("h4",(2,4));
    ("h5",(2,5));
    ("h6",(2,6));
    ("h7",(2,7));
    ("h8",(2,8))]
    |> Map.ofList

let BoardWidth = 8
let BoardHeight = 8

let makeBoard () : Board =
    let columnFold (board:Board) (column:int) : Board =
        (board, [1..BoardHeight])
        ||> List.fold 
            (fun board row -> 
                board |> Map.add (column,row) UNVISITED)

    (Map.empty, [1..BoardWidth])
    ||> List.fold columnFold

let board = makeBoard()

let drawBoard (board:Board) : unit =
    Console.Clear()
    Console.Write("  A B C D E F G H")

    [1..8] 
    |> List.iter
        (fun row ->
            Console.CursorLeft<-0
            Console.CursorTop<-row
            Console.Write(row))

    board
    |> Map.iter 
        (fun (column,row) state ->
            Console.CursorLeft <- column * 2
            Console.CursorTop <- row
            let character = 
                match state with
                | UNVISITED -> '.'
                | OCCUPIED -> 'K'
                | VISITED -> '#'
            Console.Write(character))
    ()

let makeMove (board:Board) (move:Position) : Board =
    board

let rec playGame (board:Board) : unit =
    board
    |> drawBoard

    cellTexts    
    |> Map.tryPick (fun k v -> if k = Console.ReadLine().ToLower() then Some v else None)
    |> Option.map (makeMove board)
    |> Option.iter playGame

    board
    |> playGame

[<EntryPoint>]
let main argv =
    Console.Title <- "Knight's Tour"

    board
    |> playGame

    Console.ReadLine() 
    |> ignore
    0
