let exampleLevel = ["    #####";
                    "    #   #";
                    "    #$  #";
                    "  ###  $##";
                    "  #  $ $ #";
                    "### # ## #   ######";
                    "#   # ## #####  ..#";
                    "# $  $          ..#";
                    "##### ### #@##  ..#";
                    "    #     #########";
                    "    #######"]

type position = { x: int; y:int}

type cellState = Wall | Player |  PlayerGoal | Box | BoxGoal | Goal | Floor

type cellStateDescriptor = { CellState: cellState;
                             Character: System.Char;
                             Foreground: System.ConsoleColor;
                             Background: System.ConsoleColor}

let cellStateDescriptors = [{CellState=Wall; Character='#'; Foreground=System.ConsoleColor.DarkBlue; Background=System.ConsoleColor.Blue};
                            {CellState=Player; Character='@'; Foreground=System.ConsoleColor.Green; Background=System.ConsoleColor.DarkGreen};
                            {CellState=PlayerGoal; Character='+'; Foreground=System.ConsoleColor.Red; Background=System.ConsoleColor.DarkGreen};
                            {CellState=Box; Character='$'; Foreground=System.ConsoleColor.Yellow; Background=System.ConsoleColor.DarkYellow};
                            {CellState=BoxGoal; Character='*'; Foreground=System.ConsoleColor.Red; Background=System.ConsoleColor.DarkYellow};
                            {CellState=Goal; Character='.'; Foreground=System.ConsoleColor.Red; Background=System.ConsoleColor.Black};
                            {CellState=Floor; Character=' '; Foreground=System.ConsoleColor.Black; Background=System.ConsoleColor.Black}]

let cellStateToCharacter cellState=
    let descriptor = cellStateDescriptors
                     |>List.tryFind(fun elem->elem.CellState=cellState)
    if descriptor.IsSome then 
        descriptor.Value.Character
    else 
        '#'//assume invalid cell states are walls

let characterToCellState character=
    let descriptor = cellStateDescriptors
                     |>List.tryFind(fun elem->elem.Character=character)
    if descriptor.IsSome then 
        descriptor.Value.CellState
    else 
        Wall//assume invalid characters are walls

type cell = {CellState:cellState; Position:position}

let rec loadRow (rowData: cellState list) (row:int) (column:int)=
    match rowData with
    | []->[]
    | head::tail -> {CellState=head; Position={x=column;y=row}} :: loadRow (tail) (row) (column+1)

let processRow (rowData: string) (row:int)=
    let processedRow = rowData|>Seq.map(fun elem->characterToCellState elem)|>Seq.toList
    loadRow processedRow row 0

let rec loadLevelRow (level: string list) (row:int)=
    match level with
    | [] -> []
    | head::tail -> 
        let processedHead = processRow head row
        let processedTail = loadLevelRow tail (row+1)
        processedHead @ processedTail

let loadLevel (level: string list)=
    loadLevelRow level 0

let displayLevel (board: cell list)=
    board|>List.iter(fun elem->
        let descriptor = cellStateDescriptors
                         |>List.find(fun desc->desc.CellState=elem.CellState)
        do System.Console.SetCursorPosition(elem.Position.x,elem.Position.y)
        do System.Console.ForegroundColor <- descriptor.Foreground
        do System.Console.BackgroundColor <- descriptor.Background
        do System.Console.Write descriptor.Character
     )   

let isSolved (board: cell list)=
    let aBox = board
               |>List.tryFind(fun elem->elem.CellState=Box)
    aBox.IsNone

type direction = Up | Down | Left | Right

type directionDescriptor = {Direction: direction; Delta:position; Key:System.ConsoleKey}

let directionDescriptors = [ {Direction=Up;    Delta={x=0; y= -1};  Key = System.ConsoleKey.UpArrow   };
                             {Direction=Down;  Delta={x=0; y=1};    Key = System.ConsoleKey.DownArrow };
                             {Direction=Left;  Delta={x=(-1); y=0}; Key = System.ConsoleKey.LeftArrow };
                             {Direction=Right; Delta={x=1; y=0};    Key = System.ConsoleKey.RightArrow}]

let getDirectionDescriptorByKey key = 
    directionDescriptors|>List.tryFind(fun elem->elem.Key=key)

let addPositions first second=
    {x=(first.x+second.x); y=(first.y+second.y)}

let makeMove (board: cell list) (delta: position) = 
    let playerCell = board|>List.find(fun elem->elem.CellState = Player || elem.CellState = PlayerGoal)
    let nextPlayerCellState = match playerCell.CellState with
                                | Player -> Floor
                                | _ -> Goal
    let nextPosition =  addPositions playerCell.Position delta
    let nextCell = board|>List.find(fun elem-> elem.Position=nextPosition)
    match nextCell.CellState with
    | x when x=Floor || x=Goal -> 
        let currentPlayerCellState =
            match x with
            | Floor -> Player
            | _ -> PlayerGoal
        let previousPlayerCell = {CellState = nextPlayerCellState; Position=playerCell.Position} 
        let currentPlayerCell = {CellState = currentPlayerCellState; Position=nextPosition}
        currentPlayerCell :: (previousPlayerCell :: (board|>List.filter(fun elem-> elem.Position<>playerCell.Position && elem.Position<>nextPosition)))
    | x when x=Box || x=BoxGoal ->
        let currentPlayerCellState =
            match x with
            | Box -> Player
            | _ -> PlayerGoal
        let previousPlayerCell = {CellState = nextPlayerCellState; Position=playerCell.Position} 
        let currentPlayerCell = {CellState = currentPlayerCellState; Position=nextPosition}
        let nextBoxPosition = addPositions nextPosition delta
        let nextBoxCell = board|>List.find(fun elem->elem.Position=nextBoxPosition)
        match nextBoxCell.CellState with
        | Floor ->
            let currentBoxCell = {CellState = Box; Position = nextBoxPosition}
            currentBoxCell :: (currentPlayerCell :: (previousPlayerCell :: (board|>List.filter(fun elem-> elem.Position<>playerCell.Position && elem.Position<>nextPosition && elem.Position<>nextBoxPosition))))
        | Goal ->
            let currentBoxCell = {CellState = BoxGoal; Position = nextBoxPosition}
            currentBoxCell :: (currentPlayerCell :: (previousPlayerCell :: (board|>List.filter(fun elem-> elem.Position<>playerCell.Position && elem.Position<>nextPosition && elem.Position<>nextBoxPosition))))
        | _ -> board
    | _ -> board

let rec playGame (board: cell list)=
    displayLevel board
    if isSolved board then
        board
    else
        let key = System.Console.ReadKey(true).Key
        let descriptor = getDirectionDescriptorByKey key
        if descriptor.IsNone then
            playGame board
        else
            let newBoard = makeMove board descriptor.Value.Delta
            playGame newBoard

[<EntryPoint>]
let main argv = 
    do System.Console.BackgroundColor<-System.ConsoleColor.Black
    do System.Console.Clear()
    do System.Console.CursorVisible <- false
    let board = loadLevel exampleLevel
    playGame board|>ignore
    do System.Console.ReadLine()|>ignore
    0