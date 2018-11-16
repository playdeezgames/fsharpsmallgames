namespace Grumpy.DiamondMaze

open System

type Context =
    {counter:TimeSpan}

module Context =
    let create () =
        {counter = TimeSpan.Zero}
