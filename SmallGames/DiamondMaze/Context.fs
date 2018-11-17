namespace Grumpy.DiamondMaze

open System
open Microsoft.Xna.Framework.Input

type Context =
    {counters:Map<Keys,TimeSpan>}

module Context =
    let create () =
        {counters = Map.empty}
    
    let setCounter (key:Keys) (value:TimeSpan) (context:Context) : Context =
        {context with counters = context.counters |> Map.add key value}

    let removeCounter (key:Keys) (context:Context) : Context =
        {context with counters = context.counters |> Map.remove key}

    let getCounter (key:Keys) (context:Context) : TimeSpan =
        context.counters.[key]
