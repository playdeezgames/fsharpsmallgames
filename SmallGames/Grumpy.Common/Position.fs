namespace Grumpy.Common

type Position = int * int

module Position =
    let add (first:Position) (second:Position) : Position =
        ((first |> fst) + (second |> fst), (first |> snd) + (second |> snd))
    
    let multiply  (first:Position) (second:Position) : Position =
        ((first |> fst) * (second |> fst), (first |> snd) * (second |> snd))
    



