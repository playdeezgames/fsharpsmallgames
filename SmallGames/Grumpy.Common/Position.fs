namespace Grumpy.Common

type Position = int * int

module Position =
    let add (first:Position) (second:Position) : Position =
        ((first |> fst) + (second |> fst), (first |> snd) + (second |> snd))
    
    let multiply  (first:Position) (second:Position) : Position =
        ((first |> fst) * (second |> fst), (first |> snd) * (second |> snd))

    let divide (first:Position) (second:Position) : Position = 
        ((first |> fst) / (second |> fst), (first |> snd) / (second |> snd))

    let wrap (size:Position) (position:Position) : Position =
        let x,y = position
        let width, height = size
        let wrappedX = 
            match x % width with
            | n when n < 0 -> n + width
            | n -> n
        let wrappedY =
            match y % height with
            | n when n < 0 -> n + height
            | n -> n
        (wrappedX, wrappedY)

    



