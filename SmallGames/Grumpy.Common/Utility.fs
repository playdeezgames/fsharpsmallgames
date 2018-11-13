namespace Grumpy.Common

module Utility =
    open Microsoft.Xna.Framework.Graphics
    open System.Text
    open Microsoft.Xna.Framework
    open Microsoft.Xna.Framework.Input
    open System

    let private splitArray<'T> (input:'T array) : 'T array =
        ((true, Array.empty), input)
        ||> Array.fold (fun (flip,output) v -> if flip then (not flip, [|v|] |> Array.append output ) else (not flip, output)) 
        |> snd

    let writeText (texture:Texture2D) (spriteBatch:SpriteBatch) (outputSize:Position) (position:Position) (text:string) (color:Color) :unit =
        let inputSize = (texture.Width/16, texture.Height/16)//TODO: yes, i hard coded it. so?
        (position, text |> Encoding.Unicode.GetBytes |> splitArray)
        ||> Array.fold 
            (fun p c -> 
                spriteBatch.Draw(texture,new Rectangle(p|>fst,p|>snd,outputSize|>fst,outputSize|>snd), new Rectangle((int(c % 16uy)) * (inputSize|>fst),(int(c / 16uy)) * (inputSize|>snd),inputSize|>fst,inputSize|>snd) |> Some |> Option.toNullable, color)
                ((p |> fst) + (outputSize |> fst), p|>snd)) 
        |>ignore

    let wasKeyPressed (key:Keys) (oldState:KeyboardState,newState:KeyboardState) : bool =
        (key |> oldState.IsKeyDown |> not) && (key |> newState.IsKeyDown)

    let generate(random:Random) (table:Map<'T,int>) : 'T =
        let total = 
            table
            |> Map.fold (fun acc _ v -> acc + v) 0
        let generated = 
            random.Next(total)
        table
        |> Map.fold
            (fun (result:'T option, n:int) k v -> 
                if result.IsSome then
                    (result, n)
                elif n < v then
                    (Some k, 0)
                else
                    (result, n-v)
                ) (None, generated)
        |> fst
        |> Option.get
    
    let parameterShim (f:unit->'T) (_:'T) : 'T =
        f()
