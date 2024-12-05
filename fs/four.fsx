#!/usr/bin/env -S dotnet fsi
#r "nuget: FSharpPlus, 1.6"

open FSharpPlus

let protect f = Result.protect f >> Result.mapError _.Message

let readInput path = monad' {
   let! lines = protect System.IO.File.ReadAllLines path
   do! if length lines <= 0 then Error "invalid input" else Ok ()
   do!
      if lines
         |> Seq.skip 1
         |> Seq.forall (length >> (=) (lines |> item 1 |> length))
      then Ok ()
      else Error "invalid input"
   return lines
}

let intOfBool b = if b then 1 else 0

let search f g (grid : string[]) (x, y) =
   grid[g y 1][f x 1] = 'M'
   && grid[g y 2][f x 2] = 'A'
   && grid[g y 3][f x 3] = 'S'

let search' p f g grid (x, y) =
   if p (x, y) then false else search f g grid (x, y)

let searchUp grid (x, y) =
   if y < 3 then 0
   else
      let searchUpLeft () = search' (fst >> flip (<) 3) (-) (-) grid (x, y)
      let searchUpRight () =
         search' (fun (x, y) -> x > length grid[y] - 4) (+) (-) grid (x, y)
      intOfBool (search konst (-) grid (x, y))
      + intOfBool (searchUpLeft ())
      + intOfBool (searchUpRight ())

let searchDown grid (x, y) =
   if y > length grid - 4 then 0
   else
      let searchDownLeft () = search' (fst >> flip (<) 3) (-) (+) grid (x, y)
      let searchDownRight () =
         search' (fun (x, y) -> x > length grid[y] - 4) (+) (+) grid (x, y)
      intOfBool (search konst (+) grid (x, y))
      + intOfBool (searchDownLeft ())
      + intOfBool (searchDownRight ())

let searchLeft grid (x, y) = search' (fst >> flip (<) 3) (-) konst grid (x, y)

let searchRight (grid : string[]) (x, y) =
   search' (fun (x, y) -> x > length grid[y] - 4) (+) konst grid (x, y)

let searchAround (grid : string[]) (x, y) =
   if y < 1 || x < 1 || y > length grid - 2 || x > length grid[y] - 2 then false
   else
      let upLeft = grid[y - 1][x - 1]
      let upRight = grid[y - 1][x + 1]
      let downLeft = grid[y + 1][x - 1]
      let downRight = grid[y + 1][x + 1]
      ((upLeft = 'M' && downRight = 'S') || (upLeft = 'S' && downRight = 'M'))
      && ((upRight = 'M' && downLeft = 'S')
        || (upRight = 'S' && downLeft = 'M'))

let run path = readInput path |>> fun (lns : string[]) ->
   let pts = seq { 0 .. length lns - 1 } >>= fun x ->
      zip (seq { 0 .. length lns[0] - 1}) (Seq.replicate <| length lns[x] <| x)
   let n = (0, pts) ||> fold (fun acc p ->
      if lns[snd p][fst p] <> 'X' then acc
      else
         acc
         + searchUp lns p
         + searchDown lns p
         + intOfBool (searchLeft lns p)
         + intOfBool (searchRight lns p))
   printfn "%d" n

   let n = (0, pts) ||> fold (fun acc p ->
      if lns[snd p][fst p] <> 'A'
      then acc
      else acc + intOfBool (searchAround lns p))
   printfn "%d" n

#if !INTERACTIVE
[<EntryPoint>]
#endif
let main args =
   match args with
   | [|_; arg|] ->
      match run arg with
      | Ok () -> 0
      | Error e ->
         eprintfn "%s" e
         1
   | _ ->
      eprintfn "invalid arugments"
      1

#if INTERACTIVE
main fsi.CommandLineArgs
#endif
