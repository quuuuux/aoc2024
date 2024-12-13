#!/usr/bin/env -S dotnet fsi
#r "nuget: FSharpPlus, 1.6"

open FSharpPlus

let protect f = Result.protect f >> Result.mapError _.Message

let uint8OfChar c = uint8 c - uint8 '0'

let readInput path = monad' {
   let! lines = protect System.IO.File.ReadAllLines path
   do! if length lines <= 0 then Error "invalid input" else Ok ()
   do!
      if lines
         |> Seq.skip 1
         |> Seq.forall (length >> (=) (lines |> item 1 |> length))
      then Ok ()
      else Error "invalid input"
   return lines |> Array.map (Seq.map uint8OfChar >> Array.ofSeq)
}

let tryStep (rows : uint8[][]) (x, y) =
   if y >= 0 && y < length rows && x >= 0 && x < length rows[y]
   then rows[y][x] |> int |> Some
   else None

let one (rows : uint8[][]) =
   let pts = seq { 0 .. length rows - 1 } >>= fun y ->
      zip (seq { 0 .. length rows[0] - 1}) (Seq.replicate <| length rows[y] <| y)
   let sum = (0, pts) ||> fold (fun acc (x, y) ->
      if rows[y][x] <> 0uy then acc
      else
         let queue = System.Collections.Generic.PriorityQueue()
         queue.Enqueue((x, y), 0)
         let nines = System.Collections.Generic.HashSet()
         let mutable elt = Unchecked.defaultof<_>
         let mutable prio = Unchecked.defaultof<_>
         while queue.TryDequeue(&elt, &prio) do
            if prio = 9 then
               nines.Add(elt) |> ignore
            else
               let (x, y) = elt
               let maybeInsert (x, y) =
                  match tryStep rows (x, y) with
                  | Some p when p = prio + 1 -> queue.Enqueue((x, y), p)
                  | _ -> ()
               maybeInsert (x, y + 1)
               maybeInsert (x, y - 1)
               maybeInsert (x - 1, y)
               maybeInsert (x + 1, y)
         acc + length nines)
   printfn "%d" sum

let two (rows : uint8[][]) =
   let queue = System.Collections.Generic.PriorityQueue()
   let pts = seq { 0 .. length rows - 1 } >>= fun y ->
      zip (seq { 0 .. length rows[0] - 1}) (Seq.replicate <| length rows[y] <| y)
   pts |> iter (fun (x, y) ->
      if rows[y][x] = 0uy then
         queue.Enqueue((x, y), 0))
   let mutable sum = 0
   let mutable elt = Unchecked.defaultof<_>
   let mutable prio = Unchecked.defaultof<_>
   while queue.TryDequeue(&elt, &prio) do
      if prio = 9 then
         sum <- sum + 1
      else
         let (x, y) = elt
         let maybeInsert (x, y) =
            match tryStep rows (x, y) with
            | Some p when p = prio + 1 -> queue.Enqueue((x, y), p)
            | _ -> ()
         maybeInsert (x, y + 1)
         maybeInsert (x, y - 1)
         maybeInsert (x - 1, y)
         maybeInsert (x + 1, y)
   printfn "%d" sum

let run path = readInput path |>> fun rs ->
   one rs
   two rs

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
