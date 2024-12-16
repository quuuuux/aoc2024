#!/usr/bin/env -S dotnet fsi
#r "nuget: FParsec, 1.1"
#r "nuget: FSharpPlus, 1.6"

open FSharpPlus
open System.Collections.Generic

type Tile = Empty | Wall
type Direction = Up | Down | Left | Right

module Parse =
   open FParsec

   let updateBegin p s = (Some p, snd s)
   let updateEnd p s = (fst s, Some p)
   let setBegin = updateBegin >> updateUserState
   let setEnd = updateEnd >> updateUserState

   let tile =
      (skipChar '.' >>% Empty)
      <|> (skipChar 'S' >>. getPosition >>= setBegin >>% Empty)
      <|> (skipChar 'E' >>. getPosition >>= setEnd >>% Empty)
      <|> (skipChar '#' >>% Wall)

   let grid = many (many1 tile .>> skipNewline)

   let parse path =
      let r =
         runParserOnFile grid (None, None) path <| System.Text.ASCIIEncoding()
      match r with
      | Success (g, (s, e), _) -> Result.Ok (g, s, e)
      | Failure (e, _, _) -> Result.Error e

let solve (grid : Tile[][]) (xb, yb) (xe, ye) =
   let queue = PriorityQueue()
   queue.Enqueue((xb, yb, Right, Right), 0)
   let visited = Dictionary()
   let mutable elt = Unchecked.defaultof<_>
   let mutable prio = Unchecked.defaultof<_>
   while queue.TryDequeue(&elt, &prio) do
      let (x', y', d, d') = elt
      let prev =
         match d' with
         | Up -> (x', y' + 1, d)
         | Down -> (x', y' - 1, d)
         | Left -> (x' + 1, y', d)
         | Right -> (x' - 1, y', d)
      let mutable val' = Unchecked.defaultof<_ * ResizeArray<_>>
      if visited.TryGetValue((x', y', d'), &val') then
         if prio <= fst val' then
            let seats = (snd val')
            seats.Add(prev) |> ignore
            visited[(x', y', d')] <- (prio, seats)
      else
         let seats = ResizeArray()
         seats.Add(prev) |> ignore
         visited.Add((x', y', d'), (prio, seats))
         if (x', y') <> (xe, ye) then
            if grid[y' - 1][x'] = Empty then
               match d' with
               | Up -> queue.Enqueue((x', y' - 1, d', Up), prio + 1)
               | Down -> ()
               | _ -> queue.Enqueue((x', y' - 1, d', Up), prio + 1001)
            if grid[y' + 1][x'] = Empty then
               match d' with
               | Up -> ()
               | Down -> queue.Enqueue((x', y' + 1, d', Down), prio + 1)
               | _ -> queue.Enqueue((x', y' + 1, d', Down), prio + 1001)
            if grid[y'][x' - 1] = Empty then
               match d' with
               | Left -> queue.Enqueue((x' - 1, y', d', Left), prio + 1)
               | Right -> ()
               | _ -> queue.Enqueue((x' - 1, y', d', Left), prio + 1001)
            if grid[y'][x' + 1] = Empty then
               match d' with
               | Left -> ()
               | Right -> queue.Enqueue((x' + 1, y', d', Right), prio + 1)
               | _ -> queue.Enqueue((x' + 1, y', d', Right), prio + 1001)
   visited

let trace (visited : Dictionary<_, _>) (xb, yb) (xe, ye) =
   let ds =
      (Map.empty, [|Up; Down; Left; Right|])
      ||> fold (fun acc d ->
         let mutable val' = Unchecked.defaultof<_>
         if visited.TryGetValue((xe, ye, d), &val') then
            acc |> Map.change (fst val') (function
               | Some ds -> Some <| d :: ds
               | None -> Some [d])
         else
            acc)
      |> Seq.head // There has to be a better way
   printfn "%d" ds.Key
   let seats =
      ds.Value
      |> Seq.collect (fun d -> snd visited[(xe, ye, d)])
      |> ResizeArray
   let seen = HashSet()
   while (length seats) <> 0 do
      let i = length seats - 1
      let (x, y, d) = seats[i]
      seats.RemoveAt(i)
      if (x, y) <> (xb, yb) then
         for seat in snd visited[(x, y, d)] do
            seats.Add(seat)
      seen.Add((x, y)) |> ignore
   seen

let run path = path |> Parse.parse >>= fun (grid, begin', end') ->
   match (begin', end') with
   | (None, _) | (_, None) -> Ok ()
   | (Some s, Some e) ->
      let grid = grid |> Seq.map Array.ofList |> Array.ofSeq
      let (xb, yb) = (int s.Column - 2, int s.Line - 1)
      let (xe, ye) = (int e.Column - 2, int e.Line - 1)
      let visited = solve grid (xb, yb) (xe, ye)
      let seen = trace visited (xb, yb) (xe, ye)
      Ok <| printfn "%d" (length seen + 1)

#if !INTERACTIVE
[<EntryPoint>]
#endif
let main = function
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
