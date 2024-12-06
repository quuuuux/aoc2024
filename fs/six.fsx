#!/usr/bin/env -S dotnet fsi
#r "nuget: FParsec, 1.1"
#r "nuget: FSharpPlus, 1.6"

open FSharpPlus
open System.Collections.Generic

type Tile = Empty | Blocked

module Parse =
   open FParsec

   let tile =
      (skipChar '.' >>% Empty)
      <|> (skipChar '^' >>. getPosition >>= (Some >> setUserState) >>% Empty)
      <|> (skipChar '#' >>% Blocked)

   let grid = many (many tile .>> skipNewline)

   let parse path =
      match runParserOnFile grid None path <| System.Text.ASCIIEncoding() with
      | Success (m, p, _) -> Result.Ok (m, p)
      | Failure (e, _, _) -> Result.Error e

type Direction = Up | Down | Left | Right

let rec nextPos (grid : Tile[][]) (x, y) dir =
   if x < 0 || x >= length grid[0] || y < 0 || y >= length grid then None
   else
      match grid[y][x] with
      | Blocked ->
         match dir with
         | Up -> nextPos grid (x + 1, y + 1) Right
         | Down -> nextPos grid (x - 1, y - 1) Left
         | Left -> nextPos grid (x + 1, y - 1) Up
         | Right -> nextPos grid (x - 1, y + 1) Down
      | _ -> Some ((x, y), dir)

let rec move (grid : Tile[][]) visited (x, y) dir =
   let visited = visited |> Set.add (x, y)
   let pos =
      match dir with
      | Up -> nextPos grid (x, y - 1) Up
      | Down -> nextPos grid (x, y + 1) Down
      | Left -> nextPos grid (x - 1, y) Left
      | Right -> nextPos grid (x + 1, y) Right
   match pos with
   | Some (p, d) -> move grid visited p d
   | None -> visited

let rec findCycle (grid : Tile[][]) (acc : HashSet<_>) (x, y) dir =
   if not <| acc.Add(x, y, dir) then true
   else
      let pos =
         match dir with
         | Up -> nextPos grid (x, y - 1) Up
         | Down -> nextPos grid (x, y + 1) Down
         | Left -> nextPos grid (x - 1, y) Left
         | Right -> nextPos grid (x + 1, y) Right
      match pos with
      | Some (p, d) -> findCycle grid acc p d
      | None -> false

let intOfBool b = if b then 1 else 0

let run path =
   path |> Parse.parse |> Result.bind (fun (grid, pos) ->
      match pos with
      | None -> Error "invalid input"
      | Some p ->
         let pos = (int p.Column - 2, int p.Line - 1)
         let grid = grid |> Seq.map Array.ofList |> Array.ofSeq
         let visited = move grid Set.empty pos Up
         printfn "%d" <| length visited

         let ans = (0, visited |> Set.remove pos) ||> fold (fun acc (x, y) ->
            grid[y][x] <- Blocked
            let cycle = findCycle grid <| HashSet() <| pos <| Up
            grid[y][x] <- Empty
            acc + intOfBool cycle)
         Ok <| printfn "%d" ans)

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
