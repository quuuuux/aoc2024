#!/usr/bin/env -S dotnet fsi
#r "nuget: FParsec, 1.1"
#r "nuget: FSharpPlus, 1.6"

open FSharpPlus

module Parse =
   open FParsec

   let order = puint32 .>> skipChar '|' .>>. puint32

   let update = sepBy puint32 <| skipChar ','

   let input =
      many (order .>> skipNewline)
      .>> skipNewline
      .>>. many (update .>> skipNewline)

   let parse s =
      match run input s with
      | Success (x, _, _) -> Result.Ok x
      | Failure (e, _, _) -> Result.Error e

let protect f = Result.protect f >> Result.mapError _.Message

let rec valid order update =
   match update with
   | [] | [_] -> true
   | x :: y :: tail ->
      match order |> Map.tryFind y with
      | Some xs ->
         if xs |> Set.contains x then false else valid order <| y :: tail
      | None -> valid order <| y :: tail

let cmpOrder order x y =
   match order |> Map.tryFind y with
   | Some xs -> if xs |> Set.contains x then -1 else 1
   | None -> 0

let run path =
   protect System.IO.File.ReadAllText path
   >>= Parse.parse
   |>> fun (orders, updates) ->
      let order =
         (Map.empty, orders) ||> fold (fun acc ord ->
            acc |> Map.change (fst ord) (function
               | Some xs -> Some (xs |> Set.add (snd ord))
               | None -> Some (Set.singleton <| snd ord)))
      let (ans, bad) = ((0u, []), updates) ||> fold (fun (ans, bad) upd ->
         let upd' = Array.ofList upd
         if not <| valid order upd
         then (ans, upd' :: bad)
         else (ans + (upd' |> item (length upd' / 2))), bad)
      printfn "%u" ans

      let ans = (0u, bad) ||> fold (fun acc upd ->
         upd |> Array.sortInPlaceWith (cmpOrder order)
         acc + (upd |> item (length upd / 2)))
      printfn "%u" ans

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
