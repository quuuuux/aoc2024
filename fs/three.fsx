#!/usr/bin/env -S dotnet fsi
#r "nuget: FParsec, 1.1"
#r "nuget: FSharpPlus, 1.6"

open FSharpPlus

type Inst = Do | Dont | Mul of int * int

module Parse =
   open FParsec

   let skipOther = skipNoneOf "dm" >>% None

   let o = stringReturn "o()" Do
   let ont = stringReturn "on't()" Dont
   let doOrDont = skipChar 'd' >>. opt (o <|> ont)

   let ul =
      skipString "ul("
      >>? pint32
      .>>? skipChar ','
      .>>.? pint32
      .>>? skipChar ')'
      |>> Mul

   let mul = skipChar 'm' >>. opt ul

   let insts = many (skipOther <|> doOrDont <|> mul)

   let parse s =
      match run insts s with
      | Success (ms, _, _) -> Result.Ok ms
      | Failure (e, _, _) -> Result.Error e

let protect f = Result.protect f >> Result.mapError _.Message

let run path =
   protect System.IO.File.ReadAllText path >>= Parse.parse |>> (fun insts ->
      let muls = function
         | Some(Mul (x, y)) -> Some (x, y)
         | _ -> None
      let all =
         (0, Seq.choose muls insts) ||> fold (fun acc (x, y) -> acc + (x * y))
      printfn "%u" all

      let filtered =
         ((0, true), Seq.choose id insts) ||> fold (fun (acc, do') inst ->
            match inst with
            | Do -> (acc, true)
            | Dont -> (acc, false)
            | Mul (x, y) -> if do' then (acc + (x * y), do') else (acc, do'))
      printfn "%u" <| fst filtered)

#if !INTERACTIVE
[<EntryPoint>]
#endif
let main args = function
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
