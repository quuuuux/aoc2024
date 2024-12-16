#!/usr/bin/env -S dotnet fsi
#r "nuget: FParsec, 1.1"
#r "nuget: FSharpPlus, 1.6"

open FSharpPlus

type Tile = Empty | Box | Wall
type Direction = Up | Down | Left | Right

module Parse =
   open FParsec

   let tile =
      (skipChar '.' >>% Empty)
      <|> (skipChar '@' >>. getPosition >>= (Some >> setUserState) >>% Empty)
      <|> (skipChar 'O' >>% Box)
      <|> (skipChar '#' >>% Wall)

   let grid = many (many1 tile .>> skipNewline)

   let direction =
      (skipChar '^' >>% Up)
      <|> (skipChar 'v' >>% Down)
      <|> (skipChar '<' >>% Left)
      <|> (skipChar '>' >>% Right)

   let moves = many (direction .>> optional skipNewline)

   let input = grid .>> skipNewline .>>. moves

   let parse path =
      match runParserOnFile input None path <| System.Text.ASCIIEncoding() with
      | Success ((g, ms), p, _) -> Result.Ok (g, p, ms)
      | Failure (e, _, _) -> Result.Error e

let moveFn = function
   | Up -> fun x y -> (x, y - 1)
   | Down -> fun x y -> (x, y + 1)
   | Left -> fun x y -> (x - 1, y)
   | Right -> fun x y -> (x + 1, y)

module One =
   let rec tryMoveBoxes (grid : Tile[][]) (x, y) f =
      match grid[y][x] with
      | Empty ->
         grid[y][x] <- Box
         true
      | Box -> tryMoveBoxes grid <| f x y <| f
      | Wall -> false

   let move (grid : Tile[][]) (x, y) dir =
      let f = moveFn dir
      let (x', y') = f x y
      match grid[y'][x'] with
      | Empty -> (x', y')
      | Box ->
         if not (tryMoveBoxes grid <| f x' y' <| f) then (x, y)
         else
            grid[y'][x'] <- Empty
            (x', y')
      | Wall -> (x, y)

module Two =
   type Tile' = Empty' | BoxLeft | BoxRight | Wall'

   let widen (grid : Tile[][]) = grid |>> Array.collect (function
      | Empty -> [|Empty'; Empty'|]
      | Box -> [|BoxLeft; BoxRight|]
      | Wall -> [|Wall'; Wall'|])

   let ordered x y = if x < y then (x, y) else (y, x)

   let rec tryMoveBoxesX (grid : Tile'[][]) x x' x'' y f =
      let doMove () =
         let (l, r) = ordered x' x''
         grid[y][l] <- BoxLeft
         grid[y][r] <- BoxRight
      match grid[y][x''] with
      | Empty' ->
         doMove ()
         true
      | Wall' -> false
      | _ ->
         let (x''', _) = f x'' y
         let (x'''', _) = f x''' y
         let r = tryMoveBoxesX grid x'' x''' x'''' y f
         if r then
            doMove ()
         r

   type MoveFuncs = {
      Clear : ResizeArray<Tile'[][] -> unit>
      Draw : ResizeArray<Tile'[][] -> unit> }

   let rec tryMoveBoxesY (grid : Tile'[][]) l r y f moveFns =
      let addFns () =
         moveFns.Draw.Add(fun g -> g[y][l] <- BoxLeft)
         moveFns.Draw.Add(fun g -> g[y][r] <- BoxRight)
      match grid[y][l] with
      | Wall' -> false
      | Empty' ->
         match grid[y][r] with
         | Wall' -> false
         | Empty' ->
            addFns ()
            true
         | BoxLeft ->
            let (_, y') = f l y
            let r' = tryMoveBoxesY grid r (r + 1) y' f moveFns
            if r' then
               addFns ()
               moveFns.Clear.Add(fun g -> g[y][r + 1] <- Empty')
            r'
         | BoxRight -> failwith "unreachable"
      | BoxLeft ->
         if grid[y][r] <> BoxRight then
            failwith "unreachable"
         let (_, y') = f l y
         let r = tryMoveBoxesY grid l r y' f moveFns
         if r then
            addFns ()
         r
      | BoxRight ->
         match grid[y][r] with
         | Wall' -> false
         | Empty' ->
            let (_, y') = f l y
            let r = tryMoveBoxesY grid (l - 1) l y' f moveFns
            if r then
               addFns ()
               moveFns.Clear.Add(fun g -> g[y][l - 1] <- Empty')
            r
         | BoxLeft ->
            let (_, y') = f l y
            let r' = tryMoveBoxesY grid (l - 1) l y' f moveFns
            if not r' then false
            else
               let r' = tryMoveBoxesY grid  r (r + 1) y' f moveFns
               if r' then
                  addFns ()
                  moveFns.Clear.Add(fun g -> g[y][l - 1] <- Empty')
                  moveFns.Clear.Add(fun g -> g[y][r + 1] <- Empty')
               r'
         | BoxRight -> failwith "unreachable"

   let tryMoveBoxes grid (x, y) x' y' dir f g =
      let (x'', _) = g x' y'
      match dir with
      | Up | Down ->
         let moveFns = { Clear = ResizeArray(); Draw = ResizeArray () }
         if not <| tryMoveBoxesY grid x' x'' y' f moveFns then (x, y)
         else
            moveFns.Clear |> iter (fun f -> f grid)
            moveFns.Draw |> iter (fun f -> f grid)
            grid[y'][x'] <- Empty'
            grid[y'][x''] <- Empty'
            (x', y')
      | Left | Right ->
         let (x''', _) = g x'' y'
         if not <| tryMoveBoxesX grid x' x'' x''' y' f then (x, y)
         else
            grid[y'][x'] <- Empty'
            (x', y')

   let move (grid : Tile'[][]) (x, y) dir =
      let f = moveFn dir
      let (x', y') = f x y
      match grid[y'][x'] with
      | Empty' -> (x', y')
      | BoxLeft -> tryMoveBoxes grid (x, y) x' y' dir f <| moveFn Right
      | BoxRight -> tryMoveBoxes grid (x, y) x' y' dir f <| moveFn Left
      | Wall' -> (x, y)

let run path = path |> Parse.parse >>= fun (grid, pos, moves) ->
   match pos with
   | None -> Error "invalid input"
   | Some p ->
      let grid = grid |> Seq.map Array.ofList |> Array.ofSeq
      let wideGrid = Two.widen grid
      let (x, y) = (int p.Column - 2, int p.Line - 1)
      ((x, y), moves) ||> fold (fun p m -> One.move grid p m) |> ignore
      let pts = seq { 0 .. length grid - 1 } >>= fun y ->
         let is = (seq { 0 .. length grid[0] - 1})
         let js = (Seq.replicate <| length grid[y] <| y)
         zip is js
      let n = (0, pts) ||> fold (fun acc (x, y) ->
         match grid[y][x] with
         | Box -> acc + x + (100 * y)
         | _ -> acc)
      printfn "%d" n

      ((x * 2, y), moves) ||> fold (fun p m -> Two.move wideGrid p m) |> ignore
      let pts = seq { 0 .. length wideGrid - 1 } >>= fun y ->
         let is = (seq { 0 .. length wideGrid[0] - 1})
         let js = (Seq.replicate <| length wideGrid[y] <| y)
         zip is js
      let n = (0, pts) ||> fold (fun acc (x, y) ->
         match wideGrid[y][x] with
         | Two.BoxLeft -> acc + x + (100 * y)
         | _ -> acc)
      Ok <| printfn "%d" n

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
