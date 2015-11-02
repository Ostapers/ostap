(*
 * Stream: lazy lists.
 * Copyright (C) 2006-2010
 * Dmitri Boulytchev, St.Petersburg State University
 * 
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License version 2, as published by the Free Software Foundation.
 * 
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * 
 * See the GNU Library General Public License version 2 for more details
 * (enclosed in the file COPYING).
 *)

open Lazy

type 'a t = ('a * 'a t) Lazy.t

let rec fromFunction  f      = lazy_from_fun (fun _ -> f (), fromFunction f)
let rec fromChannel   f inch = lazy_from_fun (fun _ -> f inch, fromChannel f inch)
let rec fromIterator  x f    = lazy_from_fun (fun _ -> let y, x' = f x in y, fromIterator x' f)
let rec fromGenerator x n i  = lazy_from_fun (fun _ -> i x, fromGenerator (n x) n i)
let rec fromList      l      = fromIterator  l (function [] -> raise End_of_file, [] | hd::tl -> hd, tl)
let rec fromArray     a      = let n = Array.length a in fromGenerator 0 (fun i -> i+1) (fun i -> if i < n then a.(i) else raise End_of_file)
let     fromFile             = fromChannel input_char
let     fromString    s      = let n = String.length s in fromGenerator 0 (fun i -> i+1) (fun i -> if i < n then s.[i] else raise End_of_file)
let     nil                  = fromList []
let     cons          x s    = lazy_from_val (x, s)

let complete f x = try f () with End_of_file -> x

let get   s = force s
let endOf s = complete (fun _ -> ignore (get s); false) true 
let hd    s = fst (get s)
let tl    s = snd (get s)
let rec concat x y = lazy_from_fun (fun _ -> try let x, xl = get x in x, concat xl y with End_of_file -> get y)

let rec map    f s = lazy_from_fun (fun _ -> let x, y = get s in f x, map f y)
let rec filter f s = lazy_from_fun (fun _ -> let (x, y) as z = get s in if f x then get (filter f y) else z)
let     iter   f s = let rec aux s = let x, y = get s in f x; aux y in try aux s with End_of_file -> ()
let     fold f x s =
   let acc = ref x in
   let rec aux s =
      let x, y = get s in
      acc := f !acc x;
      aux y
   in try aux s with End_of_file -> !acc

let last  s =
   let x, y = get s in
   let res = ref x in
   iter (fun x -> res := x) y;
   !res

let rec zip  x y     = lazy_from_fun (fun _ -> let (x, xl), (y, yl)                   = get x, get y               in (x, y)      , zip  xl yl      )
let rec zip3 x y z   = lazy_from_fun (fun _ -> let (x, xl), (y, yl), (z, zl)          = get x, get y, get z        in (x, y, z)   , zip3 xl yl zl   )
let rec zip4 x y z t = lazy_from_fun (fun _ -> let (x, xl), (y, yl), (z, zl), (t, tl) = get x, get y, get z, get t in (x, y, z, t), zip4 xl yl zl tl)

let rec unzip  x = map fst x, map snd x
let rec unzip3 x = map (fun (x, _, _) -> x) x, map (fun (_, x, _) -> x) x, map (fun (_, _, x) -> x) x
let rec unzip4 x = map (fun (x, _, _, _) -> x) x, map (fun (_, x, _, _) -> x) x, map (fun (_, _, x, _) -> x) x, map (fun (_, _, _, x) -> x) x

let rangeBy s l u = fromGenerator l (fun i -> i + s) (fun n -> if n > u then raise End_of_file else n)
let range         = rangeBy 1
let repeat n      = fromFunction (fun _ -> n)
let from   n      = fromGenerator n (fun i -> i+1) (fun i -> i)

module S = View.ListC (struct let concat = (^) end) (View.Char)

let take    n s = List.rev (fold (fun l x -> x :: l) [] (fst (unzip (zip s (range 1 n)))))
let takeStr n s = S.toString (take n s)

let matchPrefix f p s =
  let rec matchOne p s n =
    match p with
    | []     -> s, n
    | h :: t ->
        try 
          let c, s' = get s in
          if f h c then matchOne t s' (n+1) else s, n
        with End_of_file -> s, n
  in
  matchOne p s 0

let eqPrefix p s = matchPrefix (=) p s

let matchPrefixStr f p s =
  let m = String.length p in
  let rec matchOne i s =
    if i = m then s, i
    else
      try 
        let c, s' = get s in
        if f p.[i] c then matchOne (i+1) s' else s, i
      with End_of_file -> s, i
  in
  matchOne 0 s 

let eqPrefixStr p s = matchPrefixStr (=) p s

