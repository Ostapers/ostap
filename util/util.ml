(*
 * Util: predefined Ostap utilities.
 * Copyright (C) 2006-2009
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

open Combinators
open Matcher 
open Printf 

module Ostap =
  struct

    module Combinators = Combinators

  end

ostap (
  listByWith[delim][item][f][x]: h:item result:(-delim item) * with{f x h}{f} {result}
)

ostap (
  listBy[delim][item]: h:item t:(-delim item)* {h::t}
)

ostap (
  listWith[item][f][x]: listByWith[ostap (",")][item][f][x]
)

ostap (
  list: listBy[ostap (",")]
)

ostap (
  list0ByWith[delim][item][f][x]: h:item result:(-delim item) * with{f x h}{f} {result} | empty {x}
)

ostap (
  list0By[delim][item]: listBy[delim][item] | empty {[]}
)

ostap (
  list0With[item][f][x]: list0ByWith[ostap (",")][item][f][x]
)

ostap (
  list0: list0By[ostap (",")]
)

let left  f c x y = f (c x) y
let right f c x y = f x y

ostap (
  id[x]: x
)

let rec here1 () = if false then here1 () else ()

let expr f ops opnd =
  here1 ();
  let ops =
    Array.map 
      (fun (assoc, list) ->
        let g = match assoc with `Lefta | `Nona -> left | `Righta -> right in
        assoc = `Nona, altl (List.map (fun (oper, sema) -> ostap (!(oper) {g sema})) list)
      )
      ops 
  in
  let n      = Array.length ops in
  let op   i = snd ops.(i)      in
  let nona i = fst ops.(i)      in
  let id x   = x                in  
  let ostap (
    inner[l][c]: f[ostap (
      {n = l                } => x:opnd {c x}
    | {n > l && not (nona l)} => x:inner[l+1][id] b:(-o:op[l] inner[l][o c x])? {
        match b with None -> c x | Some x -> x
      }
    | {n > l && nona l} => x:inner[l+1][id] b:(-o:op[l] inner[l+1][o c x])? {
        match b with None -> c x | Some x -> x
      })]
  )
  in 
  ostap (inner[0][id])

let read name = 
  let inch = open_in_bin name in
  let len  = in_channel_length inch in
  let buf  = String.make len ' ' in
  really_input inch buf 0 len;
  close_in inch;
  buf
