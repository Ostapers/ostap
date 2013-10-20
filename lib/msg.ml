(*
 * Msg: parsing message module.
 * Copyright (C) 2006
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

open Printf

module Coord =
  struct

    type t = int * int

    let line = fst
    let col  = snd

    let toString (x, y) = sprintf "(%d:%d)" x y 

    let compare : t -> t -> int = fun (r, c) (r', c') ->
      let x = compare r r' in
      if x = 0 then compare c c' else x
   
  end

module Locator =
  struct

    type t = No | Point of Coord.t | Interval of Coord.t * Coord.t | Set of t list
    and  l = t

    let makeInterval x y =
      match x, y with
      | Point x, Point y -> Interval (x, y)
      | _ -> Set [x; y]

    let rec toString = function
      | No              -> ""
      | Point x         -> Coord.toString x 
      | Interval (x, y) -> sprintf "%s-%s" (Coord.toString x) (Coord.toString y)
      | Set      x      -> 
	  let module M = View.List (struct type t = l let toString = toString end) in
          M.toString x

    let compare x y =
	match (x, y) with
	| No, No -> 0
	| No, _  -> -1
	| _ , No -> 1
	| _ ->
	    let rec least = function
	      | No              -> (0, 0)
	      | Point     x     -> x
	      | Interval (x, _) -> x
	      | Set       x     -> List.hd (List.sort Coord.compare (List.map least x))
	    in
	    Coord.compare (least x) (least y)

  end

type t = {phrase: string; args: string array; loc: Locator.t} 

let make      phrase args loc = {phrase=phrase; args=args; loc=loc}
let loc       t               = t.loc

let phrase    phrase          = make phrase [||] Locator.No
let orphan    phrase args     = make phrase args Locator.No

let string t = 
  let parmExpr = Str.regexp "%\\([0-9]+\\)" in
  Str.global_substitute 
    parmExpr  
    (fun s -> 
      try 
        t.args.(int_of_string (Str.replace_matched "\\1" s))
      with
      | Failure "int_of_string" -> 
          raise (Failure 
                   (sprintf "invalid integer parameter specification in message phrase \"%s\"" s)
                )
	    
      | Invalid_argument "index out of bounds" ->
          raise (Failure 
                   (sprintf "index out of bound while accessing message parameter in \"%s\"" s)
                )
    )
    t.phrase
    
let toString t =
  let message = string t in
    match Locator.toString t.loc with
    | ""  -> message
    | loc -> message ^ " at " ^ loc
      
let augment msg loc = match msg.loc with Locator.No -> {msg with loc = loc} | _ -> msg
let augmentList msgs loc = List.map (fun x -> augment x loc) msgs

let extend msg str = {msg with phrase=str ^ msg.phrase}
let extendList msgs str = List.map (fun msg -> extend msg str) msgs
