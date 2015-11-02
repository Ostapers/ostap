(*
 * Samlpe: Ostap sample.
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

open Printf
open String
open Str
open Ostap
open Util
open Matcher

let ident = ostap (n:IDENT {Token.repr n})
let const = ostap (c:CONST {int_of_string (Token.repr c)})

let rec toString e =
  let prio = function
    | `Op (`Add, _) | `Op (`Sub, _) -> 3
    | `Op (`Mul, _) | `Op (`Div, _) | `Op (`Mod, _) -> 4
    | `Op (`Lt , _) | `Op (`Le , _) | `Op (`Gt , _) | `Op (`Ge, _) | `Op (`Eq, _) | `Op (`Neq, _) -> 2
    | `Op (`And, _) | `Op (`Or , _) -> 1
    | _ -> 5
  in
  let pe = prio e in
  let enclose e =
    (if prio e < pe then fun x -> "(" ^ x ^ ")" else fun x -> x) (toString e)
  in
  match e with
  | `Op (`Add, (x, y)) -> (enclose x) ^ " + "  ^ (enclose y) 
  | `Op (`Sub, (x, y)) -> (enclose x) ^ " - "  ^ (enclose y) 
  | `Op (`Mul, (x, y)) -> (enclose x) ^ " * "  ^ (enclose y) 
  | `Op (`Div, (x, y)) -> (enclose x) ^ " / "  ^ (enclose y) 
  | `Op (`Mod, (x, y)) -> (enclose x) ^ " % "  ^ (enclose y) 
  | `Op (`Eq , (x, y)) -> (enclose x) ^ " == " ^ (enclose y)
  | `Op (`Neq, (x, y)) -> (enclose x) ^ " != " ^ (enclose y) 
  | `Op (`Lt , (x, y)) -> (enclose x) ^ " < "  ^ (enclose y) 
  | `Op (`Le , (x, y)) -> (enclose x) ^ " <= " ^ (enclose y)
  | `Op (`Gt , (x, y)) -> (enclose x) ^ " > "  ^ (enclose y) 
  | `Op (`Ge , (x, y)) -> (enclose x) ^ " >= " ^ (enclose y) 
  | `Op (`And, (x, y)) -> (enclose x) ^ " && " ^ (enclose y) 
  | `Op (`Or , (x, y)) -> (enclose x) ^ " || " ^ (enclose y) 
  | `Neg   x           -> "-" ^ (enclose x)
  | `Not   e           -> "!" ^ (enclose e)
  | `Int   n           -> string_of_int n
  | `Name  s           -> s

let rec parse s = 
  expr (fun x -> x)     
    [|
      `Righta, [(ostap ("&&")), (fun x y -> `Op `And (x, y)); (ostap ("||")), (fun x y -> `Op `Or (x, y))];
      `Righta, [
      (ostap ("==")), (fun x y -> `Op `Eq  (x, y)); 
      (ostap ("!=")), (fun x y -> `Op `Neq (x, y)); 
      (ostap (">")) , (fun x y -> `Op `Gt  (x, y)); 
      (ostap (">=")), (fun x y -> `Op `Ge  (x, y)); 
      (ostap ("<")) , (fun x y -> `Op `Lt  (x, y)); 
      (ostap ("<=")), (fun x y -> `Op `Le  (x, y))
    ];
      `Lefta, [(ostap ("+")), (fun x y -> `Op `Add (x, y)); (ostap ("-")), (fun x y -> `Op `Sub (x, y))]; 
      `Lefta, [(ostap ("*")), (fun x y -> `Op `Mul (x, y)); (ostap ("/")), (fun x y -> `Op `Div (x, y)); (ostap ("%")), (fun x y -> `Op `Mod (x, y))]
    |]    
    primary 
    s
and ostap (
  primary: 
    n:ident           {`Name n} 
  | c:const           {`Int  c} 
  | "-" p:primary     {`Neg  p}
  | "!" p:primary     {`Not  p}
  | -"(" parse -")" 
 )

class lexer s =
  let skip  = Skip.create [Skip.whitespaces " \n\t\r"] in
  let ident = Str.regexp "[a-zA-Z][a-zA-Z0-9]*" in
  let const = Str.regexp "[0-9]+" in

  object (self)

    inherit t s

    method skip p c = skip s p c
    method getIDENT = self#get "identifier" ident
    method getCONST = self#get "constant" const

  end

let _ =
  Combinators.unwrap (parse (new lexer "a+b*3-(4-2)"))
    (fun tree -> printf "Parsed tree:\n%s\n" (toString tree))
    (fun reason -> printf "Parser error:\n%s\n" (Reason.toString (`First 3) `Acc reason))

