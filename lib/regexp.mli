(*
 * Regexp: regular expression matching against streams.
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

(** Regular expression matching. *)

(** {2 General description} *)

(** Main type: ['a t] is regular expression over the alphabet ['a]. *)
type 'a t =
    Test   of string * ('a -> bool) (** [Test (n, p)] matches the single element 
                                        against the predicate [p];  argument [n] 
                                        is provided  for debugging/visualization 
                                        purposes. 
                                     *)
  | Before of 'a t                  (** Lookahead condition.                  *)
  | Aster  of 'a t                  (** Regular zero-or-more repetition.      *)
  | Plus   of 'a t                  (** Regular one-or-more repetition.       *)
  | Opt    of 'a t                  (** Optional pattern.                     *)
  | Alter  of 'a t list             (** Alternative.                          *)
  | Juxt   of 'a t list             (** Juxstaposition.                       *)
  | Bind   of string * 'a t         (** Argument capturing.                   *)                                       
  | Arg    of string                (** Argument matching. Argument have to be
                                        completely captured prior to matching.
                                     *)
  | EOS                             (** End of stream.                        *)

(** Fold function. *)
val fold : ('b -> 'a t -> 'b) -> 'b -> 'a t -> 'b

(** Pretty-printer. *)
val toText : 'a t -> Pretty.printer 

(** String conversion. *)
val toString : 'a t -> string

(** {2 Regular expression diagram} *)
module Diagram :
  sig

    (** Type synonym for expression. *)
    type 'a expr = 'a t

    (** Type for the diagram. *)
    type 'a t 

    (** DOT visualizer. *)
    val toDOT : 'a t -> string

    (** Constructor. *)
    val make : 'a expr -> 'a t 

    (** Compiled diagram for nondeterministic matching. *)
    module Compiled :
      sig
  
        (** Type synonym for diagram. *)
        type 'a diagram = 'a t

        (** Type of the compiled diagram. *)
        type 'a t

        (** Constructor. *)
        val make : 'a diagram -> 'a t

        (** [matchStream t s] matched the stream [s] against the expression [t] and returns all 
            matchings as a stream of pairs [(s', args)], where [s'] --- residual stream,
            [args name] --- matched value for argument [name] (empty list for non-captured or
            non-matched names)
          *)
        val matchStream : 'a t -> 'a Ostream.t -> ('a Ostream.t * (string -> 'a list)) Ostream.t
  
      end

  end

(** {2 Shortcuts and synonyms} *)

(** [matchAll expr stream] nondeterministically finds all matchings of [stream] against [expr].
    The result is a stream of pairs [(s', args)], where [s'] --- residual stream, 
    [args name] --- matched value for argument [name] (empty list for non-captured or
    non-matched names).
 *)
val matchAll : 'a t -> 'a Ostream.t -> ('a Ostream.t * (string -> 'a list)) Ostream.t

(** Specialized version of [matchAll] for character streams. *)
val matchAllStr : char t -> char Ostream.t -> (char Ostream.t * (string -> string)) Ostream.t

(** Results printer for matchAllStr - prints all parses. *)
val print : string list -> (char Ostream.t * (string -> string)) Ostream.t -> unit

(** Results printer for matchAllStr - prints unique parses. *)
val printUnique : string list -> (char Ostream.t * (string -> string)) Ostream.t -> unit
