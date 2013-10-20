(*
 * Ostap: a common set of parser combinators.
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

(** Ostap --- a general set of parser combinators. *)

(** The name of this library originates from {{:http://en.wikipedia.org/wiki/Ostap_Bender} Ostap Bender}
    --- the central character of Ilya Ilf and Eugene Petrov comedy "The Twelve Chairs". Bender is
    generally referred to as "The Great Combinator" since the word "combinator" in Russian also means 
    "a swindler", "a sly man" etc.
 *)

(** {2 Main parsing types } *)

(** Type pattern for the result of parsing. Here ['a] --- type of {i parsed value}, ['b] --- type of
    {i failure reason} (description of parse problem). Result is 

    {ul {- either a parsed value coupled with optional reason designated to denote deferred errors}
        {- or a failure with optional reason.}
    }
    
    Deferred reasons are those which can be potentially signalled in the future. For example, 
    parsing the string "A, B" with the rule ("A" "B")? has to return parsed value with deferred failure
    reason "B expected".
 *)
type ('a, 'b) tag = Parsed of 'a * 'b option | Failed of 'b option

(** The type 

    {C [type ('stream, 'parsed, 'error) result = ('parsed * 'stream, 'error) tag]}

    denotes the result of parsing a stream with a parser. This result is either parsed value of type 
    ['parsed] and the residual stream of type ['stream], or failure with reason of type ['error].
 *)
type ('stream, 'parsed, 'error) result = ('parsed * 'stream, 'error) tag

(** The type 

    {C [type ('stream, 'parsed, 'error) parse  = 'stream -> ('stream, 'parsed, 'error) result]}

    corresponds to a parser. Parser takes a stream of type ['stream] and returns result.
 *)
type ('stream, 'parsed, 'error) parse  = 'stream -> ('stream, 'parsed, 'error) result

(** {2 Simple predefined parsers} *)

(** [empty] successfully consumes no items from the stream. *)
val empty : ('a, unit, 'b) parse

(** [fail r s] consumes no items from the stream [s] and always returns failure with reason [r]. *)
val fail : 'b option -> ('a, unit, 'b) parse

(** [lift s] returns [Parsed (s, s)] and so "lifts" the stream [s] as a successful parse result. *)
val lift : ('a, 'a, 'b) parse

(** {2 General parsing combinators} *)

(** [map f p] applies [f] to the result of [p], if [p] succeeded, or fails otherwise. *)
val map : ('b -> 'c) -> ('a, 'b, 'd) parse -> ('a, 'c, 'd) parse 

(** Infix synonym for [map]. Note: the order of parameters is inverted. *)
val (-->) : ('a, 'b, 'd) parse -> ('b -> 'c) -> ('a, 'c, 'd) parse

(** [sink p] returns parser which replaces the residual stream with successfully 
    parsed by [p] value; [sink] is a sort of "inversion" of [lift].
 *)
val sink : ('a, 'a, 'c) parse -> ('a, 'a, 'c) parse

(** Sequence combinator. [seq x y] constructs a parser to parse 
    successively by [x] and [y]. Parsed by [x] value is passed to [y] as a context.
    The reason value type has to supply a method [add] to add one reason value to
    another to collect multiple reasons.
 *)
val seq : ('a, 'b, <add: 'e -> 'e; ..> as 'e) parse -> ('b -> ('a, 'c,  'e) parse) -> ('a, 'c, 'e) parse

(** Infix synonym for [seq]. *)
val (|>)  : ('a, 'b, <add: 'e -> 'e; ..> as 'e) parse -> ('b -> ('a, 'c, 'e) parse) -> ('a, 'c, 'e) parse
 
(** Alternative combinator. [alt x y] returns parse function that parses that that 
    either [x] or [y] parse. [alt x y] tries [y] even if [x] returned [Error].
    The reason value type has to supply a method [add] to add one reason value to
    another to collect multiple reasons.
 *)
val alt : ('a, 'b, <add: 'e -> 'e; ..> as 'e) parse -> ('a, 'b, 'e) parse -> ('a, 'b, 'e) parse

(** Infix synonym for [alt]. *)
val (<|>) : ('a, 'b, <add: 'e -> 'e; ..> as 'e) parse -> ('a, 'b, 'e) parse -> ('a, 'b, 'e) parse

(** Optional combinator. [opt x] returns parser that parses either [x] or nothing. *)
val opt : ('a, 'b, <add: 'e -> 'e; ..> as 'e) parse -> ('a, 'b option, 'e) parse

(** <?> is infix synonym for [opt]. *)
val (<?>) : ('a, 'b, <add: 'e -> 'e; ..> as 'e) parse -> ('a, 'b option, 'e) parse

(** [manyFold] is a generalization of Kleene closure combinator (zero-or-more iteration).
    [manyFold f init p] parses the input stream with parser [p] zero-or-more times eagerly
    and returns the folded value [f .. (f (f init r0) r1) .. rk] where [ri] is the result
    of i-th parsing with [p]. The reason value type has to supply a method [add] to add one 
    reason value to another to collect multiple reasons.
 *)
val manyFold : ('c -> 'b -> 'c) -> 'c -> ('a, 'b, <add: 'e -> 'e; ..> as 'e) parse -> ('a, 'c, 'e) parse

(** [many p] is a shortcut for [(manyFold (fun acc x -> (fun l -> acc (x :: l))) (fun x -> x) p) --> (fun x -> x [])].
    In short, it returns the list of successive parsings with [p].
 *)
val many : ('a, 'b, <add: 'e -> 'e; ..> as 'e) parse -> ('a, 'b list, 'e) parse

(** [someFold p] is similar to [manyFold p] with the exception that it performs one-or-more iteraton of [p]. *)
val someFold : ('c -> 'b -> 'c) -> 'c -> ('a, 'b, <add: 'e -> 'e; ..> as 'e) parse -> ('a, 'c, 'e) parse

(** [some p]  is similar to [someFold p] with the exception that it performs one-or-more iteraton of [p]. *)
val some : ('a, 'b, <add: 'e -> 'e; ..> as 'e) parse -> ('a, 'b list, 'e) parse

(** Infix synonym for [many]. *)
val (<*>) : ('a, 'b, <add: 'e -> 'e; ..> as 'e) parse -> ('a, 'b list, 'e) parse

(** Infix synonym for [some]. *)
val (<+>) : ('a, 'b, <add: 'e -> 'e; ..> as 'e) parse -> ('a, 'b list, 'e) parse

(** Guarded parser combinator. [guard p predicate r] checks successfully parsed by [p] value 
    against [predicate] and turns it into [Error r] if this check failed.
 *)    
val guard : ('a, 'b, 'c) parse -> ('b -> bool) -> ('b -> 'c) option -> ('a, 'b, 'c) parse

(** Commenting combinator: adds a readable comment to a reason.
    The reason value type has to supply a method [comment] to add string comment to the existing 
    reason value.
  *)
val comment : ('a, 'b, <comment: string -> 'c; ..> as 'c) parse -> string -> ('a, 'b, 'c) parse

(** Alternates list of parsers (equivalent to [List.fold_left alt (fail None)]). *)
val altl : ('a, 'b, <add: 'c -> 'c; ..>  as 'c) parse list -> ('a, 'b, 'c) parse

(** [unwrap r parsed failed] unwraps parse result [r] by applying either [parsed] function to
    parsed value or [failed] function to optional reason value.
 *)
val unwrap : ('stream, 'parsed, 'error) result -> ('parsed -> 'a) -> ('error option -> 'a) -> 'a