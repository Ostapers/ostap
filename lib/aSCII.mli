(*
 * ASCII: ASCII characters and operations.
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

(** ASCII character set operations. *)

(** {2 General description} *)

(** ASCII character sorted out by classes. *)
type t =
    NULL             
  | BELL             
  | BACKSPACE
  | TAB
  | LF
  | FF
  | CR
  | ESC
  | DEL        
  | CNTRL of char (** Other characters from '\x00' --- '\x19' *)
  | CHAR  of char (** Regular character '\x20' --- '\xE7'     *)
  | EXT   of char (** Other characters                        *)

(** ASCII character classes. *)
module Class :
  sig

    (** Type of class. *)
    type t 

    (** Type of set of classes (many characters are of several classes simultaneously). *)
    type set

    (** [isIn cls set] tests whether [cls] belongs to [set]. *)
    val isIn : t -> set -> bool

    (** {2 Predefined character classes} *)
 
    (** Printable characters: '\x20' --- '\xE7'. *)
    val _PRINTABLE : t

    (** Control characters: '\x00' --- '\x19'. *)
    val _CONTROL : t

    (** Extended characters: '\xE8' --- '\xFF'. *)
    val _EXTENDED : t

    (** Uppercase letters: 'A' --- 'Z'. *)
    val _ULETTER : t

    (** Lowercase letters: 'a' --- 'z'. *)
    val _LLETTER : t

    (** Decimal digits: '0' --- '9'. *)
    val _DDIGIT : t

    (** Word characters: 'A' --- 'Z' or 'a' --- 'z' or '0' --- '9' or '_'. *)
    val _WORD : t 

    (** Binary digit: '0' --- '1'. *)
    val _BDIGIT : t

    (** Octal digit: '0' --- '7'. *)
    val _ODIGIT : t

    (** Hexadecimal digit: '0' --- '9' or 'A' --- 'F' or 'a' --- 'f'. *)
    val _HDIGIT : t

    (** Punctuator characters: one of ',', '.', '!', '?', ':', ';', '|'. *)
    val _PUNCTUATOR : t
 
    (** Bracket characters: one of '\<', '\{', '\[', '\(', '\)', '\]', '\}', '\>'. *)
    val _BRACKET : t

    (** Left brackets: one of '\<', '\{', '\[', '\('. *)
    val _LBRACKET : t

    (** Right brackets: one of '\)', '\]', '\}', '\>'. *)
    val _RBRACKET : t

    (** Arithmetic characters: one of '+', '-', '*', '/'. *)
    val _ARITHMETIC : t

    (** Relational characters: one of '<', '>', '='. *)
    val _RELATION : t

    (** Logic characters: one of '&', '^', '|', '~'. *)
    val _LOGIC : t

    (** Quote characters: one of '`', '\'', '"'. *)
    val _QUOTE : t

    (** Other characters from standard set: one of '\\', '%', '$', '#', '_', '@'. *)
    val _OTHER : t

    (** String visualizer for the class. *)
    val toString : t -> string

    (** [get c] gets the set of all classes [c] is member of. *)
    val get : char -> set

  end

(** String visualizer. *)
val toString : t -> string

(** Convertor from char. *)
val fromChar : char -> t

(** Convertor to char. *)
val toChar : t -> char

(** Converts stream of characters into stream of ASCIIs. *)
val asciiStream : char Ostream.t -> t Ostream.t

(** [range x y c] checks whether character [c] is within the range [[x..y]]. *)
val range : char -> char -> char -> bool

(** [nonrange x y c] checkes whether character [c] is outside the range [[x..y]]. *)
val nonrange : char -> char -> char -> bool

(** [oneOf s c] checks whether character [c] is one of those of string [s]. *)
val oneOf : string -> char -> bool
