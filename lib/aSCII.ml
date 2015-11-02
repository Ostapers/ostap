open Printf

type t = 
  | NULL 
  | BELL 
  | BACKSPACE 
  | TAB 
  | LF 
  | FF 
  | CR 
  | ESC 
  | DEL 
  | CNTRL of char 
  | CHAR  of char 
  | EXT   of char

let range    x y c = c >= x && c <= y 
let nonrange x y c = not (range x y c)

let oneOf s c   =
  let r = ref false in 
  String.iter (fun x -> r := !r || x = c) s;
  ! r

module Class =
  struct
 
    type t   = int
    type set = int

    let isIn x c = (x land c) > 0

    let _PRINTABLE  =      1 (* x20 -- xE7      *)
    let _CONTROL    =      2 (* x00 -- x19      *)
    let _EXTENDED   =      4 (* xE8 -- xFF      *)
    let _ULETTER    =      8 (* A-Z             *)
    let _LLETTER    =     16 (* a-z             *)
    let _DDIGIT     =     32 (* 0-9             *)
    let _WORD       =     64 (* A-Za-z0-9_      *)
    let _BDIGIT     =    128 (* 0-1             *) 
    let _ODIGIT     =    256 (* 0-7             *)
    let _HDIGIT     =    512 (* 0-9 A-F a-f     *)
    let _PUNCTUATOR =   1024 (* , . ! ? : ; |   *)
    let _BRACKET    =   2048 (* < { [ ( ) ] } > *)
    let _LBRACKET   =   4096 (* < { [ (         *)
    let _RBRACKET   =   8192 (* ) ] } >         *)   
    let _ARITHMETIC =  16384 (* + - * /         *)
    let _RELATION   =  32768 (* < > =           *)
    let _LOGIC      =  65536 (* & ^ | ~         *)
    let _QUOTE      = 131072 (* ` ' ""          *)    
    let _OTHER      = 262144 (* \ % $ # _ @     *)

    let names = [|
      "PRINTABLE";
      "CONTROL";
      "EXTENDED";
      "ULETTER";
      "LLETTER";
      "DDIGIT";
      "WORD";
      "BDIGIT";
      "ODIGIT";
      "HDIGIT";
      "PUNCTUATOR";
      "BRACKET";
      "LBRACKET";
      "RBRACKET";
      "ARITHMETIC";
      "RELATION";
      "LOGIC";
      "QUOTE";
      "OTHER"    
    |]
    
    let nClasses = Array.length names

    let toString m = 
      fst (
        Array.fold_left 
          (fun (acc, i) n -> 
             (if m land (1 lsl i) > 0 then acc ^ " " ^ n else acc), i+1
          ) 
          ("", 0) 
          names
      )
      
    let table = 
      let classTests = 
        let (||) f g = (fun c -> f c || g c) in
        [|
          range '\x20' '\xE7';
          range '\x00' '\x19';
          range '\xE8' '\xFF';
          range 'A' 'Z';
          range 'a' 'z';
          range '0' '9';
          (range '0' '9') || (range 'A' 'Z') || (range 'a' 'z') || (oneOf "_");
          range '0' '1';
          range '0' '7';
          (range '0' '9') || (range 'A' 'F') || (range 'a' 'f');
          oneOf ",.!?:;|";
          oneOf "<{[()]}>";
          oneOf "<{[(";
          oneOf ")]}>";
          oneOf "+-*/";
          oneOf "<>=";
          oneOf "&^|~";
          oneOf "`'\"";
          oneOf "\\%$#_@"
        |]
      in
      let getClasses c = fst (Array.fold_left (fun (m, i) x -> (if x c then m lor (1 lsl i) else m), i+1) (0, 0) classTests) in
      Array.init 256 (fun i -> getClasses (Char.chr i))

    let get c = table.(Char.code c)
 
  end

let toString = function
  | NULL      -> "NULL"
  | BELL      -> "BELL"
  | BACKSPACE -> "BACKSPACE"
  | TAB       -> "TAB"
  | LF        -> "LF"
  | FF        -> "FF"
  | CR        -> "CR"
  | ESC       -> "ESC"
  | DEL       -> "DEL"
  | CNTRL   c -> sprintf "CNTRL(%d)" (Char.code c)
  | CHAR    c -> sprintf "CHAR(%c)" c
  | EXT     c -> sprintf "EXT(%d)" (Char.code c)

let fromChar = function
  | '\000' -> NULL
  | '\007' -> BELL
  | '\008' -> BACKSPACE
  | '\009' -> TAB
  | '\010' -> LF
  | '\012' -> FF
  | '\013' -> CR
  | '\027' -> ESC
  | '\127' -> DEL
  | x      -> if x < '\032' then CNTRL x else if x > '\127' then EXT x else CHAR x

let toChar = function
  | NULL      -> '\000' 
  | BELL      -> '\007'
  | BACKSPACE -> '\008'
  | TAB       -> '\009'
  | LF        -> '\010'
  | FF        -> '\012'
  | CR        -> '\013'
  | ESC       -> '\027'
  | DEL       -> '\127'
  | CHAR  x 
  | EXT   x 
  | CNTRL x   -> x

let asciiStream s = Ostream.map fromChar s
