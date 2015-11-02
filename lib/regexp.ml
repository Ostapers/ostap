open Printf 
open List

type 'a t = 
    Test   of string * ('a -> bool)
  | Before of 'a t
  | Aster  of 'a t
  | Plus   of 'a t
  | Opt    of 'a t
  | Alter  of 'a t list
  | Juxt   of 'a t list
  | Bind   of string * 'a t
  | Arg    of string 
  | EOS

let rec fold f x e =
  let foldF = fold f in
  let x     = f x e in
  match e with
  | Before t | Aster t | Plus t | Opt t | Bind (_, t) -> foldF x t
  | Alter tl | Juxt tl -> fold_left (fun x t -> foldF x t) x tl 
  | _ -> x

let rec toText = 
  let ttl t   = Pretty.listByComma (List.map toText t) in
  let ptt n t = Pretty.plock (Pretty.string n) t in
  function
  | Test  (s, _) -> ptt "Test"    (Pretty.string s)
  | Before t     -> ptt "Before"  (toText t)
  | Aster  t     -> ptt "Aster"   (toText t)
  | Plus   t     -> ptt "Plus"    (toText t)
  | Opt    t     -> ptt "Opt"     (toText t)
  | Alter  tl    -> ptt "Alter"   (ttl    tl)
  | Juxt   tl    -> ptt "Juxt"    (ttl    tl)
  | Bind  (s, t) -> ptt "Bind"    (Pretty.seq  [Pretty.string s; toText t])
  | Arg    s     -> Pretty.string (sprintf "Arg (%s)" s)
  | EOS          -> Pretty.string "EOS"

let toString s = Pretty.toString (toText s)

module Diagram =
  struct

    type 'a expr = 'a t

    module SS = Set.Make (String)
    
    type 'a cond = If of string * ('a -> bool) | Ref of string | Lookahead of 'a t | EoS 
    and  'a tran = 'a cond * SS.t * 'a node
    and  'a node = {mutable final: bool; mutable transitions: 'a tran list; id: int}
    and  'a t    = 'a node list * string list * int

    let nnodes (_, _, n) = n
    let args   (_, n, _) = n
    let roots  (n, _, _) = n

    let getDest (_, _, x) = x

    module Compiled =
      struct

        module M = Map.Make (Compare.String)

        let empty       = M.empty
        let funOf m     = (fun name -> try rev (M.find name m) with Not_found -> [])
        let bind  n x m = try M.add n (x :: (M.find n m)) m with Not_found -> M.add n [x] m

        type 'a bnds    = 'a list M.t
        type 'a diagram = 'a t
        type 'a state   = 
           {
            eos       : int list; 
            symbol    : 'a -> 'a bnds -> (int * 'a bnds) list; 
            lookaheads: ('a t * int) list; 
            args      : (string * string list * int) list
           }
        and 'a t = {states : 'a state array; start: int list; ok : int list}

        let rec make ((nodes, _, num) : 'a diagram) = 
          let empty    = {eos = []; symbol = (fun _ _ -> []); lookaheads = []; args = []} in
          let ok       = ref [] in
          let t        = Array.init num (fun _ -> empty) in
          let filled   = Array.make num false            in
          let module S = Set.Make (Compare.Integer)      in
          let elems  s = S.fold (fun x l -> x :: l) s [] in
          let rec inner node =
            if not filled.(node.id) then 
            begin
              filled.(node.id) <- true;
              if node.final then ok := node.id :: !ok;
              let eos, trans, lkhds, args =
                fold_left 
                  (fun (eos, trans, lkhds, args) (cond, binds, dst) -> 
                     let addDst = S.add (dst.id) in
                     inner dst;
                     match cond with
                     | If (_, f)   -> eos, (f, binds, dst.id) :: trans, lkhds, args
                     | Ref       s -> eos, trans, lkhds, (s, SS.elements binds, dst.id) :: args
                     | Lookahead t -> eos, trans, (make t, dst.id) :: lkhds, args 
                     | EoS         -> addDst eos, trans, lkhds, args
                  ) 
                  (S.empty, [], [], [])
                  node.transitions
               in
               let trans a m =
                 flatten (map (fun (f, binds, dst) -> if f a then [dst, SS.fold (fun n m -> bind n a m) binds m] else []) trans)
               in
               t.(node.id) <- {eos = elems eos; symbol = trans; lookaheads = lkhds; args = args}
            end
          in
          List.iter inner nodes;
          {states = t; start = List.map (fun node -> node.id) nodes; ok = !ok}
          
        let rec matchStream t s =
          let rec inner = function
          | (i, s, m) :: context ->
(*              LOG[traceNFA] (printf "state: %d, stream: %s\n" i (Ostream.takeStr 10 s));*)
              let result = 
                try  
                  ignore (List.find (fun j -> i = j) t.ok); 
                  Some (s, funOf m)
                with Not_found -> None
              in
              let state    = t.states.(i) in                  
              let context' =
                let lkhds = 
                  fold_left (fun acc (t, i) -> if Ostream.endOf (matchStream t s) then acc else (i, s, m) :: acc) 
                  [] 
                  state.lookaheads 
                in
                let args =
                  fold_left 
                    (fun acc (arg, binds, i) -> 
                       let p     = funOf m arg in 
                       let s', n = Ostream.eqPrefix p s in 
(*
                       LOG[traceNFA] (
                         let module S = View.List (View.Char) in
                         printf "Matching argument: %s\n" arg;
                         printf "Value: %s\n" (S.toString (Obj.magic p));
                         printf "Stream: %s\n" (S.toString (Obj.magic (Ostream.take 10 s)));
                         printf "Matched symbols: %d\n" n;
                         printf "Residual stream: %s\n" (S.toString (Obj.magic (Ostream.take 10 s')))
                       );
*)
                       if n = List.length p 
                       then 
                         let m' = List.fold_left (fun m name -> List.fold_right (fun x m -> bind name x m) p m) m binds in
                         (i, s', m') :: acc 
                       else acc
                    ) 
                    [] 
                    state.args                     
                in
                let eos = 
                  try
                    let a, s' = Ostream.get s in
                    map (fun (i, m) -> i, s', m) (state.symbol a m)
                  with End_of_file -> map (fun i -> i, s, m) state.eos                     
                in
                lkhds @ args @ eos @ context                  
              in
(*
              LOG[traceNFA](
                printf "next states: ";
                List.iter (fun (i, _, _) -> printf "%d " i) context';
                printf "\n"
              );
*)
              (match result with None -> inner context' | Some r -> r, context')
               
          | [] -> raise End_of_file
          in
          let makeOne i = Ostream.fromIterator [i, s, empty] inner in
          List.fold_left (fun acc i -> Ostream.concat acc (makeOne i)) (makeOne (hd t.start)) (tl t.start)

      end

    let toDOT (r, _, _) =
      let clusterId =
        let counter = ref 0 in
        (fun () -> 
           let id = !counter in
           incr counter;
           id
        )
      in
      let rec toDOT prefix p =
        let module S = Set.Make (Compare.Integer) in
        let buf = Buffer.create 512 in
        let node id label = Buffer.add_string buf (sprintf "node_%s_%d [label=\"id=%d, %s\"];\n" prefix id id label) in 
        let edge id = 
          let doit t l =
            let inDOT i j label =
              Buffer.add_string buf (sprintf "node_%s_%d -> node_%s_%d [label=\"%s\"];\n" prefix i prefix j label)
            in
            inDOT id t.id l
          in
          let bindings b =
            let module L = View.List (View.String) in
            L.toString (SS.elements b)
          in
          function
          | If (s, _)  , bs, t -> doit t (sprintf "if(%s)[%s]"  s (bindings bs))
          | Ref s      , bs, t -> doit t (sprintf "ref(%s)[%s]" s (bindings bs)) 
          | EoS        , _ , t -> doit t "EoS" 
          | Lookahead x, _ , t ->
              let r        = roots x                    in
              let cId      = clusterId ()               in
              let prefix'  = sprintf "%s_%d" prefix cId in
              Buffer.add_string buf (sprintf "subgraph cluster_%d {\n" cId);
              Buffer.add_string buf (sprintf "  label=\"lookahead\";\n");
              let str, oks = toDOT prefix' r in
              Buffer.add_string buf str;
              Buffer.add_string buf "}\n";
              List.iter (fun r -> Buffer.add_string buf (sprintf "node_%s_%d -> node_%s_%d;\n" prefix id prefix' r.id)) r;
              List.iter (
                 fun ok -> 
                   Buffer.add_string buf (sprintf "node_%s_%d -> node_%s_%d;\n" prefix' ok prefix t.id)
                 )
                 oks
        in
        let rec inner start nd ((visited, oks) as context) =
          if S.mem nd.id visited 
          then context
          else
            let (visited', _) as context' = S.add nd.id visited, oks in
            node nd.id (sprintf "state (%s)" ((if start then "start, " else "") ^ if nd.final then "final" else "non-final"));
            fold_left (fun acc tran -> edge nd.id tran; inner false (getDest tran) acc) (visited', if nd.final then nd.id :: oks else oks) nd.transitions
        in
        let _, oks = fold_left (fun ctxt p -> inner true p ctxt) (S.empty, []) p in
        Buffer.contents buf, oks
      in
      sprintf "digraph X {\n%s\n}\n" (fst (toDOT "" r))
    
    let rec make expr =
      let checkName, getBindings =
        let names = ref SS.empty in
        (fun name -> 
          names := SS.add name !names; 
          name
        ),
        (fun () ->
          SS.fold (fun x l -> x :: l) !names []
        )
      in      
      let eliminateBindings expr = 
        let rec inner lookahead scoped binds = function
        | Aster  t     -> `Aster  (inner lookahead scoped binds t)
        | Before t     -> `Before  t
        | Plus   t     -> `Plus   (inner lookahead scoped binds t)
        | Opt    t     -> `Opt    (inner lookahead scoped binds t)
        | Alter  tl    -> `Alter  (map (inner lookahead scoped binds) tl)
        | Juxt   tl    -> `Juxt   (map (inner lookahead scoped binds) tl)
        | Bind  (s, t) -> inner lookahead (s :: scoped) (SS.add (checkName s) binds) t

        | Arg s -> 
           begin try 
             ignore (find (fun x -> s = x) scoped); 
             raise (Failure (sprintf "binding \"%s\" is used within capturing expression" s)) 
           with Not_found -> `Arg (s, binds) end

        | Test  (s, f) -> `Test (s, f, binds)
        | EOS          -> `EOS
        in
        inner false [] SS.empty expr
      in
      let rec simplify = function
        | `Opt    t -> (match simplify t with `Opt t -> `Opt t | `Aster t -> `Aster t | `Plus t -> `Aster t | t -> `Opt t)
        | `Aster  t -> (match simplify t with `Aster t | `Plus t | `Opt t | t -> `Aster t)
        | `Plus   t -> (match simplify t with `Plus t -> `Plus t | `Aster t | `Opt t -> `Aster t | t -> `Plus t)
        | `Before t -> `Before t
        | `Juxt   tl -> 
           (match
              flatten (
                map 
                  (fun t -> 
                     match simplify t with
                     | `Juxt tl -> tl
                     | t        -> [t]
                  ) 
                  tl
              )
             with
             | [t] -> t
             | tl  -> `Juxt tl
           )
        | `Alter tl -> 
           (match
              flatten (
                map 
                  (fun t -> 
                     match simplify t with
                     | `Alter tl -> tl
                     | t         -> [t]
                  ) 
                  tl
              )
            with
            | [t] -> t
            | tl  -> `Alter tl
           )
        | `Arg s  -> `Arg s
        | `EOS    -> `EOS
        | `Test x -> `Test x
      in
      let id =
        let i = ref 0 in
        fun () -> 
          let j = ! i in
          incr i;
          j
      in
      let make_node trans = {final=false; transitions=trans; id=id ()} in
      let transitions nodes = flatten (map (fun node -> node.transitions) nodes) in
      let append nodes transitions = iter (fun node -> node.transitions <- node.transitions @ transitions) nodes in
      let rec inner = function
      | `Aster t -> 
         let sn, en, s_en = inner t in
         append en (transitions (sn @ s_en));
         (*append s_en (transitions sn);*)
         [], en, sn @ s_en
      
      | `Test (s, t, bs) -> 
         let end_node = make_node [] in
         let start_node = make_node [If (s, t), bs, end_node] in
         [start_node], [end_node], []

      | `Arg (s, bs)  -> 
         let end_node = make_node [] in
         let start_node = make_node [Ref s, bs, end_node] in
         [start_node], [end_node], []         

      | `EOS -> 
         let end_node = make_node [] in
         let start_node = make_node [EoS, SS.empty, end_node] in
         [start_node], [end_node], []         

      | `Plus t -> inner (`Juxt [t; `Aster t])
      | `Opt t -> 
          let sn, en, s_en = inner t in
          [], en, sn @ s_en

      | `Alter tl -> 
          fold_left 
            (fun (x, y, z) t -> 
               let sn, en, s_en = inner t in 
               sn @ x, en @ y, s_en @ z
            ) 
            ([], [], []) 
            tl
         
      | `Juxt (t::tl) -> 
         let juxt2 (sn, en, s_en) (sn', en', s_en') = 
           let ends = transitions (sn' @ s_en') in
           let is_end = s_en' != [] in
           append en ends;
           append s_en ends;
           if is_end 
           then sn, en @ en' @ s_en', s_en 
           else sn @ s_en, en' @ s_en', []
         in
         fold_left (fun acc t -> juxt2 acc (inner t)) (inner t) tl

      | `Before t -> 
         let end_node = make_node [] in
         let start_node = make_node [Lookahead (make t), SS.empty, end_node] in
         [start_node], [end_node], []
      in        
      let sn, en, s_en = inner (simplify (eliminateBindings expr)) in
      iter (fun a -> a.final <- true) (s_en @ en);
      (sn @ s_en, getBindings (), id ())

  end

module ASCII =
  struct

    type expr = ASCII.t t

    let getOf a =
      let module M = Map.Make (struct type t = char include Pervasives end) in
      let m = 
        Array.fold_left 
          (fun m (c, x) ->
             if M.mem c m 
             then invalid_arg (sprintf "Ostap.Regexp.ASCII: internal error in initialization: duplicate character class '%c'" c)
             else M.add c x m
          )
          M.empty
          a
      in
      (fun c -> try Some (M.find c m) with Not_found -> None) 

    open ASCII

    let ofChar  c x = x = c 
    let ofClass c x = Class.isIn c (Class.get x)
    
    let getClass = 
      getOf [|        
(*
        '$', Alter [EOS; Before (Test ("n", fun c -> c = '\n'))];
        '%', EOS;
*)
        '.', (fun c -> c != '\n');
        'n', (ofChar '\n');
        'r', (ofChar '\r');
        't', (ofChar '\t');
        'a', (ofChar '\x07');
        'e', (ofChar '\x1B');
        'f', (ofChar '\x0C');
        'v', (ofChar '\x0B');

        'P', (ofClass Class._PRINTABLE );
        'C', (ofClass Class._CONTROL   );
        'E', (ofClass Class._EXTENDED  );
        'O', (ofClass Class._OTHER     );

        'u', (ofClass Class._ULETTER   );
        'l', (ofClass Class._LLETTER   );
        'd', (ofClass Class._DDIGIT    );
        'w', (ofClass Class._WORD      );
        'b', (ofClass Class._BDIGIT    );
        'o', (ofClass Class._ODIGIT    );
        'x', (ofClass Class._HDIGIT    );
        'p', (ofClass Class._PUNCTUATOR);

        'B', (ofClass Class._BRACKET   );
        'H', (ofClass Class._LBRACKET  );
        'K', (ofClass Class._RBRACKET  );
        'A', (ofClass Class._ARITHMETIC);
        'R', (ofClass Class._RELATION  );
        'L', (ofClass Class._LOGIC     );
        'Q', (ofClass Class._QUOTE     )
      |]
          
    let range    = ASCII.range
    let nonrange = ASCII.nonrange
    let oneOf    = ASCII.oneOf

    exception Reason of int * string
(*
    let make s =
      let explain comment f s =
        let (_, pos), _ = Stream.get s in
        try f s with End_of_file -> raise (Reason (pos, comment))
      in
      let pos  s =
        let (_, pos), _ = Stream.get s in
        pos
      in
      let next s = 
        let (c, pos), s' = Stream.get s in
        c, s'
      in
      let rec ground s =
        let makeExpr c =
          match getClass c with
          | Some t -> `T t
          | None   -> `C c
        in        
        explain 
           "unterminated escape sequence or interval" 
           (fun s ->
              match next s with
              | '\\', s' -> let c, s'' = next s' in makeExpr c, s''
              | '[' , s' ->
                 let rec inner acc s =
                   let t, s' = interval s in
                   let acc x = (acc x) or (t x) in
                   match next s' with
                   | ']', s'' -> acc, s''
                   | _        -> inner acc s'
                 in
                 let t, s'' = inner (fun _ -> false) s' in
                 (`T t), s''
                 
              |  c  , s' -> makeExpr c, s'
           )
           s
      and interval s =
        let charOf s = function
        | `T e -> raise (Reason (pos s, "character class in range expression"))
        | `C c -> c          
        in
        let e, s' = ground s in
        match next s' with
        | '-', s'' -> 
           let c        = charOf s e in
           let e', s''' = ground s'' in
           range c (charOf s'' e'), s''' 
          
        | _ -> (match e with `T e -> e | `C c -> ofChar c), s'
      and primary s =
        match next s with
        | '^', s' ->
        | '.', s' ->
        | '%', s' ->
        | '$', s' ->
        | _        -> 
           let t, s' = ground s in
           Test ("ground", t), s'
     
      
      let _ = Stream.zip (Stream.fromString s) (Stream.from 1) in
      ()
           
*)       

  end

let matchAll expr str =
  Diagram.Compiled.matchStream (Diagram.Compiled.make (Diagram.make expr)) str

let matchAllStr expr str = 
  let module S = View.ListC (struct let concat = (^) end) (View.Char) in
  Ostream.map (fun (s, args) -> s, (fun name -> S.toString (args name))) (matchAll expr str)

let streamStateToText names (s, b) =
   let module S = View.List (View.String) in
   let rest s = sprintf "%s..." (Ostream.takeStr 10 s) in 
      sprintf "  stream: %s;\n  args  : %s\n" 
         (rest s) 
         (S.toString (List.map (fun n -> sprintf "%s=[%s]" n (b n)) names))

let printUnique names s =
  let module SS = Set.Make(String) in
  let results =  
    Ostream.fold 
      (fun res s -> SS.add (streamStateToText names s) res)
      SS.empty
      s
  in SS.iter (printf "%s") results
  
let print names s =
   Ostream.iter (fun s -> printf "%s" (streamStateToText names s)) s
