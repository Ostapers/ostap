open Printf

module type StreamChar =
   sig
      type t
      val compare : t -> t -> int
      val values  : t list
      val toInt   : t -> int
      val ofInt   : int -> t
      val toString: t -> string
      val max     : int
   end

module ASCIIStreamChar =
   struct
      type t = char

      let compare = compare

      let values = Ostream.take 256 (Ostream.map char_of_int (Ostream.range 0 255))

      let toInt = int_of_char
      let ofInt = char_of_int
      let toString = function
         | '"' -> "\\\""
         | c -> if ASCII.Class.isIn ASCII.Class._PRINTABLE (ASCII.Class.get c)
                then sprintf "%c" c
                else sprintf "\%d" (toInt c)
      let max = List.length values
      
      open Regexp
(*
      primary: sym | range | "\(" main "\)"
      juxt: (primary postfix?)+
      main: juxt ('\|' main)*
*)
      exception Nomatch
      
      let regexp s =
         let l = String.length s in
         let rec main i =
            match alter i with
            | [res], j -> res, j
            | l, j -> Alter l, j
         and alter i =
            let res, j = juxt i in
            let res = match res with
               | [res] -> res
               | l -> Juxt l
            in
            if j+2 < l && s.[j] = '\\' && s.[j+1] = '|'
            then let inner, j = alter (j+2) in res::inner, j
            else [res], j
         and juxt i =
            let prim, j = primary i in
            let res, j =
               if j >= l then prim, j
               else
               match s.[j] with
               | '*' -> Aster prim, (j+1)
               | '+' -> Plus  prim, (j+1)
               | '?' -> Opt   prim, (j+1)
               | _   ->       prim,  j
            in try
               let inner, j' = juxt j in
               res::inner, j'
            with Nomatch -> [res], j
         and primary i =
            if i >= l
            then raise Nomatch
            else
            let i1 = i + 1 in
            match s.[i] with
            | '\\' ->
               begin
               let i2 = i1 + 1 in
               match s.[i1] with
               | '|'
               | ')' -> raise Nomatch
               | '$' | '^' | '.' | '*' | '+' | '?' | '[' | ']' as c -> Test (sprintf "%c" c, fun x -> x = c), i2
               | 'b' -> Alter [Before (Test ("boundary", fun x -> ('a' > x || 'z' < x) && ('A' > x || 'Z' < x) && ('0' > x || '9' < x) && x <> '_')); EOS], i2
               | '(' -> let group, j = main i2 in group, (j+2) (* assume each group successfully ends with \) *)
               end
            | '.' -> Test ("any", fun _ -> true), i1
            | '[' ->
               let rec inner acc i =
                  let f, j = interval i in
                  let acc x = acc x || f x in
                  match s.[j] with
                  | ']' -> acc, (j+1)
                  | _ -> inner acc j
               and interval i =
                  match s.[i+1] with
                  | '-' -> (fun x -> s.[i] <= x && x <= s.[i+2]), (i+3)
                  | _ -> (fun x -> x = s.[i]), (i+1)
               in 
               let f, j = inner (fun _ -> false) (i+1) in
               Test (String.sub s (i+1) (j-i-2), f), j
            | '^' -> invalid_arg "ASCIIStreamChar.primary"
            | '$' -> Alter [Before (Test ("\\n", fun x -> x = '\n')); EOS], i1
            | c -> Test (sprintf "%c" c, fun x -> x = c), i1
         in
         fst (main 0)
   end

module DetNFA (C : StreamChar) =
   struct
      open Regexp.Diagram
      module MI = Map.Make(Compare.Integer) (* to get old states from new states *)
      module M = Map.Make (Compare.String)
      module S = SS
      module SI = Set.Make (Compare.Integer)
      module MSI = Map.Make(SI) (* to get new states from old states *)

      type bnds  = C.t list M.t
      type state = 
         {
          eos       : int option; 
          lookaheads: (t * int) list; 
          args      : (int * S.t) M.t;
          symbols   : (int * S.t) array;
          final     : bool
         }
      and t = {states : state array} (* start always from 0 *)
      
      module MD = Map.Make(struct type a = t type t = a let compare = Pervasives.compare end)

      let rec make ((nodes, _, num) : C.t Regexp.Diagram.t) =
         let ok       = ref [] in
         let created  = ref 0 in
         let cur      = ref 0 in
         let oldToNew = ref MSI.empty in
         let newToOld = ref MI.empty in
         let table    = ref [] in
         let getIds lst = List.fold_left (fun set node -> SI.add node.id set) SI.empty lst in
         let setId lst =
(*
            let module VN = struct type t = C.t Regexp.Diagram.node let toString = fun node -> string_of_int node.id end in
            let module VLN = View.List(VN) in
            printf "setting #%d to %s\n" !created (VLN.toString lst);
*)
            newToOld := MI.add !created lst !newToOld;
            oldToNew := MSI.add (getIds lst) !created !oldToNew;
            created := !created + 1;
            !created - 1
         in
         ignore (setId nodes);
         while MI.mem !cur !newToOld do
            let oldStates = MI.find !cur !newToOld in (* Diagram.node list *)
            let trans, refs, eos, lookaheads =
               List.fold_left 
               (fun acc node ->
                  List.fold_left
                     (fun (trans, refs, eos, lookaheads) (cond, bnds, node) ->
                        match cond with
                        | If (_, f) ->
                           List.iter
                              (fun c ->
                                 if f c then
                                    let states, binds = trans.(C.toInt c) in
                                    trans.(C.toInt c) <- node::states, S.union binds bnds
                              )
                              C.values;
                           trans, refs, eos, lookaheads
                        | Ref arg ->
                           let states, binds = try M.find arg refs with Not_found -> [], S.empty in
                           trans, M.add arg (node::states, S.union binds bnds) refs, eos, lookaheads
                        | EoS -> trans, refs, node::eos, lookaheads
                        | Lookahead t ->
                           let t = make t in
                           let states = try MD.find t lookaheads with Not_found -> [] in
                           trans, refs, eos, MD.add t (node::states) lookaheads
                     )
                     acc
                     node.transitions
               )
               (Array.make C.max ([], S.empty), M.empty, [], MD.empty)
               oldStates
            in
            let toId states = try MSI.find (getIds states) !oldToNew with Not_found -> setId states in
            let update (states, binds) =
               (match states with
               | [] -> -1
               | _ -> toId states
               ), binds
            in
            table :=
               {symbols = Array.map update trans;
                args = M.map update refs;
                lookaheads = MD.fold (fun t states acc -> if states = [] then acc else (t, toId states)::acc) lookaheads [];
                final = List.exists (fun node -> node.Regexp.Diagram.final) oldStates;
                eos = match eos with [] -> None | _ -> Some (toId eos)
               } :: !table;
            cur := !cur + 1
         done;
         {states = Array.of_list (List.rev !table)}

      let rec toDiagram reverse t =
         let l = Array.length t.states in
         let ok = ref [] in
         let res = Array.init l (fun i -> {Regexp.Diagram.final = (if reverse then i = 0 else t.states.(i).final); transitions = []; id = i}) in
         Array.iteri
            (fun beg_id state ->
               let updTrans (end_id, binds) cond =
                  let b, e = if reverse then end_id, beg_id else beg_id, end_id in
                  res.(b).transitions <- (cond, binds, res.(e))::res.(b).transitions in
               if reverse
               then Array.iteri
                  (fun i (end_id, binds as arrow) ->
                     if end_id >= 0 then
                     updTrans arrow (If (sprintf "%s" (C.toString (C.ofInt i)), fun c -> c = C.ofInt i))
                  )
                  state.symbols
               else begin
                  let module P = Compare.Pair(Compare.Integer)(S) in
                  let module MIS = Map.Make(P) in
                  let update arrow (a, b) acc =
                     if fst arrow >= 0
                     then
                        let (name, func) = try MIS.find arrow acc with Not_found -> ("", fun c -> false) in
                        let a, b = C.ofInt a, C.ofInt b in
                        let name = if name = "" then "" else sprintf "%s," name in
                        let newval =
                           if C.compare a b = 0
                           then name ^ C.toString a, fun c -> C.compare c a = 0 || func c
                           else sprintf "%s%s-%s" name (C.toString a) (C.toString b), fun c -> C.compare a c <= 0 || C.compare c b <= 0 || func c
                        in
                        MIS.add arrow newval acc
                     else acc
                  in
                  let _, last_arrow, (last_from, arrows) =
                     Array.fold_left
                        (fun (i, prev, (from, acc)) (end_id, _ as arrow) ->
                           i + 1,
                           arrow,
                           (if P.compare prev arrow != 0
                           then i, update prev (from, i - 1) acc
                           else from, acc)
                        )
                        (0, ((-2), S.empty), (0, MIS.empty))
                        state.symbols
                  in
                  let arrows = update last_arrow (last_from, C.max - 1) arrows in
                  MIS.iter (fun arrow (name, func) -> updTrans arrow (If (name, func))) arrows
               end;
               M.iter (fun arg arrow -> updTrans arrow (Ref arg)) state.args;
               (match state.eos with None -> () | Some end_id -> updTrans (end_id, S.empty) EoS);
               List.iter
                  (fun (t, end_id) -> updTrans (end_id, S.empty) (Lookahead (toDiagram reverse t)))
                  state.lookaheads;
               if state.final then ok := beg_id :: !ok
            )
            t.states;
         List.map (Array.get res) (if reverse then !ok else [0]), [], l

      let toDOT t = Regexp.Diagram.toDOT (toDiagram false t)

      let printTable t =
         let lkhdnum = ref 0 in
         let lkhds = Hashtbl.create 10 in
         let getNum t = try Hashtbl.find lkhds t with Not_found -> Hashtbl.add lkhds t !lkhdnum; lkhdnum := !lkhdnum + 1; !lkhdnum - 1 in
         Array.iteri
         (fun i state ->
            printf "%d%c: " i (if state.final then 'f' else ' ');
            let module VSS = View.Set(S)(View.String) in
            Array.iteri (fun i (id, binds) -> if id >= 0 then printf "'%s' -> %d, %s " (C.toString (C.ofInt i)) id (VSS.toString binds)) state.symbols;
            M.iter (fun arg    (id, binds) -> if id >= 0 then printf "[%s] -> %d, %s " arg                      id (VSS.toString binds)) state.args;
            (match state.eos with None -> () | Some id ->     printf  "EoS -> %d "                              id);
            List.iter (fun (t, id) ->                         printf "#%d  -> %d "                   (getNum t) id)                      state.lookaheads;
            printf "\n"
         )
         t.states

      let minimize diag =
         let reverse = toDiagram true in
         make (reverse (let t = make (reverse (make diag)) in (*printTable t;*) t))

      let funOf : C.t list M.t -> string -> C.t list =
         fun binds name -> try List.rev (M.find name binds) with Not_found -> []
      let bind : string -> C.t -> C.t list M.t -> C.t list M.t =
         fun name sym binds -> try M.add name (sym :: (M.find name binds)) binds with Not_found -> M.add name [sym] binds
      let rec matchStream t s =
(*         let module VLC = View.List (C) in*)
         let rec inner = function
         | (i, s, m) :: context ->
(*            LOG[traceDFA] (printf "state: %d, stream: %s\n" i (VLC.toString (Ostream.take 10 s)));*)
            let state  = t.states.(i) in                  
            let context' =
               let sym_eos = 
                  try
                     let a, s' = Ostream.get s in
                     let i, args = state.symbols.(C.toInt a) in
                     if i >= 0 then
                        let m' = S.fold (fun name -> bind name a) args m in
                        [i, s', m']
                     else []
                  with End_of_file -> match state.eos with None -> [] | Some i -> [i, s, m]
               in
               let lkhds = if state.lookaheads = [] then [] else
                  List.fold_left (fun acc (t, i) -> if Ostream.endOf (matchStream t s) then acc else (i, s, m) :: acc) 
                  [] 
                  state.lookaheads 
               in
               let args = if M.is_empty state.args then [] else
                  M.fold 
                     (fun arg (i, args) acc -> 
                        let p     = funOf m arg in 
                        let s', n = Ostream.eqPrefix p s in 
(*
                        LOG[traceDFA] (
                          printf "Matching argument: %s\n" arg;
                          printf "Value: %s\n" (VLC.toString p);
                          printf "Stream: %s\n" (VLC.toString (Ostream.take 10 s));
                          printf "Matched symbols: %d\n" n;
                          printf "Residual stream: %s\n" (VLC.toString (Ostream.take 10 s'))
                        );
*)
                        if n = List.length p 
                        then 
                          let m' = S.fold (fun name -> List.fold_right (bind name) p) args m in
                          (i, s', m') :: acc 
                        else acc
                     ) 
                     state.args
                     []
               in
               sym_eos @ args @ lkhds @ context                  
            in
(*
            LOG[traceDFA](
              printf "next states: ";
              List.iter (fun (i, _, _) -> printf "%d " i) context';
              printf "\n"
            );
*)
            if state.final then ((s, funOf m), context') else inner context'

         | [] -> raise End_of_file
         in
         Ostream.fromIterator [0, s, M.empty] inner
         
   end

let matchAllStr diag str = 
  let module VLC = View.ListC (struct let concat = (^) end) (View.Char) in
  let module D = DetNFA(ASCIIStreamChar) in
  Ostream.map (fun (s, args) -> s, (fun name -> VLC.toString (args name))) (D.matchStream (D.minimize diag) str)

