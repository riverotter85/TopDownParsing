(* CSC 2710 starter code for breadth-first top-down parser implemented in OCaml
   Author:	Martha Kosa
   Date: 	04.06.2019 *)
   
(* Modifications by Logan Davis (and YYY if you have a partner) *)

(*********************)
(* list utility functions *)

(* converts from String to list of chars *)
(* from https://caml.inria.fr/mantis/view.php?id=5367 *)
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
    exp (String.length s - 1) [];;

(* TODO: Call explode with your name as input.  If you are working in a team, call
   explode once for each team member. *)
   
(* converts from list of chars to String *)
(* single char conversion from
   https://stackoverflow.com/questions/20441263/convert-char-to-string-in-ocaml/20463186 *)
let rec to_one_string char_list =
  match char_list with
    | [] -> ""
    | h::t -> (String.make 1 h)^(to_one_string t);; (* ^ does string concatenation *)

(* TODO: Call to_one_string with the result of your call(s) to explode. *)
	
(* tail-recursive linear search in a list: -1 returned if item not found, 0-based otherwise *)
let position x the_list = 
  let rec accpos i y the_list =
    match the_list with
      | [] -> -1
      | z::t -> if y = z then i else (accpos (i+1) y t)
  in (accpos 0 x the_list);;

(* TODO: Call position with the last character of the name(s) and the result(s) of your
   call(s) to explode. *)
	
(* membership function *)
(* let rec belongs_to x list =
 *	match list with
 *		| [] -> false
 *		| y::t -> x = y || (belongs_to x t);;
*)

(* improved membership function *)
let belongs_to x the_list = (position x the_list) >= 0;;

(* TODO: Call belongs_to with a valid character and an invalid character of the names(s) and
   the result(s) of your call(s) to explode. *)

let rec list_length the_list =
  match the_list with
    | [] -> 0
    | _::t -> 1 + (list_length t);;

(* TODO: Call list_length with the result(s) of your call(s) to explode. *)

(*********************)
(* basic stack ADT in OCaml *)

let push x s = x::s;;
		
let peek (h::_) = h;;
		
let pop s =
  match s with
    | _::t -> t;;

let is_empty s = (s = []);;

let is_not_empty s = (s != []);;

let rec push_all the_list s =
  match the_list with
    | [] -> s
    | h::t -> push_all t (h::s);;
	
(*********************)

(* TODO: Call push_all with a list containing the first, middle, and last name(s) used above
   on an empty list. *)
   
let anbn_productions = [('S', "aSb"); ('S', "")];;
let anbn_grammar = (anbn_productions, ['S'], 'S');;

let anbncmdm_productions = [('S', "TU"); ('T', "aTb"); ('T', ""); ('U', "cUd"); ('U', "")];;
let anbncmdm_grammar = (anbncmdm_productions, ['S';'T';'U'], 'S');;

(* TODO: Complete (and uncomment) the below recursive function to produce a list of exploded right-hand sides
   for all rules having the given left-hand side in the productions given. *)
(* let ??? collect_all_RHS lhs productions =
  match ??? with
    | ??? -> ??? (* base case *)
    | ???::??? ->
        match ??? with
          | (???, ???) ->
              if ??? = ??? then (??? ???)::(??? ??? ???)
              else (??? ??? ???);; *)

(* test cases for collect_all_RHS with expected results in the comments.
   Uncomment for testing. *)

(* collect_all_RHS 'S' anbn_productions;; *)
(* - : char list list = [['a'; 'S'; 'b']; []] *)

(* collect_all_RHS 'T' anbn_productions;; *)
(* - : char list list = [] *)

(* collect_all_RHS 'S' anbncmdm_productions;; *)
(* - : char list list = [['T'; 'U']] *)

(* collect_all_RHS 'T' anbncmdm_productions;; *)
(* - : char list list = [['a'; 'T'; 'b']; []] *)

(* collect_all_RHS 'U' anbncmdm_productions;; *)
(* - : char list list = [['c'; 'U'; 'd']; []] *)

(* collect_all_RHS 'V' anbncmdm_productions;; *)
(* - : char list list = [] *)

(* TODO: Complete (and uncomment) the below recursive function to produce a tuple consisting of a LHS and
   an exploded RHS for each rule in a set of productions. *)
(* ??? ??? explode_all_RHS productions =
  match ??? with
    | ??? -> ???
    | ???::??? ->
        match ??? with
          | (???, ???) ->
              (???, (??? ???))::(??? ???);; *)

(* test cases for explode_all_RHS with expected results in the comments.
   Uncomment for testing. *)
   
(* explode_all_RHS anbn_productions;; *)
(* - : (char * char list) list = [('S', ['a'; 'S'; 'b']); ('S', [])] *)

(* explode_all_RHS anbncmdm_productions;; *)
(* - : (char * char list) list =
[('S', ['T'; 'U']); ('T', ['a'; 'T'; 'b']); ('T', []);
 ('U', ['c'; 'U'; 'd']); ('U', [])] *)

(* TODO: Complete (and uncomment) the below recursive functions. They return either
   true or false. *) 
(* let rec are_lists_equal list1 list2 =
  match (list1, list2) with
    | ([],[]) -> ??? (* base case *)
    | (h1::t1, h2::t2) ->
        (h1 = h2) && (??? ??? ???)
    | _ -> false;; *)

(* let rec is_prefix list1 list2 =
  match (list1, list2) with
    | ([], _) -> ??? (* base case *)
    | (h1::t1, h2::t2) ->
        (??? ??? ???) && (??? ??? ???)
    | _ -> ???;; *)

(* test cases for the previous two functions with expected results in the
   comments.  Uncomment for testing. *)	
(* are_lists_equal [] [];; *)
(* true *)
(* are_lists_equal (explode "abc") (explode "abc");; *)
(* true *)
(* are_lists_equal (explode "abc") (explode "abC");; *)
(* false *)
(* are_lists_equal (explode "abc") (explode "abd");; *)
(* false *)
(* are_lists_equal (explode "abc") (explode "abcd");; *)
(* false *)
(* are_lists_equal (explode "abcd") (explode "abc");; *)
(* false *)

(* is_prefix [] [];; *)
(* true *)
(* is_prefix [] (explode "abc");; *)
(* true *)
(* is_prefix (explode "abc") [];; *)
(* false *)
(* is_prefix (explode "abc") (explode "abcd");; *)
(* true *)
(* is_prefix (explode "abcd") (explode "abc");; *)
(* false *)
(* is_prefix (explode "abb") (explode "abcd");; *)
(* false *)
(* are_lists_equal (explode "aabb") ['a';'a';'b';'b'];; *)
(* true *)

(* This function is used to find the leftmost nonterminal in an
   exploded sentential form and produce the uAv for expansion. *)
let rec split before the_list the_set =
  match the_list with
    | [] -> (before, [], [])
    | h::t ->
        if (belongs_to h the_set) then
          (before, [h], t)
        else
          split (before@[h]) t the_set;;

(* test case for split *)		  
let (my_before, [my_leftmost], my_after) = split [] ['a'; 'a'; 'S'; 'b'; 'b'] ['S'; 'T'];;
		  
(* This function is used to build a list containing strings from a given list that are
   preceded by the before string and followed by the after string. *)		  
let rec splice_all all_list before after =
  match all_list with
    | [] -> []
    | h::t -> (before@h@after)::(splice_all t before after);;

(* This function is used to create new sentential forms uwv from uAv
   for all grammar rules of the A -> w for a given leftmost nonterminal.
   TODO: Uncomment when collect_all_RHS is completed. *)	
(* let replace_LHS_by_RHS before leftmost after rules = 
  splice_all (collect_all_RHS leftmost rules) before after;; *)

(* test case for replace_LHS_by_RHS (and splice_all indirectly).
   TODO: Uncomment when collect_all_RHS is completed. *)
(* let replaced = replace_LHS_by_RHS my_before my_leftmost my_after anbn_productions;; *)

(* Note: @ does list concatenation *)
(* TODO: Complete (and uncomment) the below recursive function which implements the
   breadth-first top-down parsing algorithm discussed in class. *)
(* let rec breadth_first_parse p rules nonterminals explored stack =
  let p_as_list = (explode p) in 
    match stack with
      | [] -> ("invalid", explored, [])
      | sentential_form::tail ->
          let sf_string = (to_one_string sentential_form) in
            if (are_lists_equal p_as_list sentential_form) then
              ("valid", explored, stack)
            else
            if sentential_form = [] then
              breadth_first_parse ??? ??? ??? (explored@[""]) ???
            else
              let (???,???,???) =
                (split [] sentential_form nonterminals) in
                match leftmost with
                  | [left] ->
                      if (is_prefix ??? p_as_list) then
                        let next_level = replace_LHS_by_RHS ??? ??? ??? ???
                        in breadth_first_parse ??? ??? ??? (explored@[sf_string]) (push_all next_level ???)
                      else
                        breadth_first_parse ??? ??? ??? (???@[???]) ???
                  | [] -> breadth_first_parse ??? ??? ??? (???@[???]) ???;; *)

(* TODO: Uncomment the below code for testing when you have finished breadth_first_parse. *)
(* let parse p (productions,nonterminals,start_symbol) =
  let (result, explored, stack) =
    breadth_first_parse p productions nonterminals [] [[start_symbol]] in
  (* empty string - Unicode for lambda *)
  (* https://stackoverflow.com/questions/33777404/how-to-create-the-lambda-char-in-ocaml *)
  let p_or_lambda = if p="" then "\xCE\xBB" else p in
    (p_or_lambda^" is "^result, explored, List.map to_one_string stack);; *)

(* let parse_yes_no_only p grammar =
  let (result, _, _) = (parse p grammar) in result;; *)

(* let num_productions_explored p grammar = 
  let (_,explored,_) = (parse p grammar) in
    list_length explored;; *)

(* parse "" anbn_grammar;; *)

(* parse_yes_no_only "" anbn_grammar;; *)

(* num_productions_explored "" anbn_grammar;; *)

(* parse "ab" anbn_grammar;; *)

(* parse_yes_no_only "ab" anbn_grammar;; *)

(* num_productions_explored "ab" anbn_grammar;; *)

(* parse "aab" anbn_grammar;; *)

(* parse_yes_no_only "aab" anbn_grammar;; *)

(* num_productions_explored "aab" anbn_grammar;; *)

(* parse "aaaaaabbbbbb" anbn_grammar;; *)

(* parse_yes_no_only "aaaaaabbbbbb" anbn_grammar;; *)

(* num_productions_explored "aaaaaabbbbbb" anbn_grammar;; *)

(* parse "aaaaaaabbbbbb" anbn_grammar;; *)

(* parse_yes_no_only "aaaaaaabbbbbb" anbn_grammar;; *)

(* num_productions_explored "aaaaaaabbbbbb" anbn_grammar;; *)

(* parse "aaaaaaabbbbbbbcccddd" anbncmdm_grammar;; *)

(* parse_yes_no_only "aaaaaaabbbbbbbcccddd" anbncmdm_grammar;; *)

(* num_productions_explored "aaaaaaabbbbbbbcccddd" anbncmdm_grammar;; *)

(* TODO: the last one. Yay!!!!!
   Develop a grammar that generates {f^i m^(2j) e^(3k)| i, j, k >=0},
   where f is your first initial, m is your middle initial, and e is
   your last initial and test it for f^3 m^8 e^9 and f^3 m^3 e^2 by
   calling parse, parse_yes_no_only, and num_productions_explored.
   Do this for each team member. *)
   















