(* http://caml.inria.fr/pub/docs/oreilly-book/ocaml-ora-book.pdf *)

let rec fact n = if n<2 then 1 else n * fact(n-1) ;;

fact 8 ;;


let succ x = x+1 ;;

let compose f g x = f(g x) ;;

compose fact succ 7 ;;


() ;; (* unit - set with element () *)
let x = (12, "October") ;;
fst x ;;
snd x ;;

(* can't fst, snd on a 3+ tuple, only 2 *)


[] ;; (* empty list *)
let x = [ 1 ; "two" ; 3 ] ;;
1 :: 2 :: [] ;;
[ 1 ] @ [ 4 ] ;;
(* List library, hd, tl *)
List.hd [ 1 ; 2 ; 3 ] ;; (* ==> 1 *)

if 3=4 then 0 else 4 ;; (* => 4 *)
(* the types of 0 and 4 must be the same *)

let x=1 and y=3 ;;
let g=4 in g+g ;;


function x -> x*x ;;
(function x -> x*x) 5 ;;
(function x -> function y -> 3*x+y) 4 5 ;;
(function (x,y) -> 3*x+y) 4 5 ;;
(fun x y -> 3*x+y) 4 5 ;;
let succ = function x -> x+1 ;;
let succ x = x+1 ;;
let g x y = 2*x + 3*y ;;


List.map ;;
let square x = string_of_int (x*x) ;;
List.map square [1; 2; 3; 4] ;;
(* ==> ["1"; "4" ...] *)
List.for_all ;;
List.for_all (function n -> n<>0) [-1;1] ;; (* ==>  true *)
List.for_all (function n -> n<>0) [-1;0;1] ;; (* ==>  false *)

(* a variable declaration is called recursive if it uses
   its own itentifier in its definition *)
let rec sigma x = if x=0 then 0 else x+sigma(x-1) ;;
let rec even n = (n<>1) && ((n=0) or (odd (n-1)))
and odd n = (n<>0) && ((n=1) or (even (n-1))) ;;
even 4 ;;
odd 5 ;;

let app = function f -> function x -> f x ;;
let id x = x ;;
app id 1 ;;


(* specify the type of an expression *)
let add (x:int) (y:int) = x+y ;;
let make_pair_int (x:int) (y:int) = x,y ;;
let compose_fn_int (f:int->int) (g:int->int) (x:int) = compose f g x;;
let nil = ([]:string list);;
'H'::nil;;

let null l = (l = []);;
let rec size l =
  if null l then 0
  else 1 + (size (List.tl l)) ;;
size [];;
size [1;2;3];;

let rec iterate n f =
  if n=0 then (function x->x)
  else compose f (iterate (n-1) f) ;;


let imply v = match v with
    (true, true) -> true
  | (true, false) -> false
  | (false, true) -> true
  | (false, false) -> true ;;

let imply v = match v with
    (true, x) -> x
  | (false, x) -> true ;;

let imply v = match v with
    (true, false) -> false
  | _ -> true ;;

let is_zero n = match n with
    0 -> true
  | _ -> false ;;

let is_a_vowel c = match c with
    'a' | 'e' | 'i' | 'o' | 'u' -> true
  | _ -> false ;;

let min_rat pr = match pr with
    ((_,0), p2) -> p2
  | (p1, (_,0)) -> p1
  | (((n1,d1) as r1), ((n2,d2) as r2)) ->
    if (n1 * d2) < (n2 * d1) then r1 else r2 ;;

let rec size x = match x with
    [] -> 0
  | _::tail_x -> 1 + (size tail_x) ;;

(*iteration over a list*)
let rec fold_left f a = function
[] -> a
  | head::tail -> fold_left f (f a head) tail ;;

let (a,b,c) = (1, true, 'A');;


(* product types for tuples or records,
   sum types for unions *)

type 'param paired_with_integer = int * 'param ;;
type specific_pair = float paired_with_integer ;;
let (x:specific_pair) = (3, 3.14);;

type complex = {re:float, im:float};;
let c = {re=2.;im=3.};; (* val c:complex = { .. } *)
let add_complex c1 c2 = {re=c1.re+.c2.re; im=c1.im+.c2.im};;

type coin = Heads | Tails;;
type suit = Spades | Hearts | Diamonds | Clubs ;;
type card = 
    King of suit
  | Queen of suit
  | Knight of suit
  | Knave of suit
  | Minor_card of suit * int
  | Trump of int
  | Joker ;;
King Spades ;;
Minor_card(Hearts, 10);;
Trump 21;;



type key = Plus | Minus | Times | Div | Equals | Digit of int ;;

let is_digit = function x -> (x>=0) && (x<=9) ;;

let valid ky = match ky with
    Digit n -> is_digit n
  | _ -> true ;;

type state = {
  lcd : int; (* last computation done *)
  lka : key; (* last key activated *)
  loa : key; (* last operator activated *)
  vpr : int  (* value printed *)
} ;;

let evaluate x y ky = match ky with
    Plus -> x + y
  | Minus -> x - y
  | Times -> x * y
  | Div -> x / y
  | Equals -> y
  | Digit _ -> failwith "evaluate : no op" ;;

let transition st ky =
  let digit_transition n =
    function Digit _ -> { st with lka=ky; vpr=st.vpr*10+n }
      | _            -> { st with lka=ky; vpr=n }
  in match ky with
      Digit p -> digit_transition p st.lka
    | _       -> let res = evaluate st.lcd st.vpr st.loa
		 in { lcd=res; lka=ky; loa=ky; vpr=res } ;;
