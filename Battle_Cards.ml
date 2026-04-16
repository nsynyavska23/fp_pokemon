(* name, energy, cost, attack *)
type card = (string * int * int * int)

type hand = card list

(* hand, energy, health *)
type player = (hand*int*int)

type gamestate = player*player*hand*hand

let turn_for_current_player (player1:player) (move:card) =
    let increase_energy player1 v =
    match player1 with
    | (hd,e,hl) -> (hd,e+ v, hl)
    in
    let decrease_energy player1 v =
    match player1 with
    | (hd,e,hl) -> (hd,e - v, hl)
    in
    match move with
    | ("Energy", value, _, _) -> increase_energy player1 value
    | (_,_, cost, _) -> decrease_energy player1 cost

let turn_for_enemy_player (player2:player) (move:card) =
    let decrese_health player2 a =
        match player2 with
        | (hd,e,hl) -> if (hl - a) < 0 then (hd,e,0) else (hd,e,hl-a)
    in
    match move with
    | ("Energy", _, _, _) -> player2
    | (_,_, _, attack) -> decrese_health player2 attack

let rec get_card (h:hand) (index:int) : card =
    match h with
    | [] -> failwith "Invalid index: Card not found"
    | hd :: tl -> if index = 0 then hd else get_card tl (index - 1)


(* Removes a card at a specific index from a hand, returning the chosen card and the remaining hand *)
let rec play_card (h:hand) (index:int) =
    match h with
    | [] -> []
    | hd::tl -> if index = 0 then tl else hd::(play_card tl (index -1))

(* gamestate -> int -> gamestate *)
let change_gamestate gamestate player_choice=
    match gamestate with
    | (player1, player2, h1, h2) ->
    (turn_for_enemy_player player2 (get_card h1 player_choice),
    turn_for_current_player player1 (get_card h1 player_choice),
    h2,
    play_card h1 player_choice)

(* these function were wiritten by ai but were aktered significantly due to them returning tuples. 
It was in response to the prompt. 
"Look at the file battle cards. I want a way where a user can accsess a specific element of the list and choose one." 
Model: Gemini 3.1 Pro *)



(*Imma do an exirement and see what happens. I am going to try to write an imperative function
https://www.reddit.com/r/ocaml/comments/16obbm0/getting_users_input_in_ocaml/
*)

let print_card_type (c: card) =
    match c with
    | (name, energy, cost, attack) ->
        print_string "["; print_string name;
        print_string ", Energy: "; print_int energy;
        print_string ", Cost: "; print_int cost;
        print_string ", Attack: "; print_int attack; print_string "]"

let print_tuple (n: int) (s: string) (c: card) =
    print_int n;
    print_string s;
    print_card_type c;
    print_newline ()

let rec display_cards gamestate n =
match gamestate with 
| (a, b, hand, d) ->
match hand with
|   [] -> ()
|   (hd::tl) -> print_tuple n ": " hd;
    display_cards (a, b, tl, d) (n+1)

let check_health gamestate =
    match gamestate with
    | (p1, p2, _, _) -> 
    match p1 with 
    | (_,_, h1) -> if h1 <= 0 then print_string ("gameover") else
    match p2 with 
    | (_, _, h2) -> if h2 <= 0 then print_string ("gameover") else print_string ("")


let init_gamestate () =
  let hand1 = Battle_Cards_List.deal_hand () in
  let hand2 = Battle_Cards_List.deal_hand () in
  let p1 = (hand1, 0, 20) in
  let p2 = (hand2, 0, 20) in
  (p1, p2, hand1, hand2)
 

let print_stats_1 p1 p2 =
    match p1 with 
    (_, e, h) ->
    print_string ("Player 1 health");
    print_int(h);
    print_newline ();
    print_string("Player 1 Energy");
    print_int(e);
    print_newline ();
    match p2 with 
    (_, e2, h2) ->
    print_string ("Player 2 health");
    print_int(h2);
    print_newline ();
    print_string("Player 2 Energy");
    print_int(e2);
    print_newline ()


let text_function_1 gamestate = 
match gamestate with
    | (player1, player2, _, _) ->
    print_stats_1 player1 player2

let print_stats_2 p1 p2 =
    match p1 with 
    (_, e, h) ->
    print_string ("Player 2 health");
    print_int(h);
    print_newline ();
    print_string("Player 2 Energy");
    print_int(e);
    print_newline ();
    match p2 with 
    (_, e2, h2) ->
    print_string ("Player 1 health");
    print_int(h2);
    print_newline ();
    print_string("Player 1 Energy");
    print_int(e2);
    print_newline ()


let text_function_2 gamestate = 
match gamestate with
    | (player1, player2, _, _) ->
    print_stats_2 player1 player2

let draw_new gamestate =
match gamestate with
(a,b,c,h) -> (a,b,c,(Battle_Cards_List.get_random_cards 1 Battle_Cards_List.all_cards h)) 


(*Used ai to modify this function. What it mainly did was fixed a few syntax errors and introduced mutibility*)
let rec gameplay gamestate = 

    (*Need a func to check health and add a card. Also initial func that checks to make sure players have cards*)
    check_health gamestate;
    display_cards gamestate 1;
    print_string "Player 1 choose a card index: ";
    let input_1 = read_int () in
    let gamestate_1_pre = change_gamestate gamestate (input_1 -1) in
    text_function_2 gamestate_1_pre;
    let gamestate_1 = draw_new gamestate_1_pre in

    check_health gamestate_1;
    display_cards gamestate_1 1;
    print_string "Player 2 choose a card index: ";
    let input_2 = read_int () in
    let gamestate_2 = change_gamestate gamestate_1 (input_2 -1) in
    text_function_1 gamestate_2;
    let gamestate_pre_2 = draw_new gamestate_2 in
    gameplay gamestate_pre_2

