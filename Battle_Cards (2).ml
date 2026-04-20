(* name, energy, cost, attack, type *)
type card = (string * string * int * int * int)

(*card list*)
type hand = card list

(* hand, energy, health *)
type player = (hand*int*int)

(*Offending player, defending player, hand 1, hand 2*)
type gamestate = player*player*hand*hand

let add_energy card =
match card with
| (a,"Energy", v, b, c) -> (a,"Energy", v + 1, b, c)
| (_,_, _, _, _) -> card

let add_attack card =
match card with
| (a,"Attack", v, b, c) -> (a,"Attack", v, b, c + 1)
| (_,_, _, _, _) -> card

let depleat_energy card =
match card with
    | (_,"Energy", _, _, _) -> false
    | (_,_, _, _, _) -> true

let depleat_attacks card =
match card with
    | (_,"Attack", _, _, _) -> false
    | (_,_, _, _, _) -> true

let specail_card_handler_offense card hand =
    match card with
    | ("Depleat Energy", "Specail Offense", _, _, _) -> List.filter depleat_energy hand
    | ("Depleat Attacks", "Specail Offense", _, _, _) -> List.filter depleat_attacks hand
    | (_,_,_,_,_) -> hand

let specail_card_handler_defense card hand =
    match card with
    | ("Increase Energy", "Specail Defense", _, _, _) -> List.map add_energy hand
    | ("Increase Attacks", "Specail Defense", _, _, _) -> List.map add_attack hand
    | (_,_,_,_,_) -> hand


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
    | (_, "Energy",value, _, _) -> increase_energy player1 value
    | (_, _,_, cost, _) -> decrease_energy player1 cost

let turn_for_enemy_player (player2:player) (move:card) =
    let decrese_health player2 a =
        match player2 with
        | (hd,e,hl) -> if (hl - a) < 0 then (hd,e,0) else (hd,e,hl-a)
    in
    match move with
    | (_,"Energy", _, _, _) -> player2
    | (_,"Attack", _, _, attack) -> decrese_health player2 attack
    | (_,_,_,_,_) -> player2

let rec get_card (h:hand) (index:int) : card =
    match h with
    | [] -> failwith "Invalid index: Card not found"
    | hd :: tl -> if index = 0 then hd else get_card tl (index - 1)


(* Removes a card at a specific index from a hand, returning the chosen card and the remaining hand *)
let rec play_card (h:hand) (index:int) =
    match h with
    | [] -> []
    | hd::tl -> if index = 0 then tl else hd::(play_card tl (index -1))

let swap_hand_handler player1 player2 hand1 hand2 = (player2, player1, hand1, hand2)

(* gamestate -> int -> gamestate *)
let change_gamestate gamestate player_choice=
    match gamestate with
    | (player1, player2, h1, h2) ->
    match (get_card h1 player_choice) with
    | (_, "Specail Offense", _, _, _) -> (player2, player1, specail_card_handler_offense (get_card h1 player_choice) h2, play_card h1 player_choice)
    | (_, "Specail Defense", _, _, _) -> (player2, player1, h2, play_card (specail_card_handler_defense (get_card h1 player_choice) h1) player_choice)
    | ("Swap hands", "Specail", _, _, _) -> swap_hand_handler player1 player2 h1 h2
    | (_,_,_,_,_) ->
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
    | (name, _, energy, cost, attack) ->
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
    print_string ("Player 1 health: ");
    print_int(h);
    print_newline ();
    print_string("Player 1 Energy: ");
    print_int(e);
    print_newline ();
    match p2 with 
    (_, e2, h2) ->
    print_string ("Player 2 health: ");
    print_int(h2);
    print_newline ();
    print_string("Player 2 Energy: ");
    print_int(e2);
    print_newline ()


let text_function_1 gamestate = 
match gamestate with
    | (player1, player2, _, _) ->
    print_stats_1 player1 player2

let print_stats_2 p1 p2 =
    match p1 with 
    (_, e, h) ->
    print_string ("Player 2 health: ");
    print_int(h);
    print_newline ();
    print_string("Player 2 Energy: ");
    print_int(e);
    print_newline ();
    match p2 with 
    (_, e2, h2) ->
    print_string ("Player 1 health: ");
    print_int(h2);
    print_newline ();
    print_string("Player 1 Energy: ");
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

let rec check_energy gamestate player_choice =
  match gamestate with
  | (player_stats, _, hand_1, _) ->
  match player_stats with
  | (_, current_energy, _) ->
  match (get_card hand_1 player_choice) with
  | (_,"Energy",_,_,_) -> player_choice
  | (_,_,_,cost,_) -> 
      if current_energy >= cost then 
        player_choice 
      else begin
        print_endline "You don't have enough Energy to use that card. Pick a different one: ";
        let new_choice = (read_int ()) - 1 in
        check_energy gamestate new_choice
      end

let skip_turn gamestate =
  match gamestate with
  | (p1, p2, h1, h2) -> (p2, p1, h2, h1)


let rec gameplay gamestate = 

    (*Need a func to check health and add a card. Also initial func that checks to make sure players have cards*)
    check_health gamestate;
    display_cards gamestate 1;
    print_string "Player 1 choose a card index (0 to draw): ";
    let input_1 = read_int () in
    let gamestate_1 = 
      if input_1 = 0 then begin
        let drawn_once = draw_new (skip_turn gamestate) in 
        draw_new drawn_once
      end
      else begin
        let valid_1 = check_energy gamestate (input_1 - 1) in
        let gamestate_1_pre = change_gamestate gamestate valid_1 in
        text_function_2 gamestate_1_pre;
        draw_new gamestate_1_pre
      end
    in

    check_health gamestate_1;
    display_cards gamestate_1 1;
    print_string "Player 2 choose a card index (0 to skip turn): ";
    let input_2 = read_int () in
    let gamestate_pre_2 =
      if input_2 = 0 then begin
        let drawn_once = draw_new (skip_turn gamestate_1) in 
        draw_new drawn_once
      end
      else begin
        let valid_2 = check_energy gamestate_1 (input_2 - 1) in
        let gamestate_2 = change_gamestate gamestate_1 valid_2 in
        text_function_1 gamestate_2;
        draw_new gamestate_2
      end
    in
    gameplay gamestate_pre_2

(*Battle_Cards.gameplay (Battle_Cards.init_gamestate ());; *)