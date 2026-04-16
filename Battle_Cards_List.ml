(* List of possible cards: name, energy, cost, attack *)
let all_cards = [
  ("Energy", 1, 0, 0);
  ("Energy", 1, 0, 0);
  ("Energy", 1, 0, 0);
  ("Energy", 1, 0, 0);
  ("Energy", 1, 0, 0);
  ("Punch", 0, 1, 2);
  ("Kick", 0, 2, 4);
  ("Slash", 0, 3, 6);
  ("Fireball", 0, 4, 8);
  ("Mega Punch", 0, 5, 10);
  ("Quick Strike", 0, 1, 3);
]

let rec get_random_cards n lst acc =
  if n = 0 then acc
  else
    let random_index = Random.int (List.length lst) in
    let chosen_card = List.nth lst random_index in
    get_random_cards (n - 1) lst (chosen_card :: acc)

let deal_hand () =
  Random.self_init ();
  get_random_cards 7 all_cards []
