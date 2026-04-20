(* List of possible cards: name, energy, cost, attack *)
let all_cards = [
  ("Energy", "Energy", 1, 0, 0);
  ("Energy", "Energy", 1, 0, 0);
  ("Energy", "Energy", 1, 0, 0);
  ("Energy", "Energy", 2, 0, 0);
  ("Energy", "Energy", 2, 0, 0);
  ("Punch", "Attack", 0, 1, 2);
  ("Kick", "Attack", 0, 2, 4);
  ("Slash", "Attack", 0, 3, 6);
  ("Fireball", "Attack", 0, 4, 8);
  ("Mega Punch", "Attack", 0, 5, 10);
  ("Quick Strike", "Attack", 0, 1, 3);
  ("Depleat Energy", "Specail Offense", 0, 5, 0);
  ("Depleat Attacks", "Specail Offense", 0, 5, 0);
  ("Swap hands", "Specail", 0, 5, 0);
  ("Increase Energy", "Specail Defense", 0, 5, 0);
  ("Increase Attacks", "Specail Defense", 0, 5, 0);


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
