open OUnit2
open Game


let j = Yojson.Basic.from_file "tworoom.json"
let jb = Yojson.Basic.from_file "oneroom.json"
let gates = Yojson.Basic.from_file "gates.json"
let s = init_state j
let sb = init_state jb
let gates_init = init_state gates


let my_room_1 = {id = "Forest"; description= "Jungle"; points = 999; exits= []; treasure=[]}
let my_room_2 = {id = "Dessert"; description= "Dry"; points = 99; exits= []; treasure=[]}
let my_room_3 = {id = "Sea"; description= "Wet"; points = 9; exits= []; treasure=[]}

let my_item_1 = {id = "Cane"; description= "Stick"; points = 999}
let my_item_2 = {id = "Sword"; description= "Pointy"; points = 99}
let my_item_3 = {id = "Harpoon"; description= "Pointy"; points = 99}

(*tworoom items*)
let black_hat = {id = "black hat"; description= "a black fedora"; points = 100}
let red_hat = {id = "red hat"; description= "a red fez"; points = 10000}
let white_hat = {id = "white hat"; description= "a white panama"; points = 1000}

(*tworoom exits*)
let exit_from_room1 = {direction = "north"; room_id = "room2"}
let exit_from_room2 = {direction = "south"; room_id = "room1"}

(*gates exits*)
let exit_from_start = {direction = "start"; room_id = "Gates Main Entrance"}
let exits_from_main_entrance = [{direction = "inside"; room_id = "Gates Main Lobby"};
                                {direction = "in"; room_id = "Gates Main Lobby"};
                                {direction = "i"; room_id = "Gates Main Lobby"};
                                {direction = "down"; room_id = "Outside Gates, Lower Level"};
                                {direction = "d"; room_id = "Outside Gates, Lower Level"}]

(*tworoom rooms*)
let room1 = {id = "room1";
    description= "This is Room 1.  There is an exit to the north.\nYou should drop the white hat here.";
    points = 1; exits= [exit_from_room1]; treasure=[white_hat; red_hat]}
let room2 = {id = "room2"; description= "This is Room 2.  There is an exit to the south.\nYou should drop the black hat here.";
    points = 10; exits= [exit_from_room2]; treasure=[black_hat]}

(*Gates rooms; Only IDs accurate*)
let start = {id = "Start"; description= "You're working on A2 for your favorite class, CS 3110, when you find yourself
completely lost and confused about functional programming! (How you made it
through A1 is an enigma...)

You decide to go to Gates Hall on your quest to find inspiration. Maybe moving
around objects inside will move you to write brilliant code. It's worth a shot,
anyway.

Type START to begin, and don't forget to check your inventory!";
    points = 0; exits= [exit_from_start]; treasure=[]}

let main_entrance = {id = "Gates Main Entrance"; description= "You're right outside the main entrance of Gates Hall. You're wondering why
you're standing out here when you could go INSIDE through the two sets of glass
double doors to the lobby.

Alternatively, you could head DOWN to the ground floor entrance.";
    points = 0; exits= exits_from_main_entrance; treasure=[]}

let main_lobby = {id = "Gates Main Lobby"; description= "You're standing in the middle of the Gates Hall lobby. You smell the aroma of
coffee wafting from the SOUTH where Gimme! Coffee is. To the EAST is a corridor
that leads further into the building, and to the NORTHEAST is the dean's office.

You can also go UP or DOWN the stairs, as well as go into the ELEVATOR. Or you
could head back OUTSIDE.";
    points = 10; exits= []; treasure=[]}

(*tworoom locations*)
let location1 = {room= room1; item= black_hat}
let location2 = {room= room1; item= red_hat}

let location3 = {room= room2; item= white_hat}
let location4 = {room= room1; item= white_hat}

(*Test states following [s]*)

let s_after_go_north =
{rooms = s.rooms;
 visited_rooms = [room2; room1];
 current_room = room2;
 items = s.items;
 inventory = s.inventory;
 locations = s.locations;
 score = 10011;
 turns = 1}

let s_after_take_black_hat =
{rooms = s.rooms;
 visited_rooms = [room1];
 current_room = room1;
 items = s.items;
 inventory = [black_hat; white_hat];
 locations = [location2];
 score = 10001;
 turns = 1}

let s_after_take_red_hat =
{rooms = s.rooms;
 visited_rooms = [room1];
 current_room = room1;
 items = s.items;
 inventory = [red_hat; white_hat];
 locations = [location1];
 score = 1;
 turns = 1}

 let s_after_drop_white_hat =
{rooms = s.rooms;
 visited_rooms = [room1];
 current_room = room1;
 items = s.items;
 inventory = [];
 locations = [location4; location1; location2];
 score = 11001;
 turns = 1}

 (*Test states following [gates_init]*)

 let gates_after =
 {rooms = gates_init.rooms;
 visited_rooms = [main_entrance; start];
 current_room = main_lobby;
 items = gates_init.items;
 inventory = gates_init.inventory;
 locations = gates_init.locations;
 score = gates_init.score;
 turns = gates_init.turns + 2}

let list_dot_contains_tests = [
  "1." >::
  (fun _ -> assert_equal true ( list_dot_contains 1 [1;2;3]) );
  "2." >::
  (fun _ -> assert_equal false ( list_dot_contains location3 [location1; location2]) );
  "3." >::
  (fun _ -> assert_equal true ( list_dot_contains red_hat room1.treasure) );
  "4." >::
  (fun _ -> assert_equal false ( list_dot_contains black_hat room1.treasure) );
]

let init_item_score_ind_room_tests =[
  "1. room1 a" >:: (fun _ ->
    assert_equal 10000 ( init_item_score_ind_room s.current_room s.locations 0 ));
  "2. room1 b" >:: (fun _ ->
    assert_equal 10000 ( init_item_score_ind_room room1 s.locations 0 ));
  "3. room2" >:: (fun _ ->
    assert_equal 0 ( init_item_score_ind_room room2 s.locations 0 ));
]

let init_state_tests =[
  "1. Room 1 id check" >:: (fun _ ->
    assert_equal "room1" ( (List.hd (init_state j).rooms).id ));
  "2. Room 1 description check" >:: (fun _ ->
    assert_equal "This is Room 1.  There is an exit to the north.\nYou should drop the white hat here."
   ( (List.hd (init_state j).rooms).description ));
  "3. Room 1 points check" >:: (fun _ ->
    assert_equal 1 ( (List.hd (init_state j).rooms).points ));
  "4. Room 1 exits direction check" >:: (fun _ ->
    assert_equal "north" ( (List.hd ((List.hd (init_state j).rooms).exits)).direction));
  "5. Room 1 exits room_id check" >:: (fun _ ->
    assert_equal "room2" ( (List.hd ((List.hd (init_state j).rooms).exits)).room_id));
  "6. Room 1 treasure check a" >:: (fun _ ->
    assert_equal "white hat" ( (List.hd ((List.hd (init_state j).rooms).treasure)).id));
  "7. Room 1 treasure check b" >:: (fun _ ->
    assert_equal "red hat" ( (List.hd (List.rev ((List.hd (init_state j).rooms).treasure))).id) );
  "8. Start room check" >:: (fun _ ->
    assert_equal "room1" ( (init_state j).current_room.id) );
  "9. Inventory id check" >:: (fun _ ->
    assert_equal "white hat" ( (List.hd ( (init_state j).inventory) ).id) );
  "10. Inventory points check" >:: (fun _ ->
    assert_equal 1000 ( (List.hd ( (init_state j).inventory) ).points) );
  "11. Inventory descriptiion check" >:: (fun _ ->
    assert_equal "a white panama" ( (List.hd ( (init_state j).inventory) ).description) );
  "12. start location room id check" >:: (fun _ ->
    assert_equal "room1" ( ((List.hd ( (init_state j).locations) ).room).id )  );
  "13. start location item id check" >:: (fun _ ->
    assert_equal "black hat" ( ((List.hd ( (init_state j).locations) ).item).id )  );
  "14. visited rooms check" >:: (fun _ ->
    assert_equal "room1" ( (List.hd ( (init_state j).visited_rooms) ).id ) );
  "15. locations check" >:: (fun _ ->
    assert_equal "room1" ( (List.hd ( (init_state j).locations) ).room.id ) );
  "16. locations check" >:: (fun _ ->
    assert_equal "black hat" ( (List.hd ( (init_state j).locations) ).item.id ) );
  "17. start_score check" >:: (fun _ ->
    assert_equal 10001 ( s.score ) );
  "18. oneroom exit check" >:: (fun _ ->
    assert_equal [] ( sb.current_room.exits ) );
]


let max_score_tests =[
  "max" >:: (fun _ -> assert_equal 11111 (j |> init_state |> max_score));
]

let build_assoc_tests = [
  "1. Emptyness"
  >:: (fun _ -> assert_equal [] (build_assoc [] []) );
  "2. Half Emptyness a"
  >:: (fun _ -> assert_equal [("Sword", "Dessert"); ("Cane", "Forest")]
    ( build_assoc
    [{room= my_room_1; item=my_item_1}; {room= my_room_2; item= my_item_2}]  []) );
  "3. Half Emptyness b"
  >:: (fun _ -> assert_equal [("Harpoon", "Sea"); ("Sword", "Dessert"); ("Cane", "Forest")]
    ( build_assoc [{room= my_room_1; item=my_item_1};
      {room= my_room_2; item= my_item_2};
      {room= my_room_3; item= my_item_3}]  []) );
]

let locations_tests = [
  "1." >:: (fun _ -> assert_equal [("black hat", "room1"); ("red hat", "room1")] (locations s) )
]

let num_words_tests =[
  "1. one word" >:: (fun _ -> assert_equal 1 (num_words "one" 1));
  "2. two words" >:: (fun _ -> assert_equal 2 (num_words "one two" 1));
  "3. three words" >:: (fun _ -> assert_equal 3 (num_words "one two three" 1));
  "4. three words white space" >:: (fun _ -> assert_equal 3 (num_words "    one two three " 1));
  "5. three chars" >:: (fun _ -> assert_equal 3 (num_words "1 two $" 1));
  "6. three chars again" >:: (fun _ -> assert_equal 3 (num_words "$ two $" 1));
]

let change_list_string_tests = [
  "1." >:: (fun _ -> assert_equal "" (change_list_string []));
  "2." >:: (fun _ -> assert_equal "black" (change_list_string ["black"]));
  "3." >:: (fun _ -> assert_equal "black hat" (change_list_string ["black"; "hat"]));

]

let get_command_tests =[
  "1. Quit vanila"
  >:: (fun _ -> assert_equal Quit (get_command "quit"));
  "2. Quit whitespace"
  >:: (fun _ -> assert_equal Quit (get_command "   quit     "));
  "3. Quit upper/lower"
  >:: (fun _ -> assert_equal Quit (get_command "quIT"));
  "4. Look"
  >:: (fun _ -> assert_equal Look (get_command "look"));
  "5. Inv"
  >:: (fun _ -> assert_equal Inv (get_command "inv"));
  "6. Inventory"
  >:: (fun _ -> assert_equal Inv (get_command "inventory"));
  "7. Score"
  >:: (fun _ -> assert_equal Score (get_command "score"));
  "8. Someweirdcrap"
  >:: (fun _ -> assert_equal (Go "someweirdcrap") (get_command "someweirdcrap"));
  "9. Go"
  >:: (fun _ -> assert_equal (Go "somewhere") (get_command "go somewhere"));
  "10. Take"
  >:: (fun _ -> assert_equal (Take "something") (get_command "TaKe something"));
  "11. Take"
  >:: (fun _ -> assert_equal (Take "black hat") (get_command "TaKe black hat"));
  "12. Take"
  >:: (fun _ -> assert_equal (Take "black hat") (get_command "TaKe BLAck HaT    "));
  "13. Drop"
  >:: (fun _ -> assert_equal (Drop "something") (get_command "drOp something"));
  "14. Someweird crap"
  >:: (fun _ -> assert_equal (Go "someweird crap") (get_command "someweird crap"));
  "15. Some weird crap"
  >:: (fun _ -> assert_equal (Go "some weird crap") (get_command "some weird crap"));
  "16. WUT?, still MORE weird crap?"
  >:: (fun _ -> assert_equal
    (Go "wut?, still more weird crap?") (get_command "WUT?, still MORE weird crap?"));
  "17. WUT?, still MORE weird crap?"
  >:: (fun _ -> assert_equal
    (Go "inside") (get_command "inside"));
]

let can_exit_tests =[
  "1. tworoom init success"
  >:: (fun _ -> assert_equal true (can_exit "north" s.current_room.exits));
  "2. tworoom init fail a"
  >:: (fun _ -> assert_equal false (can_exit "south" s.current_room.exits));
  "3. tworoom init fail b"
  >:: (fun _ -> assert_equal false (can_exit "NoRTH" s.current_room.exits));
  "4. tworoom init fail c"
  >:: (fun _ -> assert_equal false (can_exit "Wierdcrap" s.current_room.exits));
  "5. oneroom fail"
  >:: (fun _ -> assert_equal false (can_exit "north" sb.current_room.exits));
  "6. oneroom fail"
  >:: (fun _ -> assert_equal false (can_exit "Literallyanywhere" sb.current_room.exits));
]

let get_exit_room_id_tests = [
  "1. tworoom init success"
  >:: (fun _ -> assert_equal "room2" (get_exit_room_id "north" s.current_room.exits));
]

let all_items_in_room_tests = [
  "1. init tworoom room2"
  >:: (fun _ -> assert_equal []
    (all_items_in_room (List.hd (List.rev (s.rooms))) s.locations []));
  "2. init tworoom room1"
  >:: (fun _ -> assert_equal [red_hat; black_hat]
    (all_items_in_room s.current_room s.locations []));
]

let can_take_drop_tests = [
  "1."
  >:: (fun _ -> assert_equal true (can_take_drop black_hat [red_hat; white_hat; black_hat]));
  "2."
  >:: (fun _ -> assert_equal true (can_take_drop black_hat [red_hat; black_hat; white_hat]));
  "3."
  >:: (fun _ -> assert_equal false (can_take_drop black_hat [red_hat; white_hat]));
]

let remove_from_locations_tests = [
  "1."
  >:: (fun _ -> assert_equal  [location1] (remove_from_locations red_hat [location1; location2]));
  "2."
  >:: (fun _ -> assert_equal  [location3; location1]
    (remove_from_locations red_hat [location1; location2; location3]));
]

let do'_tests = [
  "1. After go north from s a"
  >:: (fun _ -> assert_equal s_after_go_north (do' "go north" s));
  "2. After go north from s b"
  >:: (fun _ -> assert_equal s_after_go_north (do' "   Go nOrth   " s));
  "3. After go north from s c"
  >:: (fun _ -> assert_equal s_after_go_north (do' "   nOrth  " s));
  "4. After take black hat at s a"
  >:: (fun _ -> assert_equal s_after_take_black_hat (do' "take black hat" s));
  "5. After take black hat at s b"
  >:: (fun _ -> assert_equal s_after_take_black_hat (do' "  taKe bLack hat " s));
  "6. After take red hat at s "
  >:: (fun _ -> assert_equal s_after_take_red_hat (do' "take red hat" s));
  "7. After drop white hat at s "
  >:: (fun _ -> assert_equal s_after_drop_white_hat (do' "drop white hat" s) ) ;
  "8. Gates, you idiot "
  >:: (fun _ -> assert_equal gates_after.current_room.id
    (    (do' "inside" (do' "start" gates_init)).current_room.id   )   ) ;

]

let suite =
  "Adventure test suite"
  >::: do'_tests

let _ = run_test_tt_main suite
