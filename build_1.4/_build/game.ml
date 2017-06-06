(*
 * CS 3110 Fall 2016 A2
 * Author: Arjan Singh
 * NetID: as2995
 *
 * My implementation for New_Map is heavily derived from the implementation
 * of List.map, which I read in the List module's source code.
 *
 * Further, the text art in my adventure game was not made by me. I got it
 * from the internet: http://chris.com/ascii/index.php?art=creatures/monsters
 *
 *
 * NOTE: PLEASE MAKE THE COMMAND WINDOW AS BIG AS POSSIBLE BEFORE YOU
 * PLAY MY ADVENTURE GAME
 *
 *)

open Yojson.Basic.Util

(*----------------------------------------------------------------------------*)
(*TYPES*)

type exit = {
  direction: string;
  room_id: string}

type item = {
  id: string;
  description: string;
  points: int}

type room = {
  id: string;
  description: string;
  points: int;
  exits: exit list;
  treasure: item list}

type location = {
  room: room;
  item: item}

type state = {
  rooms: room list;
  visited_rooms: room list;
  current_room: room;
  items: item list;
  inventory: item list;
  locations: location list;
  score: int;
  turns: int
  }

type command =
  | Go of string
  | Take of string
  | Drop of string
  | Quit
  | Look
  | Inv
  | Turns
  | Score

(*[Illegal] is raised by [do'] to indicate that a command is illegal;
 * see the documentation of [do'] below. *)
exception Illegal

(*----------------------------------------------------------------------------*)
(*FUNCTIONS*)

(*[new_map f the_list second_argument] returns a list such that [f] has been applied
 * each element of [the_list], with [second_argument] as the second argument*)
let rec new_map f the_list second_argument =
  match the_list with
  |[] -> []
  |h::t -> let r = (f h second_argument) in r::(new_map f t second_argument)

(*[list_dot_contains element a_list] returns true if [element] is in [a_list]
 * and false if not*)
let rec list_dot_contains element a_list =
  match a_list with
  | [] -> false
  | h::t -> if h=element then true else list_dot_contains element t

(*[change_to_item_type j_item] is the item, but now of type item rather than type json*)
let change_to_item_type j_item =
  let item_id = j_item |> member "id" |> to_string in
  let item_description = j_item |> member "description" |> to_string in
  let item_points = j_item |> member "points" |> to_int in

  {id = item_id; description = item_description; points = item_points}

(*[get_id_from_item (item:item)] is the ID of the given item, [item]*)
let get_id_from_item (item:item) =
  item.id

(*[get_id_from_room (room:room)] is the ID of the given room, [room]*)
let get_id_from_room (room:room) =
  room.id

(*[get_item_from_id item_id items] is the item associated with the given [item_id].
 * [items] is the list of all items in the game*)
let rec get_item_from_id item_id (items:item list) =
  match items with
  |[] -> raise Illegal
  | h::t -> if h.id = item_id then h else get_item_from_id item_id t

(*[get_room_from_id room_id rooms] is the room associated with the given [room_id].
 * [rooms] is the list of all items in the game*)
let rec get_room_from_id room_id (rooms: room list) =
  match rooms with
  |[] -> raise Illegal
  | h::t -> if h.id = room_id then h else get_room_from_id room_id t

(*[change_to_item_type j_exit] is the exit, but now of type exit rather than type json*)
let change_to_exit_type j_exit =
  let direction = j_exit |> member "direction" |> to_string in
  let room_id = j_exit |> member "room_id" |> to_string in

  {direction = direction ; room_id = room_id}

(*[change_to_room_type j_room j] is the room, but now of type room rather than type json*)
let change_to_room_type j_room j=
  let room_id = j_room |> member "id" |> to_string in
  let room_descriptions = j_room |> member "description" |> to_string in
  let room_points = j_room |> member "points" |> to_int in
  let j_exits = j_room |> member "exits" |> to_list in
  let room_exits = List.map change_to_exit_type j_exits in
  let j_items = j |> member "items" |> to_list in
  let all_items = List.map change_to_item_type j_items in
  let item_ids = j_room |> member "treasure" |> to_list |> filter_string in
  let treasure = new_map get_item_from_id item_ids all_items in

  {id = room_id; description = room_descriptions; points = room_points;
  exits = room_exits; treasure = treasure}

(*[get_caml_rooms j] is the list of rooms associated with json j, and has type room list*)
let get_caml_rooms j =
  let j_rooms = j |> member "rooms" |> to_list in

  new_map change_to_room_type j_rooms j


(*[change_to_item_type j_item] is the item, but now of type item rather than type json*)
let change_to_location_type j_location j =
  let all_rooms = get_caml_rooms j in
  let j_items = j |> member "items" |> to_list in
  let all_items = List.map change_to_item_type j_items in
  let room_id = j_location |> member "room" |> to_string in
  let item_id = j_location |> member "item" |> to_string in
  let room = get_room_from_id room_id all_rooms in
  let item = get_item_from_id item_id all_items in

  {room= room; item= item}

(*[init_item_score_ind_room room location_list prev_score] are the initial points awarded
 * for items in a single room, [room], belonging to that room's treasure list*)
let rec init_item_score_ind_room room location_list prev_score=
  match location_list with
  |[] -> prev_score
  |h::t -> if (list_dot_contains h.item room.treasure) then
            if (h.room = room) then
              init_item_score_ind_room room t (prev_score+h.item.points)
            else
              prev_score
           else
            init_item_score_ind_room room t (prev_score)

(*[init_item_score room_list location_list prev_score] are the initial points awarded
 * for items in EVERY room in the game, represented by [room_list], being in the right room
 * as deemed by the treasure list of the rooms*)
let rec init_item_score room_list location_list prev_score=
  match room_list with
  |[] -> prev_score
  | h::t ->

  init_item_score t location_list (prev_score + (init_item_score_ind_room h
  location_list 0))

(*----------------------------------------------------------------------------*)

(*REQUIRED 1. [init_state j] is the initial state of the game as
 * determined by JSON object [j] *)
let init_state j =
  let rooms = get_caml_rooms j in
  let j_items = j |> member "items" |> to_list in
  let all_items = List.map change_to_item_type j_items in
  let start_inv_id = j |>  member "start_inv" |> to_list |> filter_string in
  let start_inv = new_map get_item_from_id start_inv_id all_items in
  let start_room_id = j |> member "start_room" |> to_string in
  let start_room = get_room_from_id start_room_id rooms in
  let j_start_locations = j |> member "start_locations" |> to_list in
  let start_locations = new_map change_to_location_type j_start_locations j in
  let start_score = start_room.points + (init_item_score rooms start_locations 0)
      in

  {rooms = rooms;
  visited_rooms = [start_room];
  current_room = start_room;
  items = all_items;
  inventory = start_inv;
  locations = start_locations;
  score = start_score;
  turns = 0}

(*----------------------------------------------------------------------------*)

(*[get_room_points] are the total points associated with a room, [room]*)
let get_room_points (room:room) =
  room.points

(*[get_item_points] are the total points associated with an item, [item]*)
let get_item_points (item:item) =
  item.points

(*----------------------------------------------------------------------------*)

(*REQUIRED 2. [max_score s] is the maximum score for the adventure whose current
 * state is represented by [s]. *)
let max_score s =
  let all_rooms = s.rooms in
  let all_items = s.items in

  let room_points = List.map get_room_points all_rooms in
  let item_points = List.map get_item_points all_items in

  (List.fold_left (+) 0 room_points) + (List.fold_left (+) 0 item_points)

(*REQUIRED 3. [score s] is the player's current score. *)
let score s =
  s.score

(*REQUIRED 4. [turns s] is the number of turns the player has taken so far. *)
let turns s =
  s.turns

(*REQUIRED 5. [current_room_id s] is the id of the room in which the adventurer
 * currently is. *)
let current_room_id s =
  s.current_room.id

(*REQUIRED 6. [inv s] is the list of item id's in the adventurer's current inventory.
 * No item may appear more than once in the list.  Order is irrelevant. *)
let inv s =
  let item_list = s.inventory in
  List.map get_id_from_item item_list

(*----------------------------------------------------------------------------*)

(*[inv_item s] is the list of items (not IDs) in the adventurer's current inventory.
 * No item may appear more than once in the list.  Order is irrelevant. *)
let inv_item s =
  s.inventory

(*----------------------------------------------------------------------------*)

(*REQUIRED 7. [visited s] is the list of id's of rooms the adventurer has visited.
 * No room may appear more than once in the list.  Order is irrelevant. *)
let visited s =
  let room_list = s.visited_rooms in
  List.map get_id_from_room room_list

(*----------------------------------------------------------------------------*)

(*[let insert_into_assoc (location:location) prev_assoc_list] returns a list with
 * the fields of [location] appended to the front of the old list [prev_assoc_list],
 * as a tuple *)
let insert_into_assoc (location:location) prev_assoc_list =
  (location.item.id, location.room.id)::prev_assoc_list

(*[build_assoc (location_list:location list) prev_assoc_list] returns a list with
 * elements of type (item.id * room.id), as opposed to type location in location_list
 * appended on to prev_assoc_list *)
let rec build_assoc (location_list:location list) prev_assoc_list=
  match location_list with
  |[] -> prev_assoc_list
  | h::t -> build_assoc t (insert_into_assoc h prev_assoc_list)

(*----------------------------------------------------------------------------*)

(*REQUIRED 8. [locations s] is an association list mapping item id's to the
 * id of the room in which they are currently located.  Items
 * in the adventurer's inventory are not located in any room.
 * No item may appear more than once in the list.  The relative order
 * of list elements is irrelevant, but the order of pair components
 * is essential:  it must be [(item id, room id)]. *)
let locations s =
  let location_list = s.locations in
  let assoc_list = build_assoc location_list [] in
  List.rev assoc_list

(*----------------------------------------------------------------------------*)

(*[lookup item_id location_assoc_list] is the room_id to which [item_id] is related to
 * in assoc list *)
let rec lookup item_id location_assoc_list =
  match location_assoc_list with
  | [] -> None
  | (k, v)::t -> if k=item_id then v else lookup item_id t

(*[num_words the_string old_number] is the number of words in [the_string].
 * Put old_number as 1. *)
let rec num_words the_string old_number =
  let the_string = String.trim the_string in
  if (String.contains the_string ' ') then
    let space_index = String.index the_string ' ' in
    let new_number = old_number + 1 in
    num_words ( String.sub the_string (space_index+1) (String.length the_string
     - (space_index+1) )) new_number
  else old_number

(*[change_list_string word_list] is a string with the words in word_list
 * seprated by " " *)
let rec change_list_string word_list =
  match word_list with
  | [] -> ""
  | h::[] -> h
  | h::t -> h^" "^(change_list_string t)

(*[get_command command_string] is a command (of type [command]) given a command of type
 * [string] *)
let get_command command_string =
 let command_string = String.trim command_string in
 let command_string = String.lowercase_ascii command_string in
 let word_list = Str.split (Str.regexp " ") command_string in
 let word_list = List.map String.trim word_list in
  match word_list with
  | h::[] -> (match command_string with
              | "quit" -> Quit
              | "look" -> Look
              | "repeat" -> Look
              | "what" -> Look
              | "pardon" -> Look
              | "inv"  -> Inv
              | "inventory" -> Inv
              | "turns" -> Turns
              | "score" -> Score
              | _ -> Go command_string)
 | h::t -> (match h with
                  | "go" -> Go (change_list_string t)
                  | "proceed" -> Go (change_list_string t)
                  | "take" -> Take (change_list_string t)
                  | "obtain" -> Take (change_list_string t)
                  | "grab" -> Take (change_list_string t)
                  | "drop" -> Drop (change_list_string t)
                  | "insert" -> Drop (change_list_string t)
                  | "put" -> Drop (change_list_string t)
                  | "attach" -> Drop (change_list_string t)
                  | "tether" -> Drop (change_list_string t)
                  | _ -> Go command_string)
 | _ -> Go command_string

(*[can_exit direction exit_list] is a bool that indicates whether [direction] can
 * be used to exit the current room, given the current room's exit list, [exit_list] *)
let rec can_exit direction exit_list =
  match exit_list with
  | [] -> false
  | h::t -> if direction=h.direction then true else can_exit direction t

(*[get_exit_room_id direction exit_list] the ID of the room that the player will go to
 * if she exits through [direction] *)
let rec get_exit_room_id direction exit_list =
  match exit_list with
  | [] -> raise Illegal
  | h::t -> if direction=h.direction then h.room_id
            else get_exit_room_id direction t

(*[is_visited room visited_rooms] is a bool that indicates if [room] has been previously
 * visited by the player *)
let rec is_visited room visited_rooms =
  match visited_rooms with
    | [] -> false
    | h::t -> if room=h then true else is_visited room t

(*[all_items_in_room room location_list prev_item_list] is a list of all items currently
 * in [room] *)
let rec all_items_in_room room location_list prev_item_list =
  match location_list with
    | [] -> prev_item_list
    | h::t ->
      if h.room = room then
        let new_item_list = (h.item)::prev_item_list in
        all_items_in_room room t new_item_list
      else
        all_items_in_room room t prev_item_list

(*[can_take_drop item item_list] is a bool that determines whether an item can be
 * picked up from the room, or dropped from the inventory. Note: The implementation of
 * this function is identical to [list_dot_contains element a_list], but I wanted
 * a more context sensitive name to use in [do' st c] *)
let rec can_take_drop item item_list =
  match item_list with
  | [] -> false
  | h::t -> if item=h then true else can_take_drop item t

(*[part_of_treasure item room_treasure] is a bool that indicates if [item] is part
 * of the trrasure list of a room, represented by [room treasure]*)
let rec part_of_treasure item room_treasure =
  match room_treasure with
  | [] -> false
  | h::t -> if h=item then true else part_of_treasure item t

(*[remove_from_locations item location_list] returns a list without the
 * the location type element, that contains [item].
 * Note: WILL ONLY WORK IF [item] IS ACTUALLY IN A LOCATION IN [location_list].
 * THE ORDER OF [location_list] ALSO CHANGES *)
let rec remove_from_locations item location_list =
  match location_list with
  | [] -> location_list
  | h::t -> if h.item=item then t else remove_from_locations item (t@[h])

(*[remove_from_inv item inv] returns a list without [item].
 * Note: WILL ONLY WORK IF [item] IS ACTUALLY IN [inv].
 * THE ORDER OF [inv] ALSO CHANGES *)
let rec remove_from_inv item inv =
  match inv with
  | [] -> inv
  | h::t -> if h=item then t else remove_from_inv item (t@[h])

(*----------------------------------------------------------------------------*)

(*REQUIRED 9. [do' c st] is state  [st'] if it is possible to do command [c] in
 * state [st] and the resulting new state would be [st'].  The
 * function name [do'] is used because [do] is a reserved keyword.
 *   - The "go" (and its shortcuts), "take" and "drop" commands
 *     either result in a new state, or are not possible because
 *     their object is not valid in state [st] hence they raise [Illegal].
 *       + the object of "go" is valid if it is a direction by which
 *         the current room may be exited
 *       + the object of "take" is valid if it is an item in the
 *         current room
 *       + the object of "drop" is valid if it is an item in the
 *         current inventory
 *       + if no object is provided (i.e., the command is simply
 *         the bare word "go", "take", or "drop") the behavior
 *         is unspecified
 *   - The "quit", "look", "inventory", "inv", "score", and "turns"
 *     commands are always possible and leave the state unchanged.
 *   - The behavior of [do'] is unspecified if the command is
 *     not one of the commands given in the assignment writeup.
 * The underspecification above is in order to enable karma
 * implementations that provide new commands.*)
let do' c st =
  let c = get_command c in
  match c with
  | Quit -> st
  | Look -> st
  | Inv -> st
  | Score -> st
  | Turns -> st
  | Go direction ->
      if ((can_exit direction st.current_room.exits) = false) then (raise Illegal) else
      let exit_room_id = get_exit_room_id direction st.current_room.exits in
      let exit_room = get_room_from_id exit_room_id st.rooms in
      if (is_visited exit_room st.visited_rooms) then

        {rooms = st.rooms;
        visited_rooms = st.visited_rooms;
        current_room = exit_room;
        items = st.items;
        inventory = st.inventory;
        locations = st.locations;
        score = st.score;
        turns = st.turns + 1}

      else

        let new_visited_rooms = exit_room::(st.visited_rooms) in
        let new_score = st.score + exit_room.points in

        {rooms = st.rooms;
        visited_rooms = new_visited_rooms;
        current_room = exit_room;
        items = st.items;
        inventory = st.inventory;
        locations = st.locations;
        score = new_score;
        turns = st.turns + 1}

  | Take item ->

    let item = get_item_from_id item st.items in
    let allowed_items = all_items_in_room st.current_room st.locations [] in
    if (can_take_drop item allowed_items = false) then (raise Illegal) else
      let point_deduction = ( if (part_of_treasure item st.current_room.treasure)
                            then item.points
                            else 0 ) in
      let new_score = st.score - point_deduction in
      let new_locations = remove_from_locations item st.locations in
      let new_inventory = item::(st.inventory) in

      {rooms = st.rooms;
        visited_rooms = st.visited_rooms;
        current_room = st.current_room;
        items = st.items;
        inventory = new_inventory;
        locations = new_locations;
        score = new_score;
        turns = st.turns + 1}

  | Drop item ->

    let item = get_item_from_id item st.items in
    let allowed_items = st.inventory in
    if (can_take_drop item allowed_items = false) then (raise Illegal) else
      let point_addition = ( if (part_of_treasure item st.current_room.treasure)
                            then
                           item.points
                           else 0 ) in
      let new_score = st.score + point_addition in
      let item_location = {item= item; room= st.current_room} in
      let new_locations = item_location::(st.locations) in
      let new_inventory = remove_from_inv item st.inventory in

      {rooms = st.rooms;
        visited_rooms = st.visited_rooms;
        current_room = st.current_room;
        items = st.items;
        inventory = new_inventory;
        locations = new_locations;
        score = new_score;
        turns = st.turns + 1}

(*----------------------------------------------------------------------------*)

let rec print_item_list (item_list:item list) =
  match item_list with
  | [] -> print_string ""
  | h::[] ->  ANSITerminal.(print_string [green] (h.id^": "^h.description^"\n") )
  | h::t -> ANSITerminal.(print_string [green] (h.id^": "^h.description ^ ", "));
            print_item_list t

let rec loop state max_not_reached=

  let command_string = read_line () in
  let command = get_command command_string in
  match command with
  | Quit -> exit 0
  | Look -> ANSITerminal.(print_string [green]
            ("\n"^ state.current_room.description ^ "\n\n"));
            loop state max_not_reached
  | Score -> ANSITerminal.(print_string [green]
            ("\n"^(string_of_int state.score)^ "\n\n") );
            loop state max_not_reached
  | Inv -> if (state.inventory = []) then
            (ANSITerminal.(print_string [red]
            ("\n"^"You don't have anything"^"\n\n")); loop state max_not_reached)
           else
            print_endline ""; print_item_list state.inventory; print_endline "";
            loop state max_not_reached
  | Turns -> ANSITerminal.(print_string [green]
            ("\n"^(string_of_int state.turns) ^ "\n\n"));
            loop state max_not_reached
  | Go _ ->
            let new_state = (try (do' command_string state) with
                             |Illegal -> ANSITerminal.(print_string [red]
                                ("\n"^"You can't do that."^"\n\n")); state) in
            let max_score_reached = (new_state.score = (max_score new_state))  in
            let max_score_message =
            "You have reached the max score! You may continue exploring or quit."
            in
            if (max_score_reached && max_not_reached) then
            (ANSITerminal.(print_string [blue] ("\n"^max_score_message^"\n\n") );
            loop new_state false)
            else if (new_state = state) then
            loop new_state max_not_reached
            else
            (ANSITerminal.( print_string [green] ( "\n"^(new_state.current_room.description)^"\n\n" ) );
            loop new_state max_not_reached)

  | Take item -> let new_state = (try (do' command_string state) with
                               |Illegal -> ANSITerminal.(print_string [red]
                                ("\n"^"You can't take that."^"\n\n"));
                                state) in
              let max_score_reached = (new_state.score = (max_score new_state)) in
              let max_score_message =
              "You have reached the max score! You may continue exploring or quit."
              in
              let dummy_item = {id= item; description=""; points=0} in
              let item_whole = ( try (get_item_from_id item new_state.items) with
                                 |Illegal -> dummy_item ) in
              if (max_score_reached && max_not_reached) then
              (ANSITerminal.(print_string [blue] ("\n"^max_score_message^"\n\n"));
              loop new_state false)
              else if (new_state = state) then
              loop new_state max_not_reached
              else
              ANSITerminal.(print_string [green] ("\nYou took the "^item^
                ".\nIt looks like "^(item_whole.description^".\n\n")) );
              loop new_state max_not_reached
  | Drop item -> let new_state = (try (do' command_string state) with
                                  |Illegal -> ANSITerminal.(print_string [red]
                                    ("\n"^"You don't have that."^"\n\n")); state) in
              let max_score_reached = (new_state.score = (max_score new_state)) in
              let max_score_message =
              "You have reached the max score! You may quit or continue exploring if you wish." in
              let dummy_item = {id= item; description=""; points=0} in
              let item_whole = ( try (get_item_from_id item new_state.items) with
                                 |Illegal -> dummy_item ) in
              if (max_score_reached && max_not_reached) then
                (ANSITerminal.(print_string [blue] ("\n"^max_score_message^"\n\n"));
                loop new_state false)
              else if (new_state = state) then
                loop new_state max_not_reached
              else if (part_of_treasure item_whole state.current_room.treasure) then
                (ANSITerminal.(print_string [green]
                ("\nYou dropped the "^item^". It seems like it belongs here.\n\n"));
                loop new_state max_not_reached)
              else
                ANSITerminal.(print_string [green] ("\nYou dropped the "^item^".\n\n"));
                loop new_state max_not_reached
(*----------------------------------------------------------------------------*)

(*REQUIRED 10. [main f] is the main entry point from outside this module
 * to load a game from file [f] and start playing it*)

let main file_name =

  let json = (try Yojson.Basic.from_file file_name with
                  | _ -> failwith "This is not an appropriate .json file" )  in
  let start_state = init_state json in

  ANSITerminal.(print_string [green]
  ("\n"^ start_state.current_room.description ^ "\n\n"));

  loop start_state true

(*----------------------------------------------------------------------------*)
