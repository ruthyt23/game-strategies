open! Core
open! Async
open! Game_strategies_common_lib

module Exercises = struct
  (* Here are some functions which know how to create a couple different kinds
     of games *)
  let empty_game = Game.empty Game.Game_kind.Tic_tac_toe
  let empty_omok = Game.empty Game.Game_kind.Omok

  let place_piece (game : Game.t) ~piece ~position : Game.t =
    let board = Map.set game.board ~key:position ~data:piece in
    { game with board }
  ;;

  let win_for_x =
    let open Game in
    empty_game
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 1 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 1 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 1 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 2 }
  ;;

  let non_win =
    let open Game in
    empty_game
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
  ;;

  let test_game = 
    let open Game in
    empty_game
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 0 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 2 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 1 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }

  let test_minimax_game = 
    let open Game in
    empty_game
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 0 }
    |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 1 }
    |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 2 }

    let diag_left_debug = 
      let open Game in
      empty_game
      |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
      |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 2 }
      |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 1 }
    let diag_right_debug = 
      let open Game in
      empty_game
      |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 0 }
      |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 2 }
      |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 1 }


    let _omok_test = 
      let open Game in
      empty_omok
      |> place_piece ~piece:Piece.X ~position:{ Position.row = 8; column = 3 }
      |> place_piece ~piece:Piece.X ~position:{ Position.row = 7; column = 4 }
      |> place_piece ~piece:Piece.X ~position:{ Position.row = 6; column = 5 }
      |> place_piece ~piece:Piece.X ~position:{ Position.row = 5; column = 6 }
      |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 0 }
      |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 1 }
      |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 2 }
      |> place_piece ~piece:Piece.O ~position:{ Position.row = 3; column = 6 }

    let serious_omok_test = 
      let open Game in
      empty_omok
      |> place_piece ~piece:Piece.X ~position:{ Position.row = 3; column = 5 }
      |> place_piece ~piece:Piece.X ~position:{ Position.row = 7; column = 4 }
      |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 9 }
      |> place_piece ~piece:Piece.X ~position:{ Position.row = 5; column = 6 }
      |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 0 }
      |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 8 }
      |> place_piece ~piece:Piece.O ~position:{ Position.row = 3; column = 13 }
      |> place_piece ~piece:Piece.O ~position:{ Position.row = 4; column = 7 }

  let position_game_array (game : Game.t)= 
    let length = Game.Game_kind.board_length game.game_kind in
    List.init length ~f:(
      fun row -> List.init length ~f:(
        fun col -> 
          {Game.Position.row = row; column = col}
      ))

  let _game_array (game : Game.t) = 
    let board = game.board in
    List.init 3 ~f:(
      fun row -> List.init 3 ~f:(
        fun col -> 
          match Map.find board {Game.Position.row = row; column = col} with 
          | Some piece -> Some piece
          | None -> None
      ))
    ;;

  let string_game_array (game : Game.t) = 
    let board = game.board in
    let length = Game.Game_kind.board_length game.game_kind in
    List.init length ~f:(
      fun row -> List.init length ~f:(
        fun col -> 
          match Map.find board {Game.Position.row = row; column = col} with 
          | Some piece -> Game.Piece.to_string (piece)
          | None -> " "
      ))
    ;;

  let print_game (game : Game.t) = 
    let game_array = string_game_array game in
    print_string(String.concat ~sep:"\n---------\n" (List.map game_array ~f:(fun row -> String.concat ~sep:" | " row ));
    )
  ;;

  let%expect_test "print_win_for_x" =
    print_game win_for_x;
    [%expect
      {|
      X | O | X
      ---------
      O | O | X
      ---------
      O | X | X
      |}];
    return ()
  ;;

  let%expect_test "print_non_win" =
    print_game non_win;
    [%expect
      {|
      X |   |
      ---------
      O |   |
      ---------
      O |   | X
      |}];
    return ()
  ;;

  let in_bounds ~board_length ~row ~column =
    let open Int.O in
    List.for_all [ row; column ] ~f:(fun x -> x >= 0 && x < board_length)

  let exists (game : Game.t) ~board_length ~row ~column = 
    in_bounds ~board_length ~row ~column && (
    match Map.find game.board {Game.Position.row = row; column} with 
    | Some _ -> true
    | None -> false )

  (* Exercise 1 *)
  let all_available_moves (game : Game.t) : Game.Position.t list = (*use map.keys to filter maybe? *)
    position_game_array game |> List.concat |> List.filter ~f:(
    fun pos -> match Map.find game.board pos with 
    | Some _ -> false
    | None -> true)
  ;;

  let%expect_test "available_moves" =
    print_s [%sexp ((all_available_moves empty_game) : Game.Position.t list)];
    [%expect
      {|
      (((row 0) (column 0)) ((row 0) (column 1)) ((row 0) (column 2)) 
       ((row 1) (column 0)) ((row 1) (column 1)) ((row 1) (column 2)) 
       ((row 2) (column 0)) ((row 2) (column 1)) ((row 2) (column 2)))
      |}];
    return ()
  ;;

  let close_available_moves (game : Game.t) = 
    let board_length = Game.Game_kind.board_length game.game_kind in
    all_available_moves game |>
    List.filter ~f:(fun {row ; column} -> (
      [row - 1, column - 1; row - 1, column; row - 1, column + 1; row, column - 1; row, column + 1; row + 1, column - 1; row + 1, column; row + 1, column + 1]
      |> List.filter ~f:(fun (row, column) -> exists game ~board_length ~row ~column)
      |> List.exists ~f:(fun (row, column) ->
        match Map.find game.board {Game.Position.row = row; column} with 
        | Some _ -> true
        | None -> false 
        )
    ))
  ;;

  let available_moves (game : Game.t) = 
    if Map.length (game.board) < (Game.Game_kind.win_length game.game_kind)
      then all_available_moves game
      else close_available_moves game

  let potential_win_paths (game : Game.t) ~row ~column : Game.Position.t list list = 
    let n = Game.Game_kind.win_length game.game_kind in
    let board_length = Game.Game_kind.board_length game.game_kind in
    let vertical = List.init n ~f:(fun index1 -> 
      List.init n ~f:(fun index2 ->
        {Game.Position.row = row + index1 - index2; column}
      )
      |> List.filter ~f:(fun {row ; column} -> exists game ~board_length:n ~row ~column)) in 
    let horizontal = List.init n ~f:(fun index1 -> 
      List.init n ~f:(fun index2 ->
        {Game.Position.row; column = column + index1 - index2}
      )
      |> List.filter ~f:(fun {row ; column} -> exists game ~board_length:n ~row ~column)) in 
    let diagonal_left = List.init n ~f:(fun index1 -> 
      List.init n ~f:(fun index2 ->
        {Game.Position.row = row + index1 - index2; column = column + index1 - index2}
      )
      |> List.filter ~f:(fun {row ; column} -> exists game ~board_length:n ~row ~column)) in 
    let diagonal_right = List.init n ~f:(fun index1 -> 
      List.init n ~f:(fun index2 ->
        {Game.Position.row = row + index2 - index1; column = column + index1 - index2}
      )
      |> List.filter ~f:(fun {row ; column} -> exists game ~board_length ~row ~column)) in 
    List.concat [vertical ; horizontal ; diagonal_left ; diagonal_right] |> List.filter ~f:(fun list -> List.length list > 1)
    
  let win_check (game : Game.t) : Game.Piece.t option = 
    let (winner : Game.Piece.t option ref) = ref None in
    let n = Game.Game_kind.win_length game.game_kind in
    List.iter (Map.keys game.board) ~f:(fun {row ; column} -> 
    potential_win_paths game ~row ~column |> List.filter ~f:(fun list -> List.length list = n) |>
    List.iter ~f:(fun row ->
      if List.for_all row ~f:(fun pos -> Game.Piece.equal (Map.find_exn game.board pos) Game.Piece.X)
        then winner.contents <- Some Game.Piece.X
      else if List.for_all row ~f:(fun pos -> Game.Piece.equal (Map.find_exn game.board pos) Game.Piece.O)
        then winner.contents <- Some Game.Piece.O
    ));
    !winner
  ;;

  (* Exercise 2 *)
  let evaluate (game : Game.t) : Game.Evaluation.t =
    if not (List.is_empty (List.filter (Map.keys game.board) ~f:(fun pos -> not (Game.Position.in_bounds pos ~game_kind:game.game_kind))))
      then Game.Evaluation.Illegal_move
    else match win_check game with
      | Some piece -> (Game.Evaluation.Game_over {winner = Some piece})
      | None -> (
        if List.is_empty (available_moves game)
          then Game.Evaluation.Game_over {winner = None}
        else Game.Evaluation.Game_continues)
  ;;

  (* Exercise 3 *)
  let winning_moves ~(me : Game.Piece.t) (game : Game.t) : Game.Position.t list =
    all_available_moves game
    |> List.filter ~f:(fun pos ->
      let (test_board : Game.t) = place_piece game ~piece:me ~position:pos in
      match evaluate test_board with
      | Game.Evaluation.Game_over {winner = Some piece} -> Game.Piece.equal piece me
      | _ -> false
    )
  ;;

  let%expect_test "winning_moves" =
    print_s [%sexp ((winning_moves ~me:Game.Piece.O test_game) : Game.Position.t list)];
    [%expect
      {|
      (((row 0) (column 1)) ((row 1) (column 0)))
      |}];
    return ()
  ;;

  (* Exercise 4 *)
  let losing_moves ~(me : Game.Piece.t) (game : Game.t) : Game.Position.t list =
    let opp = Game.Piece.flip me in
    let opps_winning_moves = winning_moves ~me:opp game in
    if List.is_empty opps_winning_moves then []
      else
    all_available_moves game |> List.filter ~f:(fun pos -> 
       not (List.mem opps_winning_moves pos ~equal:Game.Position.equal))
  ;;

  let%expect_test "losing_moves" =
    print_s [%sexp ((losing_moves ~me:Game.Piece.X test_game) : Game.Position.t list)];
    [%expect
      {|
      (((row 1) (column 2)) ((row 2) (column 1)))
      |}];
    return ()
  ;;

  (* Exercise 5: One move ahead *)

  let available_moves_that_do_not_immediately_lose (game : Game.t) ~(me : Game.Piece.t) : Game.Position.t list =
    available_moves game
    |> List.filter ~f:(fun pos ->
      not (List.mem (losing_moves game ~me) pos ~equal:Game.Position.equal || List.mem (winning_moves game ~me) pos ~equal:Game.Position.equal))
  ;;

  let%expect_test "available_moves_that_do_not_immediately_lose" =
    print_s [%sexp ((available_moves_that_do_not_immediately_lose ~me:Game.Piece.X test_game) : Game.Position.t list)];
    [%expect
      {|
      (((row 0) (column 1)) ((row 1) (column 0)))
      |}];
    return ()
  ;;

  (* Exercise 6: Minimax *)

  let score (game : Game.t) ~(me : Game.Piece.t) = 
    let x_score = ref 0 in
    let o_score = ref 0 in
    List.iter (Map.keys game.board) ~f:(fun {row ; column} -> 
    potential_win_paths game ~row ~column |> 
    List.iter ~f:(fun row ->
      if List.for_all row ~f:(fun pos -> Game.Piece.equal (Map.find_exn game.board pos) Game.Piece.X)
        then x_score.contents <- (List.length row)
      else if List.for_all row ~f:(fun pos -> Game.Piece.equal (Map.find_exn game.board pos) Game.Piece.O)
        then o_score.contents <- (List.length row)
    ));
    let mult_winning_moves = (List.length (winning_moves game ~me) - 1) * 50 in
    let mult_losing_moves = (List.length (winning_moves game ~me:(Game.Piece.flip me)) - 1) * 250 in
    match me with
    | Game.Piece.X -> !x_score - (!o_score * 5) + mult_winning_moves - mult_losing_moves
    | Game.Piece.O -> !o_score - (!x_score * 5) + mult_winning_moves - mult_losing_moves
  ;;

  let rec minimax_alg (game : Game.t) ~(me : Game.Piece.t) ~(maximizingPlayer : bool) ~(depth : int) ?(max_val = Int.min_value) () = 
    match evaluate game with
    | Game.Evaluation.Game_over {winner = Some piece} ->
      if Game.Piece.equal piece me then 500 + depth else (- 500 - depth)
    | Game.Evaluation.Game_over {winner = None} -> 0
    | _ -> (
      if depth = 0 then score game ~me
      else (
        let value = ref (score game ~me) in
        if !value < max_val
          then Int.min_value
        else (
          if maximizingPlayer
            then ( 
            List.iter (available_moves game) ~f:(fun pos -> 
              let new_board = place_piece game ~piece:me ~position:pos in 
              value.contents <- (max !value (minimax_alg new_board ~me ~maximizingPlayer:false ~depth:(depth - 1) ~max_val () ))
            );
            !value)
          else (
            List.iter (available_moves game) ~f:(fun pos -> 
              let new_board = place_piece game ~piece:me ~position:pos in 
              value.contents <- (min !value (minimax_alg new_board ~me ~maximizingPlayer:true ~depth:(depth - 1) ~max_val ()))
            );
            !value
            )
        )
      )
    )
  ;;

  let minimax (game : Game.t) ~(me : Game.Piece.t) = 
    let max_pos = ref Game.Position.{row = 0; column = 0} in
    let max_val = ref Int.min_value in
    let win_moves = winning_moves game ~me in
    let lose_moves = winning_moves game ~me:(Game.Piece.flip me) in
    if not (List.is_empty win_moves) 
      then (List.hd_exn win_moves)
    else if not (List.is_empty lose_moves) 
      then (List.hd_exn lose_moves)
    else if (Map.is_empty game.board)
      then (let n = Game.Game_kind.board_length game.game_kind / 2 in Game.Position.{row = n; column = n})
    else (
      List.iter (available_moves game) ~f:(fun pos -> 
      let new_board = place_piece game ~piece:me ~position:pos in 
      let value = match game.game_kind with 
        | Game.Game_kind.Omok -> minimax_alg new_board ~me ~maximizingPlayer:true ~depth:2 ~max_val:!max_val  ()
        | Game.Game_kind.Tic_tac_toe -> minimax_alg new_board ~me ~maximizingPlayer:true ~depth:5 ()
      in
      if value > !max_val then max_pos.contents <- pos; max_val.contents <- value;
      );
    !max_pos)
  ;;

  let exercise_one =
    Command.async
      ~summary:"Exercise 1: Where can I move?"
      (let%map_open.Command () = return () in
       fun () ->
         let moves = available_moves win_for_x in
         print_s [%sexp (moves : Game.Position.t list)];
         let moves = available_moves non_win in
         print_s [%sexp (moves : Game.Position.t list)];
         return ())
  ;;

  let exercise_two =
    Command.async
      ~summary:"Exercise 2: Is the game over?"
      (let%map_open.Command () = return () in
       fun () ->
         let evaluation = evaluate win_for_x in
         print_s [%sexp (evaluation : Game.Evaluation.t)];
         let evaluation = evaluate non_win in
         print_s [%sexp (evaluation : Game.Evaluation.t)];
         return ())
  ;;

  let piece_flag =
    let open Command.Param in
    flag
      "piece"
      (required (Arg_type.create Game.Piece.of_string))
      ~doc:
        ("PIECE "
         ^ (Game.Piece.all |> List.map ~f:Game.Piece.to_string |> String.concat ~sep:", ")
        )
  ;;

  let exercise_three =
    Command.async
      ~summary:"Exercise 3: Is there a winning move?"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         let winning_moves = winning_moves ~me:piece non_win in
         print_s [%sexp (winning_moves : Game.Position.t list)];
         return ())
  ;;

  let exercise_four =
    Command.async
      ~summary:"Exercise 4: Is there a losing move?"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         let losing_moves = losing_moves ~me:piece non_win in
         print_s [%sexp (losing_moves : Game.Position.t list)];
         return ())
  ;;

  let exercise_five =
    Command.async
      ~summary:"Exercise 5: One move ahead"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         let available_moves_no_immediate_loss = available_moves_that_do_not_immediately_lose ~me:piece non_win in
         print_s [%sexp (available_moves_no_immediate_loss : Game.Position.t list)];
         return ())
  ;;

  let exercise_six =
    Command.async
      ~summary:"Exercise 6: Minimax"
      (let%map_open.Command () = return ()
       and piece = piece_flag in
       fun () ->
         let minimax_move = minimax ~me:piece test_minimax_game in
         print_s [%sexp (minimax_move : Game.Position.t)];
         return ())
  ;;

  let diagonal_debug =
    Command.async
      ~summary:"Diagonal Debug"
      (let%map_open.Command () = return () in
       fun () ->
         let diag_left_debug = winning_moves diag_left_debug ~me:Game.Piece.X in
         let diag_right_debug = winning_moves diag_right_debug ~me:Game.Piece.X in
         print_s [%sexp (diag_left_debug : Game.Position.t list)];
         print_s [%sexp (diag_right_debug : Game.Position.t list)];
         return ())
  ;;

  let omok_test =
    Command.async
      ~summary:"Omok Test"
      (let%map_open.Command () = return () in
       fun () ->
          let start = Time_ns_unix.now () in
         let omok_test = minimax serious_omok_test ~me:Game.Piece.X in
         print_s [%sexp (omok_test : Game.Position.t)];
         let stop = Time_ns_unix.now () in
         let time_elapsed = Time_ns_unix.diff stop start in
        Core.printf !"Finished in %{Time_ns_unix.Span}\n%!" time_elapsed;
         return ())
  ;;

  let command =
    Command.group
      ~summary:"Exercises"
      [ "one"  , exercise_one
      ; "two"  , exercise_two
      ; "three", exercise_three
      ; "four" , exercise_four
      ; "five" , exercise_five
      ; "six" , exercise_six
      ; "diag-debug", diagonal_debug
      ; "omok-test", omok_test
      ]
  ;;
end

let handle (_client : unit) (query : Rpcs.Take_turn.Query.t) =
  let game, piece = query.game, query.you_play in
  let response = {Rpcs.Take_turn.Response.piece = piece ; Rpcs.Take_turn.Response.position = (Exercises.minimax game ~me:piece)} in
  return response
;;

let implementations =
  Rpc.Implementations.create_exn
    ~on_unknown_rpc:`Close_connection
    ~implementations:[ Rpc.Rpc.implement Rpcs.Take_turn.rpc handle]
;;

let command_play =
  Command.async
    ~summary:"Play"
    (let%map_open.Command () = return ()
     and port = flag "-port" (required int) ~doc:"_ port to listen on" in
     fun () ->
      let%bind server =
           Rpc.Connection.serve
             ~implementations
             ~initial_connection_state:(fun _client_identity _client_addr ->
               (* This constructs the "client" values which are passed to the
                  implementation function above. We're just using unit for now. *)
               ())
             ~where_to_listen:(Tcp.Where_to_listen.of_port port)
             ()
         in
         Tcp.Server.close_finished server)
;;

let command =
  Command.group
    ~summary:"Game Strategies"
    [ "play", command_play; "exercises", Exercises.command ]
;;
