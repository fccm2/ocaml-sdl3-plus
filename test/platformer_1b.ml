(* Copyright (C) 2024 Florent Monnier *)
(*
 To the extent permitted by law, you can use, modify, and redistribute
 this software, and the associated elements, as long as you also respect
 the distribution agreements of these associated elements.
*)

open Sdl3
let width, height = (560, 320)

let tex_loaded = ref false

type color = [ `white | `red | `green | `blue | `yellow ]

type player = {
  x: int;
  y: int;
  d: int;  (* tile-number *)
  v: int;  (* vertical-velocity *)
  c: color;
  u: unit;
}

type ennemy = {
  e_x: int;
  e_y: int;
  e_d: int;  (* tile-number *)
  e_2x: int * int;  (* left-right pos *)
  e_v: int;  (* vector to move horizontaly *)
}

type tile = int * int * int   (* (x, y, tile-number) *)
type rect = int * int * int * int   (* (x, y, w, h) *)

type game_state = {
  player: player;
  ennemies: ennemy list;
  tiles: tile list;
  collidables: rect list;
  color_switch: (rect * color) list;
  collectibles: (tile * rect * color) list;
  got: (tile * rect * int) list;
  dummy: unit;
}

(* Draw *)

let fill_color r color =
  (*
  let r, g, b = color in
  Render.set_draw_color r (r, g, b);
  *)
  Render.set_draw_color r (color);
  ()
;;


let new_ennemy (x, y) n = {
  e_x = x * 16;
  e_y = y * 16;
  e_d = [| 323; 360 |].(Random.int 2);  (* 380: monster with helice *)
  e_2x = (x * 16, (x + n) * 16);
  e_v = 1;
}

let update_ennemy (e) =
  let x1, x2 = e.e_2x in
  let x = e.e_x + e.e_v in
  let x, v =
    if x > x2 then (x2, - e.e_v) else
    if x < x1 then (x1, - e.e_v) else
    (x, e.e_v)
  in
  { e with
    e_x = x;
    e_v = v;
  }

(* Init-Level *)

let ( @ ) = List.rev_append ;;
let xe = ref 0

let mk_level () =  (* mk_level *)
  Random.self_init ();
  let enn = ref [] in
  let push_en en = enn := en :: !enn in

  let new_ground x1 y1 () =  (* new_ground *)
    let n = ((Random.int 10) + 1) in
    let ps =
      if n = 1 then [273] else
      List.init n (fun i ->
        if i = 0 then 270 else if i+1 = n then 272 else 271)
    in
    let y2 = 17 + ((Random.int 5) - 2) in  (* new-y *)
    let x2 = x1 + ((Random.int 4) - 1) in
    let x2 = if y1 = y2 then x1 + 1 else x2 in
    let x2 = if x1 = 0 then -1 else x2 in
    if n <> 1 && Random.int 4 = 0 then
      push_en (new_ennemy (x2, y2-1) (n-1));
    let ps, x2 =
      List.fold_left (fun (acc, x) d ->
        let acc = ((x, y2, d) :: acc) in
        (acc, x+1)
      ) ([], x2) ps
    in
    (ps, x2, y2)
  in
  let rec add_grounds ~n x y acc =  (* add_grounds *)
    if n <= 0 then List.rev acc else
    let pl, x, y = new_ground x y () in
    add_grounds ~n:(n-1) x y (pl @ acc)
  in
  let ps = add_grounds ~n:56 0 18 [] in  (* n: number of ground-platforms *)
                                         (* (length of the level) *)

  let add_vertical ps =  (* add_vertical *)
    let _ps = List.map (fun (x, y, d) -> (x, y)) ps in
    let rec fill d (x, y) acc =
      if List.mem (x, y) _ps || y > 19 then acc else
        let p = (x, y, d) in
        fill d (x, y+1) (p::acc)
    in
    let rec aux acc ps =
      match ps with
      | (x, y, 270) :: ps ->
          let acc = fill 290 (x, y+1) acc in
          aux acc ps
      | (x, y, 272) :: ps ->
          let acc = fill 292 (x, y+1) acc in
          aux acc ps
      | (x, y, 273) :: ps ->
          let acc = fill 293 (x, y+1) acc in
          aux acc ps
      | (x, y, _) :: ps -> aux acc ps
      | [] -> List.rev acc
    in
    aux [] ps
  in
  let ws = add_vertical ps in

  (* d. *)
  let li = ref [] in
  let push x = li := x :: !li in
  begin
    List.iter (fun (x, y, d) ->
      if Random.int 5 = 0 then
        let d = [| 416; 417; 418; 436; 437; 438; |].(Random.int 6) in
        push (x, y-1, d);
    ) ps;
  end;
  let ps = !li @ ps in
  let ps = ws @ ps in

  (* scene-elements *)
  let _ps = List.map (fun (x, y, d) -> (x, y)) ps in
  xe := List.fold_left (fun xm (x, _) -> max x xm) 0 _ps;

  let rec add_scene x acc =
    let x = x + 4 + Random.int 8 in
    if x > !xe then acc else
    let y = 0 in
    begin
      match Random.int 12 with
      | 0 ->
          let s3 = (x,   y,   431) in
          let s4 = (x+2, y,   433) in
          let s1 = (x+1, y+1, 412) in
          let s5 = (x,   y+1, 451) in
          let s6 = (x+2, y+1, 453) in
          add_scene (x) (s1::s3::s4::s5::s6::acc)
      | 1 ->
          let s0 = (x+2, y+1, 452) in
          let s3 = (x,   y,   431) in
          let s4 = (x+3, y,   433) in
          let s1 = (x+1, y+1, 412) in
          let s5 = (x,   y+1, 451) in
          let s6 = (x+3, y+1, 453) in
          add_scene (x) (s0::s1::s3::s4::s5::s6::acc)
      | 2 ->
          let s1 = (x,   y, 451) in
          let s2 = (x+2, y, 412) in
          let s3 = (x+1, y, 452) in
          let s4 = (x+3, y, 453) in
          add_scene (x) (s1::s2::s3::s4::acc)
      | 3 ->
          let y = Random.int 6 in
          let s1 = (x,   y+2, 407) in
          let s2 = (x+1, y+2, 408) in
          let s3 = (x+1, y+3, 428) in
          let s4 = (x+0, y+3, 427) in
          add_scene (x) (s1::s2::s3::s4::acc)
      | 4 | 5 ->  (* platform:3 *)
          let y = 6 + Random.int 8 in
          let s1 = (x,   y+0, 124) in
          let s2 = (x+1, y+0, 125) in
          let s3 = (x+2, y+0, 126) in
          add_scene (x) (s1::s2::s3::acc)
      | 6 | 7 ->  (* platform:4 *)
          let y = 6 + Random.int 8 in
          let s1 = (x,   y+0, 124) in
          let s2 = (x+1, y+0, 125) in
          let s3 = (x+2, y+0, 125) in
          let s4 = (x+3, y+0, 126) in
          add_scene (x) (s1::s2::s3::s4::acc)
      | d -> (* put color switchers *)
          let y = 7 + Random.int 7 in
          let s1 = (x, y, 547 + d - 8) in
          add_scene (x) (s1::acc)
    end
  in
  let ds = add_scene 0 [] in
  let ps = ds @ ps in

  (* add_collectibles *)
  let _ps = List.map (fun (x, y, d) -> (x, y)) ps in
  let take_col () = [|
      (`yellow, 567);
      (`red,    568);
      (`green,  569);
      (`blue,   570);
    |].(Random.int 4)
  in
  let rec add_collectibles acc x =
    let x = x + 2 + Random.int 12 in
    if x > !xe - 8 then acc else
    let y = 7 + Random.int 6 in
    let c, d = take_col () in
    let rec aux acc x n =
      if n <= 0 || List.mem (x, y) _ps then (acc, x)
      else aux ((x, y, c, d)::acc) (x+1) (n-1)
    in
    let cl, x = aux [] x (1 + Random.int 5) in
    add_collectibles (cl @ acc) x
  in
  let cs = add_collectibles [] 6 in
  let cs =
    List.map (fun (x, y, c, d) ->
      ((x, y, d), (x * 16, y * 16, 16, 16), c)
    ) cs
  in
  (ps, !enn, cs)



(* Display *)

let display r t state () =
  begin
    let player = state.player in
    let ennemies = state.ennemies in
    let level = state.tiles in

    (* Draw background *)
    fill_color r (102, 102, 136);
    Render.clear r;

    (* Scrolling *)
    let (tx, ty) =
      let x = player.x in
      let y = player.y in
      let _ = y in
      let hw = width / 2 in
      let _x = if x > hw then x - hw else 0 in
      let dx = x - (!xe * 16 - hw + 16) in
      let _x = if dx > 0 then _x - dx else _x in
      ((- _x), 0)   (* translate *)
    in

    let get_tile d = (16 * (d mod 20), 16 * (d / 20)) in

    (* Draw level-tiles *)
    let draw_tile (x, y, d) =
      let (sw, sh) = (16, 16) in
      let (sx, sy) = get_tile d in
      let (dx, dy) = (16 * x, 16 * y) in
      let (dw, dh) = (sw * 1, sh * 1) in
      let dx, dy = dx+tx, dy+ty in
      Render.render_texture r t (sx, sy, sw, sh) (dx, dy, dw, dh);
    in
    List.iter (fun tl -> draw_tile tl) level;

    (* Draw ennemies *)
    let draw_enn (e) =
      let (sw, sh) = (16, 16) in
      let (sx, sy) = get_tile e.e_d in
      let (dx, dy) = (e.e_x, e.e_y) in
      let (dw, dh) = (sw * 1, sh * 1) in
      let dx, dy = dx+tx, dy+ty in
      Render.render_texture r t (sx, sy, sw, sh) (dx, dy, dw, dh);
    in
    List.iter (fun en -> draw_enn en) ennemies;

    (* Draw collectibles *)
    let draw_coll ((_, _, d), rct, _) =
      let (x, y, w, h) = rct in
      let (sw, sh) = (w, h) in
      let (sx, sy) = get_tile d in
      let (dx, dy) = (x, y) in
      let (dw, dh) = (sw * 1, sh * 1) in
      let dx, dy = dx+tx, dy+ty in
      Render.render_texture r t (sx, sy, sw, sh) (dx, dy, dw, dh);
    in
    List.iter (fun en -> draw_coll en) state.collectibles;

    (* Draw got-collectibles *)
    let draw_coll ((_, _, d), rct, _) =
      let (x, y, w, h) = rct in
      let (sw, sh) = (w, h) in
      let (sx, sy) = get_tile d in
      let (dx, dy) = (x, y) in
      let (dw, dh) = (sw * 1, sh * 1) in
      let dx, dy = dx+tx, dy+ty in
      Render.render_texture r t (sx, sy, sw, sh) (dx, dy, dw, dh);
    in
    List.iter (fun en -> draw_coll en) state.got;

    (* Draw player *)
    begin
      let x = player.x in
      let y = player.y in
      let d = player.d in
      let (sx, sy) = get_tile d in
      let (sw, sh) = (16, 16) in
      let (dx, dy) = (x, y) in
      let (dw, dh) = (sw * 1, sh * 1) in
      let dx, dy = dx+tx, dy+ty in
      Render.render_texture r t (sx, sy, sw, sh) (dx, dy, dw, dh);
    end;
    (*
    Canvas.restore ctx;
    *)

    (* Number got *)
    begin
      (*
      fill_color (255, 255, 255);
      let s = Printf.sprintf "coins: %d" (List.length state.collectibles) in
      Canvas.font ctx "bold 12px Arial";
      Canvas.fillText ctx s (width - 68) (16);
      *)
      ()
    end;
  end;
  (*
  else begin
    (*
    fill_color (102, 102, 136);
    draw_rect (0, 0) (width, height);
    fill_color (60, 60, 80);
    let s = "loading..." in
    Canvas.font ctx "bold 16px Arial";
    Canvas.fillText ctx s (width / 4) (height / 3);
    *)
    ()
  end;
  *)

  Render.present r;
  ()
;;

(* Events *)

type key_change = KeyDown | KeyUp

type keys = {
  mutable left : bool;
  mutable right : bool;
  mutable up : bool;
  mutable down : bool;
}

let keys = {
  left = false;
  right = false;
  up = false;
  down = false;
}


(* Collidables *)

let collides = [ 270; 271; 272; 273; 290; 292; 293; 124; 125; 126;
                 547; 548; 549; 550; ]

let switches = [ 547; 548; 549; 550; ]

let level_collidables level =
  let to_tile (x, y, d) =
    if not(List.mem d collides) then None else
    Some(x * 16, y * 16, 16, 16)
  in
  List.fold_left (fun acc tl ->
    match to_tile tl with None -> acc | Some tl -> tl::acc) [] level


let tile_of_color = function
  | `yellow -> 500
  | `red    -> 440
  | `green  -> 480
  | `blue   -> 540
  | `white  -> 260


let color_switch level =
  let to_color d =
    match d with
    | 547 -> `yellow
    | 548 -> `red
    | 549 -> `green
    | 550 -> `blue
    | _ -> assert false
  in
  let to_rect (x, y, d) =
    if not(List.mem d switches) then None else
    let (sw, sh) = (16, 16) in
    let (dx, dy) = (16 * x, 16 * y) in
    let (dw, dh) = (sw * 1, sh * 1) in
    Some((dx, dy, dw, dh), to_color d)
  in
  List.fold_left (fun acc tl ->
    match to_rect tl with None -> acc | Some tl -> tl::acc) [] level


let check_switches level =
  List.for_all (fun sd ->
    List.exists (fun (_, _, d) -> d = sd) level
  ) switches

let mk_level () = (* checks that there is a switcher for every color *)
  let rec aux () =
    let level, ennemies, collectibles = mk_level () in
    if check_switches level then (level, ennemies, collectibles)
    else aux ()
  in
  aux ()



let rect_collide (x1, y1, w1, h1) (x2, y2, w2, h2) =
  if x1 >= x2 + w2 then false else
  if y1 >= y2 + h2 then false else
  if x2 >= x1 + w1 then false else
  if y2 >= y1 + h1 then false else
  true

let rect_touch (x1, y1, w1, h1) (x2, y2, w2, h2) =
  if x1 > x2 + w2 then false else
  if y1 > y2 + h2 then false else
  if x2 > x1 + w1 then false else
  if y2 > y1 + h1 then false else
  true


(* Updates *)

let update_player state () =
  let ennemies = state.ennemies in
  let player = state.player in
  let x = player.x in
  let y = player.y in

  (* Update player's vertical velocity *)
  let v = player.v in
  let v = if keys.up then v - 8 else v in (* jump *)
  let v = v + 1 in
  let v = min v 12 in
  let v = max v (-10) in

  (* Move left/right *)
  let x2 = if keys.left then x - 4 else if keys.right then x + 4 else x in
  let y2 = y + v in
  let enn_to_tile e = (e.e_x, e.e_y, 16, 16) in

  (* X-Loop *)
  let x, coll_e =
    let dx = if x2 < x then -1 else 1 in
    let rec x_loop x =
      let _x = x + dx in
      let coll_e =
        (* collisions with ennemies along x-axis *)
        List.exists (fun enn ->
          rect_collide (enn_to_tile enn) (_x, y, 16, 16)
        ) ennemies
      in
      if x = x2 then x, coll_e else
        (* collisions with collidables along x-axis *)
        match
          List.exists (fun tile ->
            rect_collide tile (_x, y, 16, 16)
          ) state.collidables
          , coll_e
        with
        | true, false -> x, false
        | _, true -> x, true
        | false, false -> x_loop _x
    in
    x_loop x
  in

  (* Y-Loop *)
  let y, v, ennemies =
    let dy = if y2 < y then -1 else 1 in
    let rec y_loop y es =
      if y = y2 then (y, v, es) else
        let _y = y + dy in
        (* collisions with collidables along y-axis *)
        if List.exists (fun tile ->
             rect_collide tile (x, _y, 16, 16)
           ) state.collidables
        then (y, 0, es)
        else 
        match (* catch ennemies by jumping onto them *)
          List.fold_left (fun (acc1, acc2) enn ->
            if rect_collide (enn_to_tile enn) (x, _y, 16, 16) && dy = 1
            then (enn::acc1, acc2)
            else (acc1, enn::acc2)
          ) ([], []) es
        with
        | [], es -> y_loop _y es
        | _, es -> (y, v, es)
    in
    y_loop y ennemies
  in

  (* Color-Switchers *)
  let c =
    List.fold_left (fun c (rect, _c) ->
      if rect_touch rect (x, y, 16, 16) then _c else c
    ) player.c state.color_switch
  in
  let c = if coll_e then `white else c in
  let d = tile_of_color c in

  (* Catch collectibles *)
  let collectibles, got =
    List.fold_left (fun (acc1, acc2) ((tile, rect, color) as cl) ->
      if rect_touch rect (x, y, 16, 16)
      && color = c
      then (acc1, (tile, rect, -8)::acc2)
      else (cl::acc1, acc2)
    ) ([], state.got) state.collectibles
  in

  (* Anim collectibles *)
  let got =
    List.map (fun (tile, (x, y, w, h), v) ->
      let v = v + 1 in
      let y = y + v in
      (tile, (x, y, w, h), v)
    ) got
  in
  let got =
    List.filter (fun (_, (_, y, _, _), _) -> (y < height)) got
  in

  let y, c = if y > height then (-16, `white) else y, c in  (* falling: wrap to top *)
  let x = if x < 0 then 0 else x in
  let x = if x > !xe * 16 then !xe * 16 else x in

  let player = { player with x; y; c; d; v } in

  { state with player; ennemies; got; collectibles; }



let update_state state () =
  let state = update_player state () in
  let ennemies = List.map update_ennemy state.ennemies in
  { state with ennemies }


(* Init game-state *)

let init_game_state () =
  let level, ennemies, collectibles = mk_level () in
  let collidables = level_collidables level in
  let color_switch = color_switch level in
  let player =
    { x = 4;
      y = 16 * 6;
      v = 0;
      d = 540;  (* 260:white, 440:red, 480:green, 500:yellow, 540:blue *)
      c = `white;
      u = ();
    }
  in
  { player;
    ennemies;
    tiles = level;
    collidables;
    collectibles;
    color_switch;
    got = [];
    dummy = ();
  }


(* Init *)

(*
 * File: "./1bitplatformerpack.bmp"
 * From: https://opengameart.org/content/1-bit-platformer-pack
 * Graphics Author: https://opengameart.org/users/kenney
 * License: CC0
 *)

let init_data rndr () =
  let surf = Surface.load_bmp ~file:"./1bitplatformerpack.bmp" in
  Surface.set_color_key surf (255, 0, 255);
  let tex = Texture.create_texture_from_surface rndr surf in
  (tex)


(* Key Change *)

let key_event m c =
  match m, c with
  (*
  | KeyDown, Scancode.A -> Printf.printf " A \n%!";
  *)
  | KeyDown, Scancode.Escape -> Sdl3.quit (); exit 0

  | KeyDown, Scancode.Left  -> (keys.left  <- true)  (* Left *)
  | KeyDown, Scancode.Right -> (keys.right <- true)  (* Right *)
  | KeyDown, Scancode.Up    -> (keys.up    <- true)  (* Up *)
  | KeyDown, Scancode.Down  -> (keys.down  <- true)  (* Down *)

  | KeyUp, Scancode.Left  -> (keys.left  <- false)  (* Left *)
  | KeyUp, Scancode.Right -> (keys.right <- false)  (* Right *)
  | KeyUp, Scancode.Up    -> (keys.up    <- false)  (* Up *)
  | KeyUp, Scancode.Down  -> (keys.down  <- false)  (* Down *)

  | _ -> ()



(* Main *)

let () =
  Sdl3.init ();
  Random.self_init ();
  let win, rndr = Render.create_window_and_renderer (width, height) in

  (* Load the sprite sheet *)
  let tex = init_data rndr () in

  (* Load a new state *)
  let state = init_game_state () in

  (* Event loop *)
  let rec event_loop () =
    let e = Events.poll_event () in
    match e with
    | None -> ()
    | Some ev ->
        match ev with
        | Quit_event -> Sdl3.quit (); exit 0
        | Key_up_event c -> key_event KeyUp c; event_loop ()
        | Key_down_event c -> key_event KeyDown c; event_loop ()
        | _ -> event_loop ()
  in

  (* Run the main loop *)
  let rec aux (state) =
    event_loop ();  (* event-loop *)
    display rndr tex state (); (* display *)
    let state = update_state state () in (* update *)
    Timer.delay (1000 / 30);
    aux (state)
  in
  aux (state)
;;

