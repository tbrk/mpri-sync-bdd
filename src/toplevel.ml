(* Adapted from the
 * Js_of_ocaml toplevel
 * distributed with ocsigen/js_of_ocaml/toplevel/examples/lwt_toplevel_lib
 *
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2011 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

open Js_of_ocaml
open Js_of_ocaml_lwt
open Js_of_ocaml_tyxml
open Js_of_ocaml_toplevel
open Lwt

module Html = Dom_html

let compiler_name = "OCaml"

let by_id s = Html.getElementById s
let by_id_coerce s f  =
  Js.Opt.get (f (Html.getElementById s)) (fun () -> raise Not_found)
let do_by_id s f =
  try f (Html.getElementById s) with Not_found -> ()

let js = Js.string
let document = Html.window##.document

(* * * Toplevel *)

(* load file using a synchronous XMLHttpRequest *)
let load_resource_aux filename url =
  Js_of_ocaml_lwt.XmlHttpRequest.perform_raw
    ~response_type:XmlHttpRequest.ArrayBuffer url
  >|= fun frame ->
  if frame.Js_of_ocaml_lwt.XmlHttpRequest.code = 200
  then
    Js.Opt.case frame.Js_of_ocaml_lwt.XmlHttpRequest.content
      (fun () -> Printf.eprintf "Could not load %s\n" filename)
      (fun b  ->
         Sys_js.update_file ~name:filename
                            ~content:(Typed_array.String.of_arrayBuffer b))
  else ()

let load_resource scheme ~prefix ~path:suffix =
  let url = scheme ^ suffix in
  let filename = Filename.concat prefix suffix in
  Lwt.async (fun () -> load_resource_aux filename url);
  Some ""

let setup_pseudo_fs () =
  Sys_js.mount ~path:"/dev/"   (fun ~prefix:_ ~path:_ -> None);
  Sys_js.mount ~path:"/http/"  (load_resource "http://");
  Sys_js.mount ~path:"/https/" (load_resource "https://");
  Sys_js.mount ~path:"/ftp/"   (load_resource "ftp://");
  Sys_js.mount ~path:"/home/"  (load_resource "filesys/")

let exec' s =
  let res : bool = JsooTop.use Format.std_formatter s in
  if not res then Format.eprintf "error while evaluating %s@." s

let setup_toplevel () =
  JsooTop.initialize ();
  Sys.interactive := false;
  exec' ("module Lwt_main = struct
             let run t = match Lwt.state t with
               | Lwt.Return x -> x
               | Lwt.Fail e -> raise e
               | Lwt.Sleep -> failwith \"Lwt_main.run: thread didn't return\"
            end");
  let header1 =
      Printf.sprintf "        %s version %%s" compiler_name in
  let header2 = Printf.sprintf
      "     Compiled with Js_of_ocaml version %s" Sys_js.js_of_ocaml_version in
  exec' (Printf.sprintf "Format.printf \"%s@.\" Sys.ocaml_version;;" header1);
  exec' (Printf.sprintf "Format.printf \"%s@.\";;" header2);
  exec' ("#enable \"pretty\";;");
  exec' ("#disable \"shortvar\";;");
  Hashtbl.add Toploop.directive_table "load_js"
    (Toploop.Directive_string
       (fun name -> Js.Unsafe.global##load_script_ name));
  Sys.interactive := true;
  ()

let resize ~container ~textbox ()  =
  Lwt.pause () >>= fun () ->
  textbox##.style##.height := js "auto";
  textbox##.style##.height := js
      (Printf.sprintf "%dpx" (max 18 textbox##.scrollHeight));
  container##.scrollTop := container##.scrollHeight;
  Lwt.return ()

let setup_printers () =
  exec'("let _print_error fmt e = Format.pp_print_string fmt (Js_of_ocaml.Js.string_of_error e)");
  Topdirs.dir_install_printer Format.std_formatter
    (Longident.(Lident "_print_error"));
  exec'("let _print_unit fmt (_ : 'a) : 'a = Format.pp_print_string fmt \"()\"");
  Topdirs.dir_install_printer Format.std_formatter
    (Longident.(Lident "_print_unit"))

let setup_examples ~container ~textbox =
  let r = Regexp.regexp "^\\(\\*+(.*)\\*+\\)$" in
  let all = ref [] in
  begin try
    let ic = open_in "/static/examples.ml" in
    while true do
      let line = input_line ic in
      match Regexp.string_match r line 0 with
      | Some res ->
         let name = match Regexp.matched_group res 1 with
                    | Some s -> s
                    | None -> assert false in
         all := `Title name :: !all
      | None -> all := `Content line :: !all
    done;
    assert false
  with  _ -> () end;
  let example_container = by_id "toplevel-examples" in
  let _ = List.fold_left (fun acc tok ->
      match tok with
      | `Content line -> line ^ "\n" ^ acc
      | `Title   name ->
      let a = Tyxml_js.Html.(a ~a:[
        a_class ["list-group-item"];
        a_onclick (fun _ ->
          textbox##.value := (js acc)##trim;
            Lwt.async(fun () ->
              resize ~container ~textbox ()  >>= fun () ->
              textbox##focus;
              Lwt.return_unit);
            true
         )] [pcdata name]) in
      Dom.appendChild example_container (Tyxml_js.To_dom.of_a a);
      ""
    ) "" !all in
  ()

let rec iter_on_sharp ~f x =
  Js.Opt.iter (Html.CoerceTo.element x)
      (fun e -> if Js.to_bool (e##.classList##contains (js "sharp"))
                then f e);
  match Js.Opt.to_option x##.nextSibling with
  | None -> ()
  | Some n -> iter_on_sharp ~f n

let current_position = ref 0
let highlight_location loc =
  let x = ref 0 in
  let output = by_id "output" in
  let first = Js.Opt.get (output##.childNodes##item(!current_position))
                         (fun _ -> assert false)
  in
  iter_on_sharp first
    ~f:(fun e ->
     incr x;
     let _file1,line1,col1 = Location.get_pos_info (loc.Location.loc_start) in
     let _file2,line2,col2 = Location.get_pos_info (loc.Location.loc_end) in
     if !x >= line1 && !x <= line2
     then
       let from_ = if !x = line1 then `Pos col1 else `Pos 0 in
       let to_   = if !x = line2 then `Pos col2 else `Last in
       Colorize.highlight from_ to_ e)

let append colorize output cl s =
  Dom.appendChild output (Tyxml_js.To_dom.of_element (colorize ~a_class:cl s))

module History = struct
  let data = ref [|""|]
  let idx = ref 0
  let get_storage () =
    match Js.Optdef.to_option Html.window##.localStorage with
    | exception _ -> raise Not_found
    | None -> raise Not_found
    | Some t -> t

  let setup () =
    try
      let s = get_storage () in
      match Js.Opt.to_option (s##getItem (js "history")) with
      | None -> raise Not_found
      | Some s -> let a = Json.unsafe_input s in
                  data:=a; idx:=Array.length a - 1
    with _ -> ()

  let push text =
    let l = Array.length !data in
    let n = Array.make (l + 1) "" in
    Array.set  !data (l - 1) text;
    Array.blit !data 0 n 0 l;
    data := n; idx := l;
    try
      let s = get_storage () in
      let str = Json.output !data in
      s##setItem (js "history") str
    with Not_found -> ()

  let current text = !data.(!idx) <- text
  let previous textbox =
    if !idx > 0
    then begin decr idx; textbox##.value := js (!data.(!idx)) end
  let next textbox =
    if !idx < Array.length !data - 1
    then begin incr idx; textbox##.value := js (!data.(!idx)) end
end

(* * * Editor *)

let do_indent editor =
  let p = Ace.get_cursor_position editor in
  let r = Ace.get_selection_range editor in
  let r_start = r##.start##.row + 1 in
  let r_last = r##.end_##.row + 1 in
  let s = Ocp_indent.indent (Ace.get_contents editor)
                            (fun x -> r_start <= x && x <= r_last) in
  Ace.set_contents editor s;
  let lcol = String.length (Ace.get_line editor p##.row) in
  Ace.(set_cursor_position editor (create_position p##.row lcol))

let blank_line = Regexp.regexp "^[ \t]*$"
let is_blank s = Regexp.string_match blank_line s 0 <> None

let find_blank editor delta limit i =
  let rec go i =
    let i' = i + delta in
    if i' = limit then limit
    else if is_blank (Ace.get_line editor i') then i
    else go i'
  in
  if i = limit || is_blank (Ace.get_line editor i) then i
  else go i

let select_paragraph editor =
  let r = Ace.get_selection_range editor in
  let c_start = r##.start in
  let c_end   = r##.end_ in
  (* First try to select a paragraph. *)
  let r_start = Ace.create_position
                  (find_blank editor (-1) 0 c_start##.row) 0
  in
  let r_end_row = find_blank editor (+1) (Ace.get_length editor) c_end##.row in
  let r_end   = Ace.(create_position r_end_row
                       (String.length (get_line editor r_end_row))) in

  let newsel =
    if not (Ace.position_equal r_start c_start)
    then Ace.create_range r_start r_end
    (* If the selection is unchanged, expand it *)
    else if c_start##.row = 0 && c_start##.column = 0
    then Ace.(create_range (create_position 0 0) (get_last editor))
    else Ace.(create_range (create_position 0 0) r_end)
  in
  Ace.set_selection_range editor newsel

let send_selection execute editor =
  let c = Ace.get_selection_range editor in
  let r, reset_cursor =
    if Ace.position_equal c##.start c##.end_
    then let () = select_paragraph editor
         in (Ace.get_selection_range editor, true)
    else (c, false)
  in
  Lwt.(async (fun () ->
         execute (Ace.get_contents ~range:r editor)
         >>= (fun () -> if reset_cursor then
                          Ace.set_selection_range editor c;
                        Ace.focus editor;
                        return ())))

let start_editor focus_terminal execute =
  let editor_div = by_id "editor" in
  let editor = Ace.create_editor ~show_gutter:false editor_div in
  Ace.set_mode editor "ace/mode/ocaml";
  Ace.set_theme editor "ace/theme/eclipse";
  Ace.set_tab_size editor 2;
  Ace.add_keybinding editor "indent" "Tab"        do_indent;
  Ace.add_keybinding editor "select" "Ctrl-Space" select_paragraph;
  Ace.add_keybinding editor "send"   "Ctrl-Enter" (send_selection execute);
  Ace.add_keybinding editor "switch" "Ctrl-t"    focus_terminal;
  editor

(* * * Startup *)

let run _ =
  let container = by_id "toplevel-container" in
  let output = by_id "output" in
  let textbox : 'a Js.t = by_id_coerce "userinput" Html.CoerceTo.textarea in

  let sharp_chan = open_out "/dev/null0" in
  let sharp_ppf = Format.formatter_of_out_channel sharp_chan in

  let caml_chan = open_out "/dev/null1" in
  let caml_ppf = Format.formatter_of_out_channel caml_chan in

  let execute () =
    let content = Js.to_string (textbox##.value##trim) in
    let content' =
      let len = String.length content in
      if try content <> ""
             && content.[len-1] <> ';'
             && content.[len-2] <> ';'
         with _ -> true
      then content ^ ";;"
      else content in
    current_position := output##.childNodes##.length;
    textbox##.value := js "";
    History.push content;
    JsooTop.execute true ~pp_code:sharp_ppf ~highlight_location caml_ppf content';
    resize ~container ~textbox () >>= fun () ->
    container##.scrollTop := container##.scrollHeight;
    textbox##focus;
    Lwt.return_unit in

  let history_down _e =
    let txt = Js.to_string textbox##.value in
    let pos = textbox##.selectionStart in
    try
      (if String.length txt = pos  then raise Not_found);
      let _ = String.index_from txt pos '\n' in
      Js._true
    with Not_found ->
      History.current txt;
      History.next textbox;
      Js._false
  in

  let history_up _e =
    let txt = Js.to_string textbox##.value in
    let pos = textbox##.selectionStart - 1  in
    try
      (if pos < 0  then raise Not_found);
      let _ = String.rindex_from txt pos '\n' in
      Js._true
    with Not_found ->
      History.current txt;
      History.previous textbox;
      Js._false
  in

  let editor =
    start_editor
      (fun _ -> textbox##focus)
      (fun x -> Lwt.(return (textbox##.value := js x) >>= execute))
  in

  let meta e =
    let b = Js.to_bool in
    b e##.ctrlKey || b e##.altKey || b e##.metaKey in

  let shift e = Js.to_bool e##.shiftKey in
  let ctrl e = Js.to_bool e##.ctrlKey in
  begin (* setup handlers *)
    textbox##.onkeyup :=
      Html.handler (fun _ -> Lwt.async (resize ~container ~textbox); Js._true);
    textbox##.onchange :=
      Html.handler (fun _ -> Lwt.async (resize ~container ~textbox); Js._true);
    textbox##.onkeydown :=
      Html.handler (fun e ->
        match e##.keyCode with
        | 13 when not (meta e || shift e) -> Lwt.async execute; Js._false
        | 13 -> Lwt.async (resize ~container ~textbox); Js._true
        | 76 when meta e -> output##.innerHTML := js ""; Js._true
        | 75 when meta e -> setup_toplevel (); Js._false
        | 38 -> history_up e
        | 40 -> history_down e
        | 84 when ctrl e -> Ace.focus editor; Js._false
        | _ -> Js._true
      );
  end;

  Lwt.async_exception_hook:=(fun exc ->
    Format.eprintf "exc during Lwt.async: %s@." (Printexc.to_string exc);
    match exc with
    | Js.Error e -> Firebug.console##log (e##.stack)
    | _ -> ());

  Lwt.async (fun () ->
    resize ~container ~textbox () >>= fun () ->
    textbox##focus;
    Lwt.return_unit);

  Sys_js.set_channel_flusher caml_chan  (append Colorize.ocaml output "caml");
  Sys_js.set_channel_flusher sharp_chan (append Colorize.ocaml output "sharp");
  Sys_js.set_channel_flusher stdout     (append Colorize.text  output "stdout");
  Sys_js.set_channel_flusher stderr     (append Colorize.text  output "stderr");

  let readline () =
    Js.Opt.case
      (Html.window##prompt
         (js "The toplevel expects inputs:") (js ""))
      (fun () -> "")
      (fun s -> Js.to_string s ^ "\n") in
  Sys_js.set_channel_filler stdin readline;

  (* setup_examples ~container ~textbox; *)
  setup_pseudo_fs ();
  setup_toplevel ();
  setup_printers ();
  History.setup ();

  exec' ("let gv ?engine = Gv.gv ?engine");

  Html.window##.onbeforeunload := Html.handler
      (fun ev ->
         let dirty = not (Ace.is_clean editor) in
         if dirty then Dom.preventDefault ev;
         Js.bool dirty);

  textbox##.value := js ""

let _ =
  Html.window##.onload := Html.handler (fun _ -> run (); Js._false)

