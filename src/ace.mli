(* This file is part of Learn-OCaml.
 *
 * Copyright (C) 2016 OCamlPro.
 *
 * Learn-OCaml is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * Learn-OCaml is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>. *)

open Js_of_ocaml

(** Editor *)

type 'a editor

type loc = {
  loc_start: int * int;
  loc_end: int * int;
}

val create_editor: ?show_gutter:bool
                   -> Dom_html.divElement Js.t
                   -> 'a editor

val set_mode: 'a editor -> string -> unit
val set_theme: 'a editor -> string -> unit

val read_range: Ace_types.range Js.t -> (int * int) * (int * int)
val create_range:
   Ace_types.position Js.t -> Ace_types.position Js.t -> Ace_types.range Js.t
val create_position: int -> int -> Ace_types.position Js.t
val position_equal: Ace_types.position Js.t -> Ace_types.position Js.t -> bool
val read_position: Ace_types.position Js.t -> int * int
val greater_position:
  Ace_types.position Js.t -> Ace_types.position Js.t -> bool

val get_contents: ?range:Ace_types.range Js.t -> 'a editor -> string
val get_line: 'a editor -> int -> string
val set_contents: 'a editor -> string -> unit

val get_cursor_position: 'a editor -> Ace_types.position Js.t
val set_cursor_position: 'a editor -> Ace_types.position Js.t -> unit

val get_selection_range: 'a editor -> Ace_types.range Js.t
val set_selection_range: 'a editor -> Ace_types.range Js.t -> unit
val select_all: 'a editor -> unit
val get_selection: 'a editor -> string

val mark_clean: 'a editor -> unit
val is_clean: 'a editor -> bool

type mark_type = Error | Warning | Message

val set_mark:
  'a editor -> ?loc:loc -> ?type_:mark_type -> string -> unit
val clear_marks: 'a editor -> unit
val record_event_handler: 'a editor -> string -> (unit -> unit) -> unit
val set_background_color: 'a editor -> string -> unit
val add_class: 'a editor -> string -> unit
val remove_class: 'a editor -> string -> unit

val focus: 'a editor -> unit
val resize: 'a editor -> bool -> unit

val require: string -> unit

val show_keybindings: 'a editor -> unit
val add_keybinding:
  'a editor ->
  ?ro:bool ->
  ?scrollIntoView:string ->
  ?multiSelectAction:string ->
  string -> string -> ('a editor -> unit) -> unit

val set_font_size: 'a editor -> int -> unit
val set_tab_size: 'a editor -> int -> unit
val get_state: 'a editor -> int -> < .. > Js.t

val get_length: 'a editor -> int
val get_last: 'a editor -> Ace_types.position Js.t

type doc

val document: 'a editor -> doc
val replace: doc -> Ace_types.range Js.t -> string -> unit
val delete: doc -> Ace_types.range Js.t -> unit
val remove: 'a editor -> unit

val get_custom_data: 'a editor -> 'a
val set_custom_data: 'a editor -> 'a -> unit

(** Mode *)

type token
val token: type_:string -> string -> token

type 'state helpers = {
  initial_state: unit -> 'state;
  get_next_line_indent: 'state -> line:string -> tab:string -> string;
  get_line_tokens: string -> 'state -> int -> doc -> ('state * token list);
  check_outdent: ('state -> string -> string -> bool) option;
  auto_outdent: ('state -> Ace_types.document Js.t -> int -> unit) option;
}

val define_mode: string -> 'state helpers -> unit
