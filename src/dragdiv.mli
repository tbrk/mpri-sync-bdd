
type t

val create :    ?min_width:int
             -> ?min_height:int
             -> ?width:Js.js_string Js.t
             -> ?height:Js.js_string Js.t
             -> ?pos:int * int
             -> ?title:Js.js_string Js.t
             -> ?on_resize:(unit -> unit)
             -> Dom_html.element Js.t
             -> t

val close : t -> unit

val on_resize : t -> (unit -> unit) -> unit

