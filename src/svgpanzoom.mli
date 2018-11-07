(***********************************************************************)
(*                                                                     *)
(*    Low-level js_of_ocaml interface to the svn-pan-zoom library      *)
(*                                                                     *)
(*                   Timothy Bourke (Inria/ENS)                        *)
(*                                                                     *)
(*  Copyright 2018 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under an MIT License, refer to the file LICENSE.                   *)
(*                                                                     *)
(***********************************************************************)

(**
   Low-level interface to the
   {{:https://github.com/ariutta/svg-pan-zoom}{svg-pan-zoom}}
   Javascript library.

   @version v3.6.0
   @author Timothy Bourke (Inria/ENS)
 *)

(** Representtion of pan attributes *)
class type point = object
  method x : float Js.prop
  method y : float Js.prop
end

val from_point : point Js.t -> (float * float)
val to_point : x:float -> y:float -> point Js.t

type ctm_r = {
  a : float;
  b : float;
  c : float;
  d : float;
  e : float;
  f : float;
}

(** Representation of CTM (Current Transformation Matrix) values. *)
class type ctm = object
  method a : float Js.prop
  method b : float Js.prop
  method c : float Js.prop
  method d : float Js.prop
  method e : float Js.prop
  method f : float Js.prop
end

val from_ctm : ctm Js.t -> ctm_r
val to_ctm : ctm_r -> ctm Js.t

(** Representation of view boxes *)
class type box = object
  method x      : float Js.prop
  method y      : float Js.prop
  method width  : float Js.prop
  method height : float Js.prop
end

(** Values returned by getSizes *)
class type sizes = object
  method width    : float Js.prop
  method height   : float Js.prop
  method realZoom : float Js.prop
  method viewBox  : box Js.t Js.prop
end

(** The public api of svg_pan_zoom. *)
class type t = object
  method enablePan  : unit -> t Js.t Js.meth
  method disablePan : unit -> t Js.t Js.meth
  method isPanEnabled : unit -> bool Js.t Js.meth
  method pan : point Js.t -> t Js.t Js.meth
  method panBy : point Js.t -> t Js.t Js.meth
  method getPan : unit -> point Js.t Js.meth
  method setBeforePan : (point Js.t -> point Js.t -> bool Js.t) Js.callback Js.t
                        -> t Js.t Js.meth
  method setOnPan : (point Js.t -> unit) Js.callback Js.t 
                    -> t Js.t Js.meth
  method enableZoom : unit -> t Js.t Js.meth
  method disableZoom : unit -> t Js.t Js.meth
  method isZoomEnabled : unit -> bool Js.t Js.meth
  method enableControlIcons : unit -> t Js.t Js.meth
  method disableControlIcons : unit -> t Js.t Js.meth
  method isControlIconsEnabled : unit -> bool Js.t Js.meth
  method enableDblClickZoom : unit -> t Js.t Js.meth
  method disableDblClickZoom : unit -> t Js.t Js.meth
  method isDblClickZoomEnabled : unit -> bool Js.t Js.meth
  method enableMouseWheelZoom : unit -> t Js.t Js.meth
  method disableMouseWheelZoom : unit -> t Js.t Js.meth
  method isMouseWheelZoomEnabled : unit -> bool Js.t Js.meth
  method setZoomScaleSensitivity : float -> t Js.t Js.meth
  method setMinZoom : float -> t Js.t Js.meth
  method setMaxZoom : float -> t Js.t Js.meth
  method setBeforeZoom : (float -> float -> bool Js.t) Js.callback Js.t
                         -> t Js.t Js.meth
  method setOnZoom : (float -> unit) Js.callback Js.t 
                     -> t Js.t Js.meth
  method zoom : float -> t Js.t Js.meth
  method zoomBy : float -> t Js.t Js.meth
  method zoomAtPoint : float -> point Js.t -> t Js.t Js.meth
  method zoomAtPointBy : float -> point Js.t -> t Js.t Js.meth
  method zoomIn : unit -> t Js.t Js.meth
  method zoomOut : unit -> t Js.t Js.meth
  method setOnUpdatedCTM : (ctm Js.t -> unit) Js.callback Js.t
                           -> t Js.t Js.meth
  method getZoom : unit -> float Js.meth
  method resetZoom : unit -> t Js.t Js.meth
  method resetPan : unit -> t Js.t Js.meth
  method reset : unit -> t Js.t Js.meth
  method fit : unit -> t Js.t Js.meth
  method contain : unit -> t Js.t Js.meth
  method center : unit -> t Js.t Js.meth
  method updateBBox : unit -> t Js.t Js.meth
  method resize : unit -> t Js.t Js.meth
  method getSizes : unit -> sizes Js.t Js.meth
  method destroy : unit -> t Js.t Js.meth
end

(** Arguments passed to the init event handler. *)
class type init_arg = object
  method svgElement : Dom_svg.svgElement Js.t Js.prop
  method instance   : t Js.t Js.prop
end

(** Implementations of custom event handlers. *)
class type custom_event_handler = object
  method haltEventListeners : Js.string_array Js.t
  method init               : init_arg Js.t -> unit Js.meth
  method destroy            : unit -> unit Js.meth
end

(** Class for specifying options *)
class type options = object
  method viewportSelector          : Js.js_string Js.t Js.optdef_prop
  method panEnabled                : bool Js.t Js.optdef_prop
  method controlIconsEnabled       : bool Js.t Js.optdef_prop
  method zoomEnabled               : bool Js.t Js.optdef_prop
  method dblClickZoomEnabled       : bool Js.t Js.optdef_prop
  method mouseWheelZoomEnabled     : bool Js.t Js.optdef_prop
  method preventMouseEventsDefault : bool Js.t Js.optdef_prop
  method zoomScaleSensitivity      : float Js.optdef_prop
  method minZoom                   : float Js.optdef_prop
  method maxZoom                   : float Js.optdef_prop
  method fit                       : bool Js.t Js.optdef_prop
  method contain                   : bool Js.t Js.optdef_prop
  method center                    : bool Js.t Js.optdef_prop
  method refreshRate               : Js.js_string Js.t Js.optdef_prop

  method beforeZoom
    : (t Js.t, float -> float -> bool Js.t) Js.meth_callback Js.optdef_prop

  method onZoom
    : (t Js.t, float -> unit) Js.meth_callback Js.optdef_prop

  (** The beforePan handler may return a [point Js.t] or a [bool Js.t]. *)
  method beforePan
    : (t Js.t, point Js.t -> point Js.t -> < .. > Js.t)
          Js.meth_callback Js.optdef_prop

  method onPan
    : (t Js.t, point Js.t -> unit) Js.meth_callback Js.optdef_prop

  method onUpdatedCTM
    : (t Js.t, ctm Js.t -> unit) Js.meth_callback Js.optdef_prop

  method customEventsHandler : custom_event_handler Js.t Js.optdef_prop
  method eventsListenerElement : Dom_svg.svgElement Js.t Js.optdef_prop
end

val svgPanZoom
  : (Js.js_string Js.t -> t Js.t) Js.constr

val svgPanZoom_withOptions
  : (Js.js_string Js.t -> options Js.t -> t Js.t) Js.constr

val svgPanZoom'
  : (Dom_svg.svgElement Js.t -> t Js.t) Js.constr

val svgPanZoom_withOptions'
  : (Dom_svg.svgElement Js.t -> options Js.t -> t Js.t) Js.constr

type point_or_bool =
  | Point of float * float
  | Bool  of bool

val create_options:
     ?viewportSelector          : string
  -> ?panEnabled                : bool
  -> ?controlIconsEnabled       : bool
  -> ?zoomEnabled               : bool
  -> ?dblClickZoomEnabled       : bool
  -> ?mouseWheelZoomEnabled     : bool
  -> ?preventMouseEventsDefault : bool
  -> ?zoomScaleSensitivity      : float
  -> ?minZoom                   : float
  -> ?maxZoom                   : float
  -> ?fit                       : bool
  -> ?contain                   : bool
  -> ?center                    : bool
  -> ?refreshRate               : string
  -> ?beforeZoom   : (t Js.t -> float -> float -> bool)
  -> ?onZoom       : (t Js.t -> float -> unit)
  -> ?beforePan    : (t Js.t -> (float * float) -> (float * float) -> point_or_bool)
  -> ?onPan        : (t Js.t -> (float * float) -> unit)
  -> ?onUpdatedCTM : (t Js.t -> ctm_r -> unit)
  -> ?customEventsHandler       : custom_event_handler Js.t
  -> ?eventsListenerElement     : Dom_svg.svgElement Js.t
  -> unit
  -> options Js.t

val create:
     ?viewportSelector          : string
  -> ?panEnabled                : bool
  -> ?controlIconsEnabled       : bool
  -> ?zoomEnabled               : bool
  -> ?dblClickZoomEnabled       : bool
  -> ?mouseWheelZoomEnabled     : bool
  -> ?preventMouseEventsDefault : bool
  -> ?zoomScaleSensitivity      : float
  -> ?minZoom                   : float
  -> ?maxZoom                   : float
  -> ?fit                       : bool
  -> ?contain                   : bool
  -> ?center                    : bool
  -> ?refreshRate               : string
  -> ?beforeZoom   : (t Js.t -> float -> float -> bool)
  -> ?onZoom       : (t Js.t -> float -> unit)
  -> ?beforePan    : (t Js.t -> (float * float) -> (float * float) -> point_or_bool)
  -> ?onPan        : (t Js.t -> (float * float) -> unit)
  -> ?onUpdatedCTM : (t Js.t -> ctm_r -> unit)
  -> ?customEventsHandler       : custom_event_handler Js.t
  -> ?eventsListenerElement     : Dom_svg.svgElement Js.t
  -> string
  -> t Js.t

val create_withsvg:
     ?viewportSelector          : string
  -> ?panEnabled                : bool
  -> ?controlIconsEnabled       : bool
  -> ?zoomEnabled               : bool
  -> ?dblClickZoomEnabled       : bool
  -> ?mouseWheelZoomEnabled     : bool
  -> ?preventMouseEventsDefault : bool
  -> ?zoomScaleSensitivity      : float
  -> ?minZoom                   : float
  -> ?maxZoom                   : float
  -> ?fit                       : bool
  -> ?contain                   : bool
  -> ?center                    : bool
  -> ?refreshRate               : string
  -> ?beforeZoom   : (t Js.t -> float -> float -> bool)
  -> ?onZoom       : (t Js.t -> float -> unit)
  -> ?beforePan    : (t Js.t -> (float * float) -> (float * float) -> point_or_bool)
  -> ?onPan        : (t Js.t -> (float * float) -> unit)
  -> ?onUpdatedCTM : (t Js.t -> ctm_r -> unit)
  -> ?customEventsHandler       : custom_event_handler Js.t
  -> ?eventsListenerElement     : Dom_svg.svgElement Js.t
  -> Dom_svg.svgElement Js.t
  -> t Js.t

