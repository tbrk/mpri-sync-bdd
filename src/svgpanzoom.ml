
class type point = object
  method x : float Js.prop
  method y : float Js.prop
end

let from_point p = (p##.x, p##.y)
let to_point ~x ~y =
  object%js
    val mutable x = x
    val mutable y = y
  end

type ctm_r = {
  a : float;
  b : float;
  c : float;
  d : float;
  e : float;
  f : float;
}

class type ctm = object
  method a : float Js.prop
  method b : float Js.prop
  method c : float Js.prop
  method d : float Js.prop
  method e : float Js.prop
  method f : float Js.prop
end

let from_ctm c = {
  a = c##.a;
  b = c##.b;
  c = c##.c;
  d = c##.d;
  e = c##.e;
  f = c##.f;
}
let to_ctm { a; b; c; d; e; f } =
  object%js
    val mutable a = a
    val mutable b = b
    val mutable c = c
    val mutable d = d
    val mutable e = e
    val mutable f = f
  end

class type box = object
  method x      : float Js.prop
  method y      : float Js.prop
  method width  : float Js.prop
  method height : float Js.prop
end

class type sizes = object
  method width    : float Js.prop
  method height   : float Js.prop
  method realZoom : float Js.prop
  method viewBox  : box Js.t Js.prop
end

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

class type init_arg = object
  method svgElement : Dom_svg.svgElement Js.t Js.prop
  method instance   : t Js.t Js.prop
end

class type custom_event_handler = object
  method haltEventListeners : Js.string_array Js.t
  method init               : init_arg Js.t -> unit Js.meth
  method destroy            : unit -> unit Js.meth
end

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

let svgPanZoom =
  (Js.Unsafe.global##._svgPanZoom
    : (Js.js_string Js.t -> t Js.t) Js.constr)

let svgPanZoom_withOptions =
  (Js.Unsafe.global##._svgPanZoom
    : (Js.js_string Js.t -> options Js.t -> t Js.t) Js.constr)

let svgPanZoom' =
  (Js.Unsafe.global##._svgPanZoom
    : (Dom_svg.svgElement Js.t -> t Js.t) Js.constr)

let svgPanZoom_withOptions' =
  (Js.Unsafe.global##._svgPanZoom
    : (Dom_svg.svgElement Js.t -> options Js.t -> t Js.t) Js.constr)

type point_or_bool =
  | Point of float * float
  | Bool  of bool

let set_option fv f vo =
  match vo with
  | None -> ()
  | Some v -> f (fv v)

let set_string = set_option Js.string
let set_bool = set_option Js.bool
let set_float = set_option (fun x -> x)

let wrap_beforeZoom f =
  Js.wrap_meth_callback (fun this x y -> Js.bool (f this x y))
let wrap_onZoom f =
  Js.wrap_meth_callback f
let wrap_beforePan f =
  Js.wrap_meth_callback (fun this p1 p2 ->
      match f this (from_point p1) (from_point p2) with
      | Point (x, y) -> to_point ~x ~y
      | Bool b       -> Js.Unsafe.coerce (Js.bool b))
let wrap_onPan f =
  Js.wrap_meth_callback (fun this p -> f this (from_point p))
let wrap_onUpdatedCTM f =
  Js.wrap_meth_callback (fun this c -> f this (from_ctm c))

let create_options
      ?viewportSelector
      ?panEnabled
      ?controlIconsEnabled
      ?zoomEnabled
      ?dblClickZoomEnabled
      ?mouseWheelZoomEnabled
      ?preventMouseEventsDefault
      ?zoomScaleSensitivity
      ?minZoom
      ?maxZoom
      ?fit
      ?contain
      ?center
      ?refreshRate
      ?beforeZoom
      ?onZoom
      ?beforePan
      ?onPan
      ?onUpdatedCTM
      ?customEventsHandler
      ?eventsListenerElement
      unit =
  let o = (Js.Unsafe.obj [| |] : options Js.t) in
  set_string (fun x -> o##.viewportSelector := x)      viewportSelector;
  set_bool   (fun x -> o##.panEnabled := x)            panEnabled;
  set_bool   (fun x -> o##.controlIconsEnabled := x)   controlIconsEnabled;
  set_bool   (fun x -> o##.zoomEnabled := x)           zoomEnabled;
  set_bool   (fun x -> o##.dblClickZoomEnabled := x)   dblClickZoomEnabled;
  set_bool   (fun x -> o##.mouseWheelZoomEnabled := x) mouseWheelZoomEnabled;
  set_bool   (fun x -> o##.preventMouseEventsDefault := x) preventMouseEventsDefault;
  set_float  (fun x -> o##.zoomScaleSensitivity := x) zoomScaleSensitivity;
  set_float  (fun x -> o##.minZoom := x) minZoom;
  set_float  (fun x -> o##.maxZoom := x) maxZoom;
  set_bool   (fun x -> o##.fit := x) fit;
  set_bool   (fun x -> o##.contain := x) contain;
  set_bool   (fun x -> o##.center := x) center;
  set_string (fun x -> o##.refreshRate := x) refreshRate;
  set_option wrap_beforeZoom (fun x -> o##.beforeZoom := x) beforeZoom;
  set_option wrap_onZoom (fun x -> o##.onZoom := x) onZoom;
  set_option wrap_beforePan (fun x -> o##.beforePan := x) beforePan;
  set_option wrap_onPan (fun x -> o##.onPan := x) onPan;
  set_option wrap_onUpdatedCTM (fun x -> o##.onUpdatedCTM := x) onUpdatedCTM;
  set_option (fun x -> x) (fun x -> o##.customEventsHandler := x) customEventsHandler;
  set_option (fun x -> x) (fun x -> o##.eventsListenerElement := x) eventsListenerElement;
  o

let create
      ?viewportSelector
      ?panEnabled
      ?controlIconsEnabled
      ?zoomEnabled
      ?dblClickZoomEnabled
      ?mouseWheelZoomEnabled
      ?preventMouseEventsDefault
      ?zoomScaleSensitivity
      ?minZoom
      ?maxZoom
      ?fit
      ?contain
      ?center
      ?refreshRate
      ?beforeZoom
      ?onZoom
      ?beforePan
      ?onPan
      ?onUpdatedCTM
      ?customEventsHandler
      ?eventsListenerElement
      svgid =
  let o = create_options
            ?viewportSelector
            ?panEnabled
            ?controlIconsEnabled
            ?zoomEnabled
            ?dblClickZoomEnabled
            ?mouseWheelZoomEnabled
            ?preventMouseEventsDefault
            ?zoomScaleSensitivity
            ?minZoom
            ?maxZoom
            ?fit
            ?contain
            ?center
            ?refreshRate
            ?beforeZoom
            ?onZoom
            ?beforePan
            ?onPan
            ?onUpdatedCTM
            ?customEventsHandler
            ?eventsListenerElement
            ()
  in
  new%js svgPanZoom_withOptions (Js.string svgid) o

let create_withsvg
      ?viewportSelector
      ?panEnabled
      ?controlIconsEnabled
      ?zoomEnabled
      ?dblClickZoomEnabled
      ?mouseWheelZoomEnabled
      ?preventMouseEventsDefault
      ?zoomScaleSensitivity
      ?minZoom
      ?maxZoom
      ?fit
      ?contain
      ?center
      ?refreshRate
      ?beforeZoom
      ?onZoom
      ?beforePan
      ?onPan
      ?onUpdatedCTM
      ?customEventsHandler
      ?eventsListenerElement
      svgele =
  let o = create_options
            ?viewportSelector
            ?panEnabled
            ?controlIconsEnabled
            ?zoomEnabled
            ?dblClickZoomEnabled
            ?mouseWheelZoomEnabled
            ?preventMouseEventsDefault
            ?zoomScaleSensitivity
            ?minZoom
            ?maxZoom
            ?fit
            ?contain
            ?center
            ?refreshRate
            ?beforeZoom
            ?onZoom
            ?beforePan
            ?onPan
            ?onUpdatedCTM
            ?customEventsHandler
            ?eventsListenerElement
            ()
  in
  new%js svgPanZoom_withOptions' svgele o

