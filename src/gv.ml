
module Html = Dom_html
let js = Js.string
let document = Html.window##.document

let zoom_repeat_interval = 50.

let mousedown_repeat interval f ev =
  f ();
  let id = Html.window##setInterval (Js.wrap_callback f) interval in
  document##.onmouseup := Dom.handler (fun _ ->
      Html.window##clearInterval id;
      document##.onmouseup := Dom.no_handler;
      Js._false
    );
  Js._false

let add_custom_controls contents panzoom =
  let open Dom_html in

  let zoom_in = createDiv document in
  zoom_in##.id := js "pan-zoomin";
  zoom_in##.classList##add (js "pan-control");
  zoom_in##.classList##add (js "noselect");
  zoom_in##.textContent := Js.some (js "+");
  zoom_in##.onmousedown :=
    Html.handler (mousedown_repeat zoom_repeat_interval
                    (fun _ -> ignore (panzoom##zoomBy 1.1)));
  Dom.appendChild contents zoom_in;

  let zoom_out = createDiv document in
  zoom_out##.id := js "pan-zoomout";
  zoom_out##.classList##add (js "pan-control");
  zoom_out##.classList##add (js "noselect");
  zoom_out##.textContent := Js.some (js "-");
  zoom_out##.onmousedown :=
    Html.handler (mousedown_repeat zoom_repeat_interval
                    (fun _ -> ignore (panzoom##zoomBy (1. /. 1.1))));
  Dom.appendChild contents zoom_out;

  let zoom_reset = createDiv document in
  zoom_reset##.id := js "pan-reset";
  zoom_reset##.classList##add (js "pan-control");
  zoom_reset##.classList##add (js "noselect");
  zoom_reset##.textContent := Js.some (js "0");
  zoom_reset##.onclick := Html.handler (fun _ -> panzoom##resetZoom ();
                                                 panzoom##center ();
                                                 Js._false);
  Dom.appendChild contents zoom_reset;
  ()

let mapo f vo =
  match vo with
  | None -> None
  | Some v -> Some (f v)

let gv ?(title="Graphviz") ?engine s =
  let viz = new%js Graphviz.viz in
  let options = Graphviz.options ?engine:(mapo js engine) () in
  let p_svg = viz##renderSVGElement_withOptions (js s) options in
  ignore (Promise._then p_svg
            (fun svg ->
              let c = Dom_html.createDiv document in
              Dom.appendChild c svg;
              let svg_style =
                "width: 100%; flex-grow: 1; "
                ^ Js.(Opt.(case (svg##getAttribute (js "style"))
                                (fun () -> "") (fun s -> to_string s))) in
              svg##setAttribute (js "style") (js svg_style);

              (* Calculate size *)
              let sw, sh = svg##.width##.animVal##.value,
                           svg##.height##.animVal##.value in
              let ww, wh = 400., 400. in
              let r1, r2 = sw /. ww, sh /. wh in
              let w, h =
                if r1 < 1. && r2 < 1. then sw, sh
                else if r1 > r2 then sw /. r1, sh /. r1
                else  sw /. r2, sh /. r2
              in
              let w, h = int_of_float w, int_of_float h in

              (* Make window *)
              let win = Dragdiv.create
                          ~min_width:100
                          ~min_height:100
                          ~width:(js (string_of_int w ^ "px"))
                          ~height:(js (string_of_int h ^ "px"))
                          ~title:(js title)
                          c
              in
              let pan = Svgpanzoom.create_withsvg
                          ~zoomEnabled:true
                          ~controlIconsEnabled:false
                          ~contain:true
                          ~center:true
                          svg
              in
              Dragdiv.on_resize win (fun () ->
                  ignore (((pan##resize ())##fit ())##center ()));
              ignore (pan##resize ());
              add_custom_controls c pan;
              Promise.resolve_value win));
  ()

