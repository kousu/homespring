open Gobject;;
open Gtk;;
open Homespring;;
open Printf;;
open List;;

type homespring_state = SnowTick | WaterTick | PowerTick | FishTick | MiscTick
                      | InputTick

let choose (t,f) b = if b then t else f

(* lablgtk3 can't draw a pixbuf onto a cairo context, so convert it into a
   cairo image surface. This goes through an in-memory PNG rather than reading
   the pixels with GdkPixbuf.get_pixels: the Gpointer.region that returns holds
   a raw C pointer, which the OCaml 5 garbage collector crashes on. *)
let surface_of_pixbuf pb =
    let buf = Buffer.create 4096 in
    GdkPixbuf.save_to_buffer pb ~typ:"png" buf;
    let png = Buffer.contents buf in
    let pos = ref 0 in
    Cairo.PNG.create_from_stream ~input:(fun s n ->
        (* cairo hands us a string to fill in place *)
        Bytes.blit_string png !pos (Bytes.unsafe_of_string s) 0 n;
        pos := !pos + n)

(* load an svg (or any image gdk-pixbuf can read) scaled to w by h pixels *)
let render w h s =
    surface_of_pixbuf (GdkPixbuf.from_file_at_size s ~width:w ~height:h)

(* Loading an svg through gdk-pixbuf takes ~30ms, far too slow to redo at every
   zoom step. So load each icon once, at the largest size it is ever drawn at
   (16 pixels at the maximum zoom of 4), and scale it down when drawing. *)
let icon_px = 64
let icons = Hashtbl.create 8
let icon file =
    try Hashtbl.find icons file with Not_found ->
        let surface = render icon_px icon_px file in
        Hashtbl.add icons file surface;
        surface

(* I/O to the textview *)
class io (buf : GText.buffer) =
    object(self)
    val mutable out_flag = false (* prevent us from re-inputting our output *)
    val mutable curline = ""
    val mutable lines = []

    method set_unedit =
        buf#remove_tag_by_name "fixed" ~start:buf#start_iter ~stop:buf#end_iter;
        buf#apply_tag_by_name "fixed" ~start:buf#start_iter ~stop:buf#end_iter

    method insert_cb s =
        if not out_flag then
            if String.get s ((String.length s) - 1) = '\n' then
                (lines <- lines @ [curline]; curline <- ""; self#set_unedit)
            else
                curline <- curline ^ s

    method print s =
        out_flag <- true;
        buf#insert ~iter:buf#end_iter s;
        out_flag <- false;
        self#set_unedit

    method ready = List.length lines > 0

    method get =
        let ret = hd lines in
        lines <- (tl lines);
        ret
end;;

let insert_cb io iter s = io#insert_cb s

(* we supply an empty pic with which to initialise the nodes; this makes it go
much faster *)
let init_pic = Cairo.Image.create Cairo.Image.ARGB32 ~w:1 ~h:1

(* A node of the river, drawn with cairo. [calc_size] measures the node and
   its subtree, [layout] assigns canvas positions, and [draw] paints the
   subtree. The pictures are cairo surfaces as returned by [render]. *)
class gui_node (hs_node : node) =
    object(self)
    val mutable children = []

    val mutable s_pic = (init_pic, init_pic)
    val mutable w_pic = (init_pic, init_pic)
    val mutable p_pic = (init_pic, init_pic)
    val mutable f_pic = (init_pic, init_pic)

    val label = printable hs_node#getName
    val mutable fish_text = ""
    val mutable fish_hidden = true

    (* where this node was laid out: x is its left edge, y its top edge *)
    val mutable x = 0.0
    val mutable y = 0.0

    (* font and picture sizes, set by calc_size *)
    val mutable point_size = 12.0
    val mutable pic_size = 16.0

    (* these are the width and height of the trees rooted at this node *)
    val mutable height = 0.0
    val mutable width = 0.0

    val mutable self_width = 0.0
    val mutable self_height = 0.0

    method printTree spaces =
        let x n = n#printTree (spaces + 2) in
        print_string (String.make spaces ' ');
        printf "\"%s\"\n" (printable hs_node#getName);
        List.iter x children

    method hs_node = hs_node
    method getHeight = height
    method getWidth = width
    method getSelfHeight = self_height
    method getSelfWidth = self_width
    method getX = x
    method getY = y

    (* build a tree of gui_nodes rooted at this node *)
    method init_tree =
        let n = List.map (fun c -> new gui_node c) hs_node#getChildren in
        children <- n;
        List.iter (fun c -> c#init_tree) children;

    (* build a tree of gui_nodes rooted at this node and lay them out. [cr] is
       only used to measure text. *)
    method init cr =
        self#init_tree;
        self#resize cr 1.0;

    (* re-layout the tree. [cr] is only used to measure text. *)
    method resize cr zoom =
        let size = zoom *. 16.0 in
        let point_size = zoom *. 12.0 in
        let datadir = 
                try Sys.getenv "DATADIR" with Not_found -> 
                        (if (Sys.file_exists("snow_on.svg")) then "."
                         else if (Sys.file_exists("data/snow_on.svg")) then "data"
                         else raise (Failure "data dir not found (set DATADIR env to directory containing e.g. snow_on.svg)"))
        in

        s_pic <- ( (icon (datadir^"/snow_on.svg")),
                   (icon (datadir^"/snow_off.svg")));
        w_pic <- ( (icon (datadir^"/water_on.svg")),
                   (icon (datadir^"/water_off.svg")));
        p_pic <- ( (icon (datadir^"/power_on.svg")),
                   (icon (datadir^"/power_off.svg")));
        f_pic <- ( (icon (datadir^"/fish_on.svg")),
                   (icon (datadir^"/fish_off.svg")));
        self#calc_size cr ~point_size ~padding:size ~pic_size:size;
        self#layout 0.0 0.0 ~size ~s:s_pic ~w:w_pic ~p:p_pic ~f:f_pic

    (* calculate the size of this node and the size of this subtree *)
    method calc_size cr ~point_size:ps ~padding ~pic_size:pic =
        let sub n = n#calc_size cr ~point_size:ps ~padding ~pic_size:pic in
        let sum_h h node = node#getHeight +. h +. padding in
        let max_w w node = max node#getWidth w in
        let fe =
            Cairo.set_font_size cr ps;
            Cairo.font_extents cr
        in
        point_size <- ps;
        pic_size <- pic;
        self_width <- max (Cairo.text_extents cr label).Cairo.x_advance
                          (pic *. 4.0);
        self_height <- pic +. fe.Cairo.ascent +. fe.Cairo.descent;

        iter sub children;
        height <- max ((fold_left sum_h 0.0 children) -. padding) self_height;
        width <- (fold_left max_w 0.0 children) +. self_width +. padding;

    (* position every node in this subtree. (x0, y0) is the top left corner of
       the area reserved for the subtree; this node is centred vertically in
       it. *)
    method layout x0 y0 ~size ~s ~w ~p ~f =
        let child_y = ref y0 in
        let child_x = x0 +. self_width +. size in
        let lay_child ch =
            ch#layout child_x !child_y ~size ~s ~w ~p ~f;
            child_y := !child_y +. ch#getHeight +. size
        in
        s_pic <- s; w_pic <- w; p_pic <- p; f_pic <- f;
        x <- x0;
        y <- y0 +. (height *. 0.5);
        iter lay_child children

    (* one "M U name" row (mature/young, upstream/downstream) per salmon here;
       tabs don't render in cairo's text API, so separate the columns with
       spaces *)
    method update_fish_list =
        let f_str sal =
            (if (mature sal) then "M  " else "Y  ") ^
            (if (upstream sal) then "U  " else "D  ") ^
            (printable sal.name)
        in
        fish_text <- String.concat "\n" (List.map f_str hs_node#getSalmon)

    method toggle_fish_list =
        fish_hidden <- not fish_hidden;
        self#update_fish_list

    method update =
        if not fish_hidden then
            self#update_fish_list;
        iter (fun c -> c#update) children

    (* Handle a left click at canvas position (px, py): toggle the fish list of
       the node under it. Returns true if some node was hit, in which case the
       caller should redraw. *)
    method click px py =
        let border = pic_size *. 0.2 in
        if px >= x -. border && px <= x +. self_width +. border
           && py >= y -. border && py <= y +. self_height +. border then
            (self#toggle_fish_list; true)
        else
            exists (fun c -> c#click px py) children

    (* draw the lines from every node in this subtree to its children *)
    method draw_edges cr =
        let cx = x +. self_width *. 0.5 in
        let cy = y +. self_height *. 0.5 in
        let edge c =
            Cairo.move_to cr cx cy;
            Cairo.line_to cr (c#getX +. c#getSelfWidth *. 0.5)
                             (c#getY +. c#getSelfHeight *. 0.5);
            Cairo.stroke cr;
            c#draw_edges cr
        in
        Cairo.set_source_rgb cr 0.0 0.0 0.0;
        Cairo.set_line_width cr 1.0;
        iter edge children

    (* draw this node's box, pictures and name, then those of its subtree *)
    method draw_nodes cr =
        let border = pic_size *. 0.2 in
        let pic i (on, off) b =
            Cairo.save cr;
            Cairo.translate cr (x +. pic_size *. i) y;
            Cairo.scale cr (pic_size /. float_of_int icon_px)
                           (pic_size /. float_of_int icon_px);
            Cairo.set_source_surface cr (choose (on, off) b) ~x:0.0 ~y:0.0;
            Cairo.paint cr;
            Cairo.restore cr
        in
        Cairo.rectangle cr (x -. border) (y -. border)
            ~w:(self_width +. 2.0 *. border) ~h:(self_height +. 2.0 *. border);
        Cairo.set_source_rgb cr 1.0 1.0 1.0;
        Cairo.fill_preserve cr;
        Cairo.set_source_rgb cr 0.0 0.0 0.0;
        Cairo.set_line_width cr 1.0;
        Cairo.stroke cr;
        pic 0.0 s_pic hs_node#hasSnow;
        pic 1.0 w_pic hs_node#hasWater;
        pic 2.0 p_pic hs_node#hasPower;
        pic 3.0 f_pic hs_node#hasSalmon;
        Cairo.set_source_rgb cr 0.0 0.0 0.0;
        Cairo.set_font_size cr point_size;
        Cairo.move_to cr x (y +. pic_size +. (Cairo.font_extents cr).Cairo.ascent);
        Cairo.show_text cr label;
        iter (fun c -> c#draw_nodes cr) children

    (* draw the open fish lists of this subtree, below each node *)
    method draw_fish cr =
        if not fish_hidden && fish_text <> "" then begin
            let lines = String.split_on_char '\n' fish_text in
            Cairo.set_font_size cr (point_size *. 0.75);
            let fe = Cairo.font_extents cr in
            let line_h = fe.Cairo.ascent +. fe.Cairo.descent in
            let w = fold_left
                (fun m l -> max m (Cairo.text_extents cr l).Cairo.x_advance)
                0.0 lines in
            let top = y +. self_height in
            Cairo.rectangle cr (x -. 2.0) (top -. 2.0)
                ~w:(w +. 4.0) ~h:(line_h *. float_of_int (length lines) +. 4.0);
            Cairo.set_source_rgba cr 1.0 1.0 1.0 (2.0 /. 3.0);
            Cairo.fill_preserve cr;
            Cairo.set_source_rgb cr 0.0 0.0 0.0;
            Cairo.set_line_width cr 1.0;
            Cairo.stroke cr;
            List.iteri (fun i l ->
                Cairo.move_to cr x
                    (top +. line_h *. float_of_int i +. fe.Cairo.ascent);
                Cairo.show_text cr l) lines
        end;
        iter (fun c -> c#draw_fish cr) children

    (* draw the whole subtree: edges at the bottom, fish lists on top *)
    method draw cr =
        self#draw_edges cr;
        self#draw_nodes cr;
        self#draw_fish cr
end;;

class canvas (file : string) =
    let c = GnoCanvas.canvas () in
    let t = GText.view () in
    let io = (new io t#buffer) in
    let r = match build_tree file io with
            None -> new Homespring.node
                        "In Homespring, the null program is not a quine.\n"
                        None io
            | Some n -> n
    in
    let scroll1_v = GData.adjustment ~step_incr:25.0 () in
    let scroll1_h = GData.adjustment ~step_incr:25.0 () in

    object(self)
    val mutable state = SnowTick
    val mutable run_id = None
    val gui_root = new gui_node (GnoCanvas.group c#root) c r
    val root = r
    val scroll1 = GBin.scrolled_window ~vpolicy:`AUTOMATIC ~hpolicy:`AUTOMATIC
        ~vadjustment:scroll1_v ~hadjustment:scroll1_h ()
    val scroll2 = GBin.scrolled_window ~vpolicy:`AUTOMATIC ~hpolicy:`AUTOMATIC()
    val pane = GPack.paned `VERTICAL ()
    val text = t
    val canvas = c

    val mutable zoom = 1.0
    val mutable min_zoom = 0.15
    val mutable max_zoom = 4.0

    method getPane = pane
    method getRoot = gui_root
    method root = gui_root#hs_node
    method isFinished = root#isFinished
    method tick =
        if not root#isFinished then begin
            (match state with
                SnowTick -> root#snowTick; state <- WaterTick
              | WaterTick -> root#waterTick; state <- PowerTick
              | PowerTick -> root#powerTick; state <- FishTick
              | FishTick -> root#fishTick; state <- MiscTick
              | MiscTick -> root#miscTick; state <- InputTick
              | InputTick -> root#inputTick; state <- SnowTick);
            gui_root#update;
            flush stdout
        end
    method step =
        match state with
            InputTick -> self#tick
          | _ -> self#tick; self#step

    method run id = run_id <- Some id
    method pause = (match run_id with
        Some id -> Glib.Timeout.remove id
      | None -> ());
        run_id <- None;
    method isRunning = match run_id with Some i -> true | None -> false
    method reset =
        root#reset;
        gui_root#update;
        run_id <- None;
        state <- SnowTick

    method resize =
        let ratio_x = scroll1_h#value /. scroll1_h#upper in
        let ratio_y = scroll1_v#value /. scroll1_v#upper in
        gui_root#resize zoom;
        scroll1_h#set_value (min
            (ratio_x *. scroll1_h#upper)
            (scroll1_h#upper -. scroll1_h#page_size));
        scroll1_v#set_value (min
            (ratio_y *. scroll1_v#upper)
            (scroll1_v#upper -. scroll1_v#page_size));
        flush stdout

    method zoom_in =
        zoom <- min (zoom *. 1.2) max_zoom;
        self#resize

    method zoom_out =
        zoom <- max (zoom *. (5.0 /. 6.0)) min_zoom;
        self#resize

    method zoom_100 =
        zoom <- 1.0;
        self#resize

    method zoom_fit =
        let r = scroll1#misc#allocation in
        let w = float_of_int r.width in
        let h = float_of_int r.height in
        let z = min (w /. gui_root#getWidth) (h /. gui_root#getHeight) in
        zoom <- min (max (z *. zoom *. 0.9) min_zoom) max_zoom;
        self#resize

    initializer
        gui_root#getCanvas#set_pixels_per_unit 1.0;
        gui_root#getCanvas#set_center_scroll_region true;
        gui_root#init;
        scroll1#add gui_root#getCanvas#coerce;
        scroll2#add text#coerce;
        pane#pack1 ~resize:true ~shrink:true scroll1#coerce;
        pane#pack2 ~shrink:true scroll2#coerce;

        ignore (canvas#event#connect#scroll ~callback:(fun s ->
            let test x = Gdk.Convert.test_modifier x (GdkEvent.Scroll.state s)
            in
            let dir = GdkEvent.Scroll.direction s in
            let d = if dir = `UP || dir = `LEFT then (-.1.0) else 1.0 in
            if test `SHIFT then
                scroll1_h#set_value (min
                    (scroll1_h#value +. scroll1_h#step_increment *. d)
                    (scroll1_h#upper -. scroll1_h#page_size))
            else if test `CONTROL then
                if d < 0.0 then self#zoom_in else self#zoom_out
            else
                scroll1_v#set_value (min
                    (scroll1_v#value +. scroll1_v#step_increment *. d)
                    (scroll1_v#upper -. scroll1_v#page_size));
            true));

        (*ignore (scroll1_v#connect#value_changed ~callback:(fun s ->
            Printf.printf "value %f, upper %f, page-size %f\n"
                scroll1_v#value scroll1_v#upper scroll1_v#page_size;
            flush stdout));*)

        let _ = t#buffer#create_tag ~name:"fixed"
                                    [`EDITABLE false; `EDITABLE_SET true]
        in ();
        let _ = t#buffer#connect#insert_text
            ~callback:(insert_cb io) in ();
end;;

