open GMain
open GdkKeysyms
open GDraw
open GObj
open GtkObject

let locale = GtkMain.Main.init ()

let main () =
  let window = GWindow.window ~width:320 ~height:240
                              ~title:"Simple lablgtk program" () in
  let vbox = GPack.vbox ~packing:window#add () in
  window#connect#destroy ~callback:Main.quit;

  (* Menu bar *)
  let menubar = GMenu.menu_bar ~packing:vbox#pack () in
  let factory = new GMenu.factory menubar in
  let accel_group = factory#accel_group in
  let file_menu = factory#add_submenu "File" in

  (* File menu *)
  let factory = new GMenu.factory file_menu ~accel_group in
  factory#add_item "Quit" ~key:_Q ~callback: Main.quit;

  (* Button *)
  let button = GButton.button ~label:"+"
                              ~packing:vbox#add () in
  button#connect#clicked ~callback: (fun () -> prerr_endline "zoom in");

  let button = GButton.button ~label:"-"
                              ~packing:vbox#add () in
  button#connect#clicked ~callback: (fun () -> prerr_endline "zoom out");

  (*  (* Create the drawing area. *)
  let da = GMisc.drawing_area ~packing:vbox#add () in
  let drawable = lazy (new GDraw.drawable da#misc#window) in

  object (self)
(* inherit widget vbox#as_widget *)
inherit gtk_object obj

    initializer
      ignore(da#event#connect#expose
               ~callback:(fun _ -> self#repaint (); false));
      ignore(adjustment#connect#value_changed
               ~callback:(fun _ -> self#repaint ()))

  method repaint ()=
    let drawable = Lazy.force drawable in
    let (width, height) = drawable#size in
  drawable#set_background `WHITE;
      drawable#line ~x:40 ~y:(height-40) ~x:(width-40) ~y:(height-40); *)

  let array = Array.init 100 (fun _ -> Random.int 10) in
    (* Create a graph in the main area. *)
    let graph = new graph font ~packing:vbox#add array in
    graph#set_title "Random data";


  (* Display the windows and enter Gtk+ main loop *)
  window#add_accel_group accel_group;
  window#show ();
  Main.main ()

  let () = main ()
