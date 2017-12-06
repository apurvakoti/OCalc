open GMain
open GdkKeysyms
open GDraw
open GObj
open GtkObject
open GPack

let locale = GtkMain.Main.init ()

let main () =
  let window = GWindow.window ~width:500 ~height:500
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
  factory#add_item "Zoom In" ~key:_I ~callback: (fun () -> prerr_endline "zoom in");
  factory#add_item "Zoom Out" ~key:_O ~callback: (fun () -> prerr_endline "zoom out");

  (* Button *)
  let button_table : ((int * int), GButton.button) Hashtbl.t = (Hashtbl.create 64) in

  (*https://github.com/klartext/lablgtk2-ocaml-Tutorial/blob/master/markdown/c669.md*)
  (* Create a new hbox with an image and a label packed into it
   * and pack the box *)
let xpm_label_box ~file ~packing () =
  if not (Sys.file_exists file) then failwith (file ^ " does not exist");

  (* Create box for image and label and pack *)
  let box = GPack.hbox ~border_width:0 ~packing () in

  (* Now on to the image stuff and pack into box *)
  let pixmap = GDraw.pixmap_from_xpm ~file () in
  GMisc.pixmap pixmap ~packing:(box#pack ~padding:0) ();  ()

  (* Create a label for the button and pack into box *)
  (* GMisc.label ~text ~packing:(box#pack ~padding:3) ()  *)

in
  let hbox_table = GPack.hbox ~packing:vbox#add () in
  let table = GPack.table ~homogeneous:true ~width: 350 ~height: 350
      ~packing:hbox_table#add () in

      for x = 0 to 10 do
        for y = 0 to 10 do
          let button = GButton.button () in
          Hashtbl.add button_table (x,y) button;
          table#attach ~left:x ~top:y ~expand:`BOTH (button#coerce);
          (if (x,y) = (5, 5)
           then xpm_label_box ~file:"green.xpm" ~packing:button#add ()
          );
        done
        done;
  (* Display the windows and enter Gtk+ main loop *)
  window#add_accel_group accel_group;
  window#show ();
  Main.main ()

let () = main ()
