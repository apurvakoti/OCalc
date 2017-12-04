class graph font ?width ?height ?packing ?show array =
  (* Constants. *)
  let page_size = 10 in            (* Number of bars on "page". *)
  let max_y = 10 in                (* Maximum on Y scale. *)

  (* Number of data points. *)
  let array_size = Array.length array in

  (* Create the containing vbox. *)
  let vbox = GPack.vbox ?width ?height ?packing ?show () in

  (* Create the drawing area. *)
  let da = GMisc.drawing_area ~packing:vbox#add () in
  let drawable = lazy (new GDraw.drawable da#misc#window) in

  (* Create the scrollbar. *)
  let adjustment =
    GData.adjustment
      ~lower:0. ~upper:(float_of_int (array_size-1))
      ~step_incr:1. ~page_incr:(float_of_int page_size) () in
  let scrollbar =
    GRange.scrollbar `HORIZONTAL ~adjustment ~packing:vbox#pack () in

  object (self)
    inherit widget vbox#as_widget

    initializer
      ignore(da#event#connect#expose
               ~callback:(fun _ -> self#repaint (); false));
      ignore(adjustment#connect#value_changed
               ~callback:(fun _ -> self#repaint ()))

    (* Methods will go here. *)

    method private repaint () =
      let drawable = Lazy.force drawable in
      let (width, height) = drawable#size in
      drawable#set_background `WHITE;
      drawable#set_foreground `WHITE;
      drawable#rectangle ~x:0 ~y:0 ~width ~height ~filled:true ();
      drawable#set_foreground `BLACK;

      (* Draw the title. *)
      draw_text drawable font `Centre (width/2, 20) title;

      (* Draw the axes. *)
      drawable#line ~x:40 ~y:(height-40) ~x:(width-40) ~y:(height-40);
      drawable#line ~x:40 ~y:(height-40) ~x:40 ~y:40;

      (* Which part of the data to display? first .. first+page_size-1 *)
      let first_bar = int_of_float adjustment#value in
      let data = Array.sub array first_bar page_size in
      let bar_width = (width - 80) / page_size in

      (* Compute function mapping graph (x, y) to screen coordinates. *)
      let map (x,y) =
        (40 + x * bar_width, height-40 - y * (height-80) / max_y)  in

      (* Draw the axes scales. *)
      draw_text drawable font `Right (40, height-40) "0";
      draw_text drawable font `Right (40, 40) (string_of_int max_y);
      for i = 0 to page_size-1 do
        let x = 40 + i * bar_width + bar_width/2 in
        let y = height-35 in
        let v = first_bar + i in
        draw_text drawable font `Centre (x, y) (string_of_int v)
      done;

      (* Draw the data. *)
      for i = 0 to page_size-1 do
        let (ll_x,ll_y) = map (i, data.(i)) in
        let (tr_x,tr_y) = map (i+1, 0) in
        draw_rectangle drawable "red" (ll_x, ll_y) (tr_x, tr_y)
      done
  end