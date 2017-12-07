open Graphics

let string_of_float f =
  let st = string_of_float f in
  if st.[(String.length st) - 1] = '.'
  then st ^ "0"
  else st

let set_up title minx maxx miny maxy assoc_lst =

  Graphics.open_graph "";
  resize_window 1000 1000;
  set_window_title ("OCalc - graphing " ^ title);
  set_color black;
  set_line_width 1;
  moveto 100 900;
  set_text_size 1000;
  draw_string title;
  (*plot axes*)
  let xoffsets = (maxx -. minx) /. 10. in
  let yoffsets = (maxy -. miny) /. 10. in
  moveto 500 0;
  lineto 500 1000;
  moveto 0 500;
  lineto 1000 500;

  (*set markers*)
  let x = ref 0 in
  let xm = ref 0. in
  while !x < 1000 do
    moveto !x 485;
    lineto !x 515;
    moveto !x 460;
    draw_string ((minx +. !xm) |> string_of_float);
    xm := (!xm +. xoffsets);
    x := (!x + 100);
  done;

  let y = ref 0 in
  let ym = ref 0. in
  while !y < 1000 do
    moveto 485 !y;
    lineto 515 !y;
    moveto 515 !y;
    draw_string ((miny +. !ym) |> string_of_float);
    ym := (!ym +. yoffsets);
    y := (!y + 100);
  done;

  (*let maxy = List.fold_left (fun (a, b) (c, d) -> max b d) (0.,0.) assoc_lst in*)
  (*let maxy = List.map (fun (a, b) -> b) assoc_lst |> List.fold_left max 0. in
  (*let miny = List.fold_left (fun (a, b) (c, d) -> min b d) (0.,0.) assoc_lst in*)
  let miny = List.map (fun (a, b) -> b) assoc_lst |> List.fold_left min 0. in*)

  let transformx x = ((x -. minx) /. (maxx -. minx)) *. 1000. in
  let transformy y = ((y -. miny) /. (maxy -. miny)) *. 900. |> (+.) 50. in
  let finallst = List.map (fun (x, y) -> (transformx x),(transformy y)) assoc_lst in
  let filterlst = List.filter (fun (x, y) -> y >0. && y < 1000.) finallst in

  let firstx = List.hd filterlst |> fst |> int_of_float in
  let firsty = List.hd filterlst |> snd |> int_of_float in
  moveto firstx firsty;
  set_color magenta;
  set_line_width 4;
  for i=0 to (List.length filterlst)-1 do
    let y = (i |> List.nth filterlst |> snd |> int_of_float) in
    if y > 0 && y < 1000 then
    lineto (i |> List.nth filterlst |> fst |> int_of_float) y
    else ()
  done;

  ()
;
