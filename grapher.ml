open Graphics

let set_up title min max assoc_lst =
  
  Graphics.open_graph "";
  resize_window 700 700;
  (*plot axes*)
  
  moveto 350 0;
  lineto 350 700;
  moveto 0 350;
  lineto 700 350;

  (*set markers*)
  let x = ref 0 in
  while !x < 700 do
    moveto !x 335;
    lineto !x 365;
    x := (!x + 70);
  done

  let y = ref 0 in
  while !y < 700 do
    moveto 335 !y;
    lineto 365 !y;
    y := (!y + 70);
  done

  let maxy = List.fold_left (fun (a, b) (c, d) -> max c d) 0. assoc_lst in
  let miny = List.fold_left (fun (a, b) (c, d) -> min c d) 0. assoc_lst in

  let transformx x = ((x -. min) /. (max -. min)) *. 700. in
  let transformy y = ((y -. miny) /. (maxy -. miny)) * 700. in
  let finallst = List.map (fun (x, y) -> (transformx x),(transformy y)) assoc_lst in
  for i=0 to (List.length finallst) do
    plot (i |> List.nth finallst |> fst |> int_of_float) (i |> List.nth finallst |> snd |> int_of_float)
  done;

  ()
  
