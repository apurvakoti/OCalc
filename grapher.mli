(*[set_up minx maxx miny maxy assoc_lst] sets up the graph window, with x-scale between
*minx and maxx, and y-scale between miny and maxy, using pre-calculated values in
*assoc_lst. Then draws the graph. *)
val set_up : string -> float -> float -> float -> float -> (float*float) list -> unit
