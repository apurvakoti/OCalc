(*[compare_string s] compares string [s] to the library of supported functions, and returns
 * [s', p], where s' is the function to which [s] was matched, and [p] being the % match value.*)
val compare_string : string -> string * float


(*[graph_expr s] takes a string [s], evaluates it using the AST, and produces a GUI window
 * displaying the graph of the function that [s] evaluated to.*)
val graph_expr : string -> unit