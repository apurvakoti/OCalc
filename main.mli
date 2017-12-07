(*[interp_expr s minx maxx miny maxy] parses and evaluates string [s] between the numeric
 * bounds [min] and [max], and returns the string that is the result of that evaluation.
 * Could be an error - a parsing error, lexical error, or evaluation error.
 * If the evaluation starts the GUI, the string is the empty string.*)
val interp_expr : string -> float -> float -> float -> float -> string


(*[help_text] is the helper text to be displayed when the user inputs
 *"help."*)
val help_text : string

(*[main minx maxx] starts the REPL - processing user input and, where appropriate, starting
 * up a GUI to display a function.*)
val main : float -> float -> float -> float -> unit -> unit
