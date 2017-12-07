(*[interp_expr s minx maxx miny maxy] parses and evaluates string [s] between the numeric
 * bounds [min] and [max], and returns the string that is the result of that evaluation.
 * Could be an error - a parsing error, lexical error, or evaluation error.
 * If the evaluation starts the GUI, the string is the string "Plotting..."*)
val interp_expr : string -> float -> float -> float -> float -> string

(*Same as the Pervasives version, but adds "0" if the result of the function call ends with
".", i.e. [string_of_float 2. = "2.0"].*)
val string_of_float : float -> string

(*[help_text] is the helper text to be displayed when the user inputs
 *"help."*)
val help_text : string

(*[main minx maxx] starts the REPL - processing user input and, where appropriate, starting
 * up a GUI to display a function.*)
val main : float -> float -> float -> float -> unit -> unit
