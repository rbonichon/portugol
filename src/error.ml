type t =
  | UndeclaredVariableError of string * Location.t
  | TypeError
