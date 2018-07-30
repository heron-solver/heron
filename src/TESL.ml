type clock = Clock of string;;

type instant_index = int;;

type tag =
  | Unit
  | Int of int
(*  | Rat of rat *);;

type primitive =
  | Timestamp      of clock * instant_index * tag_expr
  | Ticks          of clock * instant_index
  | NotTicks       of clock * instant_index
  | NotTicksUntil  of clock * instant_index
  | NotTicksFrom   of clock * instant_index
  | TimestampRel   of tagvar * tagvar * (tag -> tag -> bool)
  | TickCountLeq   of tcnt_exp * tcnt_exp * (int -> int -> bool);;

type run = primitive list;;
