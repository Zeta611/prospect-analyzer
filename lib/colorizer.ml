let colorize (col : int) (text : string) : string =
  let xterm_256_color (color : int) : string =
    let escape_prefix = "\027" (* = \e = \0x1B = (\033 in oct) *) in
    let finish = "m" in
    escape_prefix ^ "[38;5;" ^ string_of_int color ^ finish
  in
  let reset = "\027[0m" in
  xterm_256_color col ^ text ^ reset

let palette =
  [ 046; 045; 166; 105; 226; 207; 063; 027; 165; 166; 219; 069; 202 ]

let colorize_palatte t =
  colorize (List.nth palette ((t - 1) mod List.length palette))
