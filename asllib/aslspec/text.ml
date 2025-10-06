let green = "\027[32m"
let red = "\027[31m"
let reset_color = "\027[0m"
let regex_underscore = Str.regexp_string "_"
let regexp_spaces_segment = Str.regexp "[ \t]+"

(** An under score escape sequence for LaTeX *)
let escaped_underscore = {|\_|}

let shrink_whitespace str =
  let regexp_newline_segment = Str.regexp "[ \n\r]+" in
  Str.global_replace regexp_newline_segment " " str

let escape_underscores str =
  Str.global_replace regex_underscore escaped_underscore str

let underscore_to_space str = Str.global_replace regex_underscore " " str

type font_type = Text | TextTT | TextSF | TextSC | TextIT

let spec_var_to_latex_var ~font_type var_str =
  if String.equal var_str "_" then "\\Ignore"
  else
    let font =
      match font_type with
      | Text -> "text"
      | TextTT -> "texttt"
      | TextSF -> "textsf"
      | TextSC -> "textsc"
      | TextIT -> "textit"
    in
    Format.sprintf {|\%s{%s}|} font (escape_underscores var_str)

let spec_var_to_prose var_str = underscore_to_space var_str
let to_math_mode str = "$" ^ str ^ "$"
let spec_var_to_template_var var_str = "{" ^ var_str ^ "}"
let remove_underscores str = Str.global_replace (Str.regexp_string "_") "" str
let elem_name_to_math_macro elem_name = "\\" ^ remove_underscores elem_name
