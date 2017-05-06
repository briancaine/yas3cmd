open Core.Std

exception InvalidObjectName of string

open S3_misc

module Name = struct
  type t = string

  let is_valid str =
    String.length str <= 1024

  let special_handling_chars =
    [
      '&';
      '$';
      '@';
      '=';
      ';';
      ':';
      '+';
      ' ';
      ',';
      '?';
    ] @ List.map ~f:Char.of_int_exn ((0 -- 31) @ [127])

  let needs_special_handling str =
    List.find ~f:(String.contains str) special_handling_chars <> None

  let should_be_avoided_chars =
    [
      '\\';
      '{';
      '^';
      '}';
      '%';
      '`';
      ']';
      '"';
      '\'';
      '>';
      '[';
      '~';
      '<';
      '#';
      '|';
    ] @
      (* is this correct? I know ascii characters 7 bits and below are kosher
         utf8, but what about for that last bit?

         probably not? *)
      List.map ~f:Char.of_int_exn (128 -- 255)

  let should_be_avoided str =
    List.find ~f:(String.contains str) should_be_avoided_chars <> None

  let of_string item =
    if not (is_valid item)
    then raise (InvalidObjectName item) ;
    item

  let to_string (item : t) = item
end
