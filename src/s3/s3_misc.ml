open Core.Std

module Infix = struct
  (* http://stackoverflow.com/a/244104/3667507 *)
  let ( -- ) i j =
    let rec aux n acc =
      if n < i then acc else aux (n-1) (n :: acc)
    in aux j []
end
