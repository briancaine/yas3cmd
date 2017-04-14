open Core.Std

module Int = struct
  include Int

  let of_string_opt str =
    try Some (of_string str)
    with Failure _ -> None
end
