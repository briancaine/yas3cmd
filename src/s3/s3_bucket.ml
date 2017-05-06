open Core.Std

exception InvalidBucketName of string

open S3_overrides

module Name = struct
  type t = string

  let ip_prefix parts =
    (List.take_while ~f:(fun str -> Int.of_string_opt str <> None) parts
     |> List.length)
    >= 4

  let has_ip_sequence str =
    let rec iter split_parts =
      if ip_prefix split_parts
      then true
      else if List.length split_parts <= 4
      then false
      else iter (List.tl_exn split_parts)
    in
    iter (String.split str ~on:'.')

  let no_double_dots str =
    (String.split str ~on:'.'
     |> List.filter ~f:String.is_empty
     |> List.length) = 0

  let is_valid str =
    String.length str >= 3    &&
    String.length str <= 63   &&
    no_double_dots str        &&
    not (has_ip_sequence str)

  let of_string item =
    if not (is_valid item)
    then raise (InvalidBucketName item) ;
    item

  let to_string (item : t) = item

  let to_https_uri t =
    Uri.make ~scheme:"https"
             ~host:(t ^ ".s3.amazonaws.com")
             ()
end
