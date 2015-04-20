open Result.Export


module Stable = struct
  module V1 = struct
    type 'a t = ('a, Error.Stable.V1.t) Result.Stable.V1.t 

    let map x ~f = Result.Stable.V1.map x ~f1:f ~f2:Fn.id
  end
end

(* 2015-01-14: We don't [include Stable.V1], because that has a different [with bin_io]
   format, and we don't want to change this "unstable" format, which is in use.  We just
   introduced the stable format, so people haven't yet had the time to adjust code to use
   it. *)
type 'a t = ('a, Error.t) Result.t 

let invariant invariant_a t =
  match t with
  | Ok a -> invariant_a a
  | Error error -> Error.invariant error
;;


include (Result : Monad.S2 with type ('a, 'b) t := ('a, 'b) Result.t)

let try_with ?(backtrace = false) f =
  let backtrace = (backtrace :> bool) in
  try Ok (f ())
  with exn -> Error (Error.of_exn exn ?backtrace:(if backtrace then Some `Get else None))
;;

let try_with_join ?backtrace f = join (try_with ?backtrace f)

let ok_exn = function
  | Ok x -> x
  | Error err -> Error.raise err
;;

let of_exn ?backtrace exn = Error (Error.of_exn ?backtrace exn)

let of_exn_result = function
  | Ok _ as z -> z
  | Error exn -> of_exn exn
;;

let error message a sexp_of_a = Error (Error.create message a sexp_of_a)

let error_string message = Error (Error.of_string message)

let errorf format = Printf.ksprintf error_string format

TEST = errorf "foo %d" 13 = error_string "foo 13"

let tag t message = Result.map_error t ~f:(fun e -> Error.tag e message)
let tag_arg t message a sexp_of_a =
  Result.map_error t ~f:(fun e -> Error.tag_arg e message a sexp_of_a)

let unimplemented s = error "unimplemented" (* s <:sexp_of< string >> *)

module List = struct
  include List 
let partition_map t ~f =
  let rec loop t fst snd =
    match t with
    | [] -> (List.rev fst, List.rev snd)
    | x :: t ->
      match f x with
      | `Fst y -> loop t (y :: fst) snd
      | `Snd y -> loop t fst (y :: snd)
  in
  loop t [] []
end


let combine_errors l =
  let ok, errs = List.partition_map l ~f:Result.ok_fst in
  match errs with
  | [] -> Ok ok
  | _ -> Error (Error.of_list errs)


let combine_errors_unit l = Result.map (combine_errors l) ~f:(fun (_ : unit list) -> ())
