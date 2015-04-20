open Lwt

module Deferred_intf = struct
  type how = [ `Parallel | `Sequential ]
end
open Deferred_intf

module Deferred = struct
  type 'a t = 'a Lwt.t

  include Monad.Make(struct
    type 'a t = 'a Lwt.t
    let return = Lwt.return
    let bind = Lwt.bind
    let map = `Custom (fun m ~f -> Lwt.map f m)
  end)

  let unit = Lwt.return_unit

  module Result = struct
    type ('a, 'b) t = ('a, 'b) Result.t Lwt.t

    include Monad.Make2(struct
      type ('a, 'b) t = ('a, 'b) Result.t Lwt.t

      let return x = Lwt.return (Ok x)

      let bind m f = Lwt.bind m (function
        | Ok x -> f x
        | Error _ as x -> Lwt.return x
      )

      let map = `Custom (fun m ~f -> Lwt.map (function
        | Ok x -> Ok (f x)
        | Error _ as x -> x
      ) m)
    end)
  end

  module List = struct

    let fold l ~init ~f = Lwt_list.fold_left_s f init l

    let iter ?(how = `Sequential) l ~f =
      match how with
      | `Sequential -> Lwt_list.iter_s f l
      | `Parallel -> Lwt_list.iter_p f l

    let map ?(how = `Sequential) l ~f =
      match how with
      | `Sequential -> Lwt_list.map_s f l
      | `Parallel -> Lwt_list.map_p f l

    let filter ?(how = `Sequential) l ~f =
      match how with
      | `Sequential -> Lwt_list.filter_s f l
      | `Parallel -> Lwt_list.filter_p f l

  end

  module Or_error = struct
    module List = struct

      let map ?(how = `Sequential) l ~f =
        let map = match how with
          | `Sequential -> Lwt_list.map_s
          | `Parallel -> Lwt_list.map_p
        in
        let module M = struct
          exception E of Error.t
          let helper () = map (fun x ->
            f x >>| function
            | Ok x -> x
            | Error e -> raise (E e)
          ) l
        end in
        try (M.helper() >>| fun x -> Ok x)
        with M.E e -> return (Error e)

      let iter ?(how = `Sequential) l ~f =
        let iter = match how with
          | `Sequential -> Lwt_list.iter_s
          | `Parallel -> Lwt_list.iter_p
        in
        let module M = struct
          exception E of Error.t
          let helper () = iter (fun x ->
            f x >>| function
            | Ok () -> ()
            | Error e -> raise (E e)
          ) l
        end in
        try (M.helper() >>| fun () -> Ok ())
        with M.E e -> return (Error e)

    end
  end

end

let return = Deferred.return
let (>>=) = Deferred.(>>=)
let (>>|) = Deferred.(>>|)
let (>>=?) = Deferred.Result.(>>=)
let (>>|?) = Deferred.Result.(>>|)
let fail = Lwt.fail
let raise = `Use_fail_instead

let try_with f =
  Lwt.catch
    (fun () -> f () >>| fun x -> Ok x)
    (fun exn -> return (Error exn))


module In_thread = struct
  let run f = Lwt_preemptive.detach f ()
end

module Pipe = struct
  module Reader = struct
    type 'a t = 'a Lwt_stream.t
  end

  let read r =
    Lwt_stream.get r >>| function
    | Some x -> `Ok x
    | None -> `Eof

  let junk = Lwt_stream.junk

  let peek_deferred r =
    Lwt_stream.peek r >>| function
    | Some x -> `Ok x
    | None -> `Eof

  let map r ~f = Lwt_stream.map f r

  let fold r ~init ~f =
    Lwt_stream.fold_s (fun a accum -> f accum a) r init

  let iter r ~f = Lwt_stream.iter_s f r

end

module Reader = struct
  module Read_result = struct
    type 'a t = [ `Eof | `Ok of 'a ]
  end

  type t = Lwt_io.input_channel

  let open_file ?buf_len file =
    Lwt_io.open_file ?buffer_size:buf_len ~mode:Lwt_io.input file

  let close = Lwt_io.close

  let with_file ?buf_len file ~f =
    Lwt_io.with_file ?buffer_size:buf_len ~mode:Lwt_io.input file f

  let read_line ic =
    Lwt_io.read_line_opt ic >>| function
    | Some x -> `Ok x
    | None -> `Eof

  let read_all ic read_one =
    Lwt_stream.from (fun () -> match%lwt read_one ic with
    | `Ok x -> Lwt.return (Some x)
    | `Eof ->
      Lwt_io.close ic >>= fun () ->
      Lwt.return None
    )

  let lines ic = read_all ic read_line

  let contents ic =
    Lwt_io.read ic >>= fun ans ->
    Lwt_io.close ic >>= fun () ->
    return ans

  let file_contents file = with_file file Lwt_io.read

  let file_lines file =
    Lwt_io.lines_of_file file
    |> Lwt_stream.to_list

end

module Writer = struct
  type t = Lwt_io.output_channel

  let with_file ?perm ?(append=false) file ~f =
    let flags = match append with
      | true ->  Unix.([O_WRONLY; O_CREAT; O_APPEND])
      | false -> Unix.([O_WRONLY; O_CREAT; O_TRUNC])
    in
    Lwt_io.with_file ~flags ?perm ~mode:Lwt_io.output file f

  let write = Lwt_io.write
  let write_char = Lwt_io.write_char
  let write_line = Lwt_io.write_line
end

module Sys = struct
  include Sys
  let file_exists x = Lwt_preemptive.detach file_exists x
end

module Unix = struct

  type file_perm = Unix.file_perm

  (* Lwt doesn't provide a non-blocking version of getcwd because
     presumably it is doesn't block. However, Async does because it
     claims it could block. See
     https://sympa.inria.fr/sympa/arc/ocsigen/2013-09/msg00003.html.

     If we agreed it is non-blocking, then could implement as:

     let getcwd () = return (Unix.getcwd())

     However, I think Async is right, so I wrap it in Lwt's
     detach. *)
  let getcwd () = Lwt_preemptive.detach Unix.getcwd ()

  let rename ~src ~dst = Lwt_unix.rename src dst

  let getpid = Unix.getpid

end
