(*---------------------------------------------------------------------------
   Copyright (c) 2017 Jacques-Henri Jourdan. All rights reserved. Distributed
   under the MIT license.
  ---------------------------------------------------------------------------*)

open Memprof

(* Helper function for mutex with correct handling of exceptions. *)

let with_lock m f x =
  Mutex.lock m;
  match f x with
  | exception e -> Mutex.unlock m; raise e
  | y -> Mutex.unlock m; y

(* Sampling is deactivated for these threads. *)

module ISet = Set.Make (
  struct
    type t = int
    let compare : int -> int -> int = fun x y -> Pervasives.compare x y
  end)

let disabled_threads_ids = ref ISet.empty
let disabled_threads_mutex = Mutex.create ()
let add_disabled_thread = with_lock disabled_threads_mutex @@ fun thread ->
  disabled_threads_ids := ISet.add (Thread.id thread) !disabled_threads_ids
let remove_disabled_thread = with_lock disabled_threads_mutex @@ fun thread ->
  disabled_threads_ids := ISet.remove (Thread.id thread) !disabled_threads_ids
let is_disabled_thread thread =
  (* Reading from the reference is atomic, so no need to take the lock
     here. *)
  ISet.mem (Thread.id thread) !disabled_threads_ids

let no_sampling f x =
  let th = Thread.self () in
  if is_disabled_thread th then f x
  else begin
    add_disabled_thread th;
    match f x with
    | exception e -> remove_disabled_thread th; raise e
    | y -> remove_disabled_thread th; y
    end

(* Data structures for sampled blocks *)

let min_buf_size = 1024
let empty_ephe = Ephemeron.K1.create ()
let samples = ref (Array.make min_buf_size empty_ephe)
let n_samples = ref 0
let samples_lock = Mutex.create ()

(* Data structure management functions. *)

let clean () =
  let s = !samples and sz = !n_samples in
  let rec aux i j =
    if i >= sz then j
    else if Ephemeron.K1.check_key s.(i) then (s.(j) <- s.(i); aux (i+1) (j+1))
    else aux (i+1) j
  in
  n_samples := aux 0 0;
  Array.fill s !n_samples (sz - !n_samples) empty_ephe;
  if 8 * !n_samples <= Array.length s && Array.length s > min_buf_size then
    samples := Array.sub s 0 (max min_buf_size (2 * !n_samples))
  else if 2 * !n_samples > Array.length s then begin
    let s_new = Array.make (2 * !n_samples) empty_ephe in
    Array.blit !samples 0 s_new 0 !n_samples;
    samples := s_new
  end

let push e =
  if !n_samples = Array.length !samples then clean ();
  !samples.(!n_samples) <- e;
  incr n_samples

(* Our callback. *)

let callback : sample_info Memprof.callback = fun info ->
  if is_disabled_thread (Thread.self ()) then None
  else
    let ephe = Ephemeron.K1.create () in
    Ephemeron.K1.set_data ephe info;
    with_lock samples_lock push ephe;
    Some ephe

(* Control functions *)

let started = ref false
let start sampling_rate callstack_size min_samples_print =
  if !started then failwith "Already started";
  started := true;
  Memprof.start { sampling_rate; callstack_size; callback }

let reset = no_sampling @@ with_lock samples_lock @@ fun () ->
  samples := Array.make min_buf_size empty_ephe;
  n_samples := 0

let dump = no_sampling @@ with_lock samples_lock @@ fun () ->
  let s, sz = !samples, !n_samples in
  let rec aux acc i =
    if i >= sz then acc
    else match Ephemeron.K1.get_data s.(i) with
      | None -> aux acc (i+1)
      | Some s -> aux (s :: acc) (i+1)
  in
  aux [] 0
