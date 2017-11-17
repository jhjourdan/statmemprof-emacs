(*---------------------------------------------------------------------------
   Copyright (c) 2017 Jacques-Henri Jourdan and Frédéric Bour. All rights
   reserved. Distributed under the MIT license.
  ---------------------------------------------------------------------------*)

open Memprof
open Printexc
open Inuit

(* Data structures *)

let min_buf_size = 1024
let empty_ephe = Ephemeron.K1.create ()
let samples = ref (Array.make min_buf_size empty_ephe)
let n_samples = ref 0
let samples_lock = Mutex.create ()

(* Data structure management functions. They are not reentrant, so they should
   not be called when the sampling is active. *)

let reset () =
  samples := Array.make min_buf_size empty_ephe;
  n_samples := 0

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
  Mutex.lock samples_lock;
  if !n_samples = Array.length !samples then clean ();
  !samples.(!n_samples) <- e;
  incr n_samples;
  Mutex.unlock samples_lock

(* The callback we use. *)

let dump_thread_id = ref None

let callback : sample_info Memprof.callback = fun info ->
  if Some (Thread.id (Thread.self ())) = !dump_thread_id then None
  else
    let ephe = Ephemeron.K1.create () in
    Ephemeron.K1.set_data ephe info;
    push ephe;
    Some ephe

(* Reading and printing the set of samples. *)

type sampleTree =
    STC of sample_info list * int * (raw_backtrace_slot, sampleTree) Hashtbl.t

let add_sampleTree (s:sample_info) (t:sampleTree) : sampleTree =
  let rec aux idx (STC (sl, n, sth)) =
    if idx >= Printexc.raw_backtrace_length s.callstack then
      STC(s::sl, n+s.n_samples, sth)
    else
      let li = Printexc.get_raw_backtrace_slot s.callstack idx in
      let child =
        try Hashtbl.find sth li
        with Not_found -> STC ([], 0, Hashtbl.create 3)
      in
      Hashtbl.replace sth li (aux (idx+1) child);
      STC(sl, n+s.n_samples, sth)
  in
  aux 0 t

type sortedSampleTree =
    SSTC of int array * int * (raw_backtrace_slot * sortedSampleTree) list

let acc_si si children =
  let acc = Array.make 3 0 in
  List.iter (fun s ->
    let o = match s.Memprof.kind with
      | Memprof.Minor -> 0
      | Memprof.Major -> 1
      | Memprof.Major_postponed -> 1
      | Memprof.Serialized -> 2
    in
    acc.(o) <- acc.(o) + s.Memprof.n_samples;
  ) si;
  List.iter (fun (_, SSTC (acc',_,_)) ->
    acc.(0) <- acc.(0) + acc'.(0);
    acc.(1) <- acc.(1) + acc'.(1);
    acc.(2) <- acc.(2) + acc'.(2);
  ) children;
  acc

let rec sort_sampleTree (t:sampleTree) : sortedSampleTree =
  let STC (sl, n, sth) = t in
  let children =
    List.sort (fun (_, SSTC (_, n1, _)) (_, SSTC (_, n2, _)) -> n2 - n1)
      (Hashtbl.fold (fun li st lst -> (li, sort_sampleTree st)::lst) sth [])
  in
  SSTC (acc_si sl children, n, children)

let dump () =
  Mutex.lock samples_lock;
  let s, sz = !samples, !n_samples in
  let rec aux st i =
    if i >= sz then st
    else match Ephemeron.K1.get_data s.(i) with
      | None -> aux st (i+1)
      | Some s -> aux (add_sampleTree s st) (i+1)
  in
  let st = aux (STC ([], 0, Hashtbl.create 3)) 0 in
  Mutex.unlock samples_lock;
  sort_sampleTree st

let min_samples = ref 0

let sturgeon_dump sampling_rate k =
  let print_acc k acc =
    let n = acc.(0) + acc.(1) + acc.(2) in
    let percent x = float x /. float n *. 100.0 in
    if n > 0 then begin
      Cursor.printf k " (";
      if acc.(0) > 0 then begin
        Cursor.printf k "%02.2f%% minor" (percent acc.(0));
        if acc.(0) < n then Cursor.printf k ", "
      end;
      if acc.(1) > 0 then begin
        Cursor.printf k "%02.2f%% major" (percent acc.(1));
        if acc.(2) > 0 then Cursor.printf k ", "
      end;
      if acc.(2) > 0 then
        Cursor.printf k "%02.2f%% unmarshal" (percent acc.(2));
      Cursor.printf k ")"
    end
  in
  let rec aux root (slot, SSTC (si, n, bt)) =
    if n >= !min_samples then (
      let children =
        if List.exists (fun (_,SSTC(_,n',_)) -> n' >= !min_samples) bt then
          Some (fun root' -> List.iter (aux root') bt)
        else None
      in
      let node = Widget.Tree.add ?children root in
      begin match Printexc.Slot.location (convert_raw_backtrace_slot slot) with
      | Some { filename; line_number; start_char; end_char } ->
        Cursor.printf node "%11.2f MB | %s:%d %d-%d"
                      (float n /. sampling_rate *. float Sys.word_size /. 8e6)
                      filename line_number start_char end_char
      | None ->
        Cursor.printf node "%11.2f MB | ?"
                      (float n /. sampling_rate *. float Sys.word_size /. 8e6)
      end;
      print_acc node si
    )
  in
  let (SSTC (si, n, bt)) = dump () in
  let root = Widget.Tree.make k in
  let node = Widget.Tree.add root ~children:(fun root' -> List.iter (aux root') bt) in
  Cursor.printf node "%11.2f MB total "
                (float n /. sampling_rate *. float Sys.word_size /. 8e6);
  print_acc node si

let started = ref false
let start sampling_rate callstack_size min_samples_print =
  if !started then failwith "Already started";
  started := true;
  min_samples := min_samples_print;
  Memprof.start { sampling_rate; callstack_size; callback };
  let name = Filename.basename Sys.executable_name in
  let server =
    Sturgeon_recipes_server.text_server (name ^ "memprof")
    @@ fun ~args:_ shell ->
    let cursor = Sturgeon_stui.create_cursor shell ~name in
    let menu = Cursor.sub cursor in
    Cursor.text cursor "\n";
    let body = Cursor.sub cursor in
    Cursor.link menu "[Refresh]"
      (fun _ -> Cursor.clear body; sturgeon_dump sampling_rate body);
    sturgeon_dump sampling_rate body
  in
  ignore (Thread.create (fun () ->
              dump_thread_id := Some (Thread.id (Thread.self ()));
              Sturgeon_recipes_server.main_loop server) ());

  (* HACK : when the worker thread computes, it does not give back the
    control to the sturgeon thread easily. As a result, the sturgeon
    interface is not responsive.

    We solve this issue by periodically suspending the worker thread
    for a very short time. *)
  let preempt signal = Thread.delay 1e-6 in
  Sys.set_signal Sys.sigvtalrm (Sys.Signal_handle preempt);
  ignore (Unix.setitimer Unix.ITIMER_VIRTUAL
             { Unix.it_interval = 1e-2; Unix.it_value = 1e-2 })
