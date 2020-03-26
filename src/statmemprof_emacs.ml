(*---------------------------------------------------------------------------
   Copyright (c) 2017 CNRS and Frédéric Bour. All rights reserved.
   Distributed under the MIT license.
  ---------------------------------------------------------------------------*)

open Printexc
open Inuit

module Memprof = Gc.Memprof

(* Reading and printing the set of samples. *)

type sample_info = Statmemprof_driver.sample_info

type sampleTree =
    STC of sample_info list * int *
             (raw_backtrace_slot, sampleTree) Hashtbl.t

let add_sampleTree (t:sampleTree) (s:sample_info) : sampleTree =
  let rec aux idx (STC (sl, n, sth)) =
    if idx >= Printexc.raw_backtrace_length s.info.callstack then
      STC(s::sl, n+s.info.n_samples, sth)
    else
      let li = Printexc.get_raw_backtrace_slot s.info.callstack idx in
      let child =
        try Hashtbl.find sth li
        with Not_found -> STC ([], 0, Hashtbl.create 3)
      in
      Hashtbl.replace sth li (aux (idx+1) child);
      STC(sl, n+s.info.n_samples, sth)
  in
  aux 0 t

type sortedSampleTree =
    SSTC of int array * int * (raw_backtrace_slot * sortedSampleTree) list

let kind (s : sample_info) =
  if s.info.unmarshalled then 2
  else if s.minor then 0 else 1

let acc_si si children =
  let acc = Array.make 3 0 in
  List.iter (fun s ->
    let o = kind s in
    acc.(o) <- acc.(o) + s.info.n_samples;
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

let dump_SST () =
  Statmemprof_driver.dump ()
  |> List.fold_left add_sampleTree (STC ([], 0, Hashtbl.create 3))
  |> sort_sampleTree

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
  let (SSTC (si, n, bt)) = dump_SST () in
  let root = Widget.Tree.make k in
  let node = Widget.Tree.add root ~children:(fun root' -> List.iter (aux root') bt) in
  Cursor.printf node "%11.2f MB total "
                (float n /. sampling_rate *. float Sys.word_size /. 8e6);
  print_acc node si

let started = ref false
let start sampling_rate callstack_size min_samples_print =
  Statmemprof_driver.start sampling_rate callstack_size;
  min_samples := min_samples_print;
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
              Statmemprof_driver.add_disabled_thread (Thread.self ());
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
