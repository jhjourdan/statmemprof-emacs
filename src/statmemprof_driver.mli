(*---------------------------------------------------------------------------
   Copyright (c) 2017 CNRS. All rights reserved. Distributed under the MIT
   license.
  ---------------------------------------------------------------------------*)

type sample_info = { minor : bool ; info : Gc.Memprof.allocation }

(** After a call to this functions, blocks allocated by the given
    thread will no longer be sampled. *)
val add_disabled_thread : Thread.t -> unit

(** Removing a thread from the disabled set. *)
val remove_disabled_thread : Thread.t -> unit

(** Is this thread disabled for sampling? *)
val is_disabled_thread : Thread.t -> bool

(** [no_sampling f x] executes [f x] by temporarilly disabling
    sampling for the current thread. If an exception occurs, sampling
    is re-enabled. *)
val no_sampling : ('a -> 'b) -> 'a -> 'b

(** [reset ()] empties the current set of tracked blocks. *)
val reset : unit -> unit

(** [dump ()] dumps the current set of tracked blocks. *)
val dump : unit -> sample_info list

(** [start sampling_rate callstack_size min_sample_print] starts the
    sampling on the current process.

    [sampling_rate] is the sampling rate of the profiler. Good value: 1e-4.

    [callstack_size] is the size of the fragment of the call stack
    which is captured for each sampled allocation.

    [min_sample_print] is the minimum number of samples under which
    the location of an allocation is not displayed.
 *)
val start : float -> int -> int -> unit
