(*---------------------------------------------------------------------------
   Copyright (c) 2017 CNRS. All rights reserved. Distributed under the MIT
   license.
  ---------------------------------------------------------------------------*)

(** [start sampling_rate callstack_size min_sample_print] starts the
    sampling on the current process and creates a Sturgeon server to
    be used from emacs for profiling memory consumption.

    [sampling_rate] is the sampling rate of the profiler. Good value: 1e-4.

    [callstack_size] is the size of the fragment of the call stack
    which is captured for each sampled allocation.

    [min_sample_print] is the minimum number of samples under which
    the location of an allocation is not displayed.
 *)
val start : float -> int -> int -> unit
