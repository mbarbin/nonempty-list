open Base

(** A ['a t] represents a non-empty list, as evidenced by the fact that there is no [[]]
    variant. The sexp representation is as a regular list (i.e., the same as the
    [Stable.V3] module below). *)
type 'a t = ( :: ) of 'a * 'a list
[@@deriving compare, equal, sexp, sexp_grammar, hash, quickcheck]

include Comparator.Derived with type 'a t := 'a t
include Container.S1 with type 'a t := 'a t
include Invariant.S1 with type 'a t := 'a t
include Monad.S with type 'a t := 'a t
include Indexed_container.S1 with type 'a t := 'a t

val create : 'a -> 'a list -> 'a t
val init : int -> f:(int -> 'a) -> 'a t
val of_list : 'a list -> 'a t option
val of_list_error : 'a list -> 'a t Or_error.t
val of_list_exn : 'a list -> 'a t
val singleton : 'a -> 'a t
val cons : 'a -> 'a t -> 'a t
val hd : 'a t -> 'a
val tl : 'a t -> 'a list
val nth : 'a t -> int -> 'a option
val nth_exn : 'a t -> int -> 'a
val reduce : 'a t -> f:('a -> 'a -> 'a) -> 'a
val reverse : 'a t -> 'a t
val append : 'a t -> 'a list -> 'a t
val unzip : ('a * 'b) t -> 'a t * 'b t
val zip : 'a t -> 'b t -> ('a * 'b) t List.Or_unequal_lengths.t
val zip_exn : 'a t -> 'b t -> ('a * 'b) t
val mapi : 'a t -> f:(int -> 'a -> 'b) -> 'b t
val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t List.Or_unequal_lengths.t
val map2_exn : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
val filter : 'a t -> f:('a -> bool) -> 'a list
val filteri : 'a t -> f:(int -> 'a -> bool) -> 'a list
val filter_map : 'a t -> f:('a -> 'b option) -> 'b list
val filter_mapi : 'a t -> f:(int -> 'a -> 'b option) -> 'b list
val concat : 'a t t -> 'a t
val concat_map : 'a t -> f:('a -> 'b t) -> 'b t
val last : 'a t -> 'a
val drop_last : 'a t -> 'a list
val to_sequence : 'a t -> 'a Sequence.t
val sort : 'a t -> compare:('a -> 'a -> int) -> 'a t
val stable_sort : 'a t -> compare:('a -> 'a -> int) -> 'a t
val dedup_and_sort : 'a t -> compare:('a -> 'a -> int) -> 'a t
val permute : ?random_state:Random.State.t -> 'a t -> 'a t
val iteri : 'a t -> f:(int -> 'a -> unit) -> unit
val cartesian_product : 'a t -> 'b t -> ('a * 'b) t
val fold_nonempty : 'a t -> init:('a -> 'acc) -> f:('acc -> 'a -> 'acc) -> 'acc
val fold_right : 'a t -> init:'b -> f:('a -> 'b -> 'b) -> 'b
val folding_map : 'a t -> init:'b -> f:('b -> 'a -> 'b * 'c) -> 'c t
val fold_map : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc * 'b) -> 'acc * 'b t

(** [min_elt'] and [max_elt'] differ from [min_elt] and [max_elt] (included in
    [Container.S1]) in that they don't return options. *)
val min_elt' : 'a t -> compare:('a -> 'a -> int) -> 'a

val max_elt' : 'a t -> compare:('a -> 'a -> int) -> 'a

(** Like [Map.add_multi], but comes with a guarantee that the range of the returned map is
    all nonempty lists. *)
val map_add_multi : ('k, 'v t, 'cmp) Map.t -> key:'k -> data:'v -> ('k, 'v t, 'cmp) Map.t

(** Like [Map.of_alist_multi], but comes with a guarantee that the range of the returned
    map is all nonempty lists. *)
val map_of_alist_multi
  :  ('k * 'v) list
  -> comparator:('k, 'cmp) Comparator.Module.t
  -> ('k, 'v t, 'cmp) Map.t

(** Like [Map.of_sequence_multi], but comes with a guarantee that the range of the
    returned map is all nonempty lists. *)
val map_of_sequence_multi
  :  ('k * 'v) Sequence.t
  -> comparator:('k, 'cmp) Comparator.Module.t
  -> ('k, 'v t, 'cmp) Map.t

(** Like [Map.of_list_with_key_multi], but comes with a guarantee that the range of the
    returned map is all nonempty lists. *)
val map_of_list_with_key_multi
  :  'v list
  -> comparator:('k, 'cmp) Comparator.Module.t
  -> get_key:('v -> 'k)
  -> ('k, 'v t, 'cmp) Map.t

(** Like [Result.combine_errors] but for non-empty lists *)
val combine_errors : ('ok, 'err) Result.t t -> ('ok t, 'err t) Result.t

(** Like [Result.combine_errors_unit] but for non-empty lists *)
val combine_errors_unit : (unit, 'err) Result.t t -> (unit, 'err t) Result.t

(** Like [Or_error.combine_errors] but for non-empty lists *)
val combine_or_errors : 'a Or_error.t t -> 'a t Or_error.t

(** Like [Or_error.combine_errors_unit] but for non-empty lists *)
val combine_or_errors_unit : unit Or_error.t t -> unit Or_error.t

type 'a nonempty_list := 'a t

(** a non-empty version of Reversed_list.t *)
module Reversed : sig
  type 'a t = ( :: ) of 'a * 'a Reversed_list.t

  val cons : 'a -> 'a t -> 'a t
  val to_rev_list : 'a t -> 'a Reversed_list.t
  val rev : 'a t -> 'a nonempty_list
  val rev_append : 'a t -> 'a list -> 'a nonempty_list
  val rev_map : 'a t -> f:('a -> 'b) -> 'b nonempty_list
  val rev_mapi : 'a t -> f:(int -> 'a -> 'b) -> 'b nonempty_list

  (** Renders sexps without reversing the list. E.g. [1::2] is represented as [(1 2)]. *)
  module With_sexp_of : sig
    type nonrec 'a t = 'a t [@@deriving sexp_of]
  end

  (** Renders sexps after reversing the list. E.g. [1::2] is represented as [(2 1)]. *)
  module With_rev_sexp_of : sig
    type nonrec 'a t = 'a t [@@deriving sexp_of]
  end
end

val rev' : 'a t -> 'a Reversed.t
val rev_append : 'a Reversed_list.t -> 'a t -> 'a t

module Unstable : sig
  type nonrec 'a t = 'a t [@@deriving compare, equal, hash, sexp, sexp_grammar]
end

module Stable : sig
  (** Represents a [t] as an ordinary list for sexp conversions, e.g. [1::2]
      is represented as [(1 2)]. *)
  module V3 : sig
    type nonrec 'a t = 'a t [@@deriving compare, equal, sexp, sexp_grammar, hash]
  end
end
