(*
                              CS51 Lab 12
               Imperative Programming and References
 *)
(*
                               SOLUTION
 *)

(*
Objective:

This lab provides practice with reference types and their use in
building mutable data structures and in imperative programming more
generally. It also gives further practice in using modules to abstract
data types.

There are 4 total parts to this lab. Please refer to the following
files to complete all exercises:

   lab12_part1.ml -- Part 1: Implementing modules
   lab12_part2.ml -- Part 2: Files as modules
-> lab12_part3.ml -- Part 3: Interfaces as abstraction barriers (this file)
   lab12_part4.ml -- Part 4: Polymorphic abstract types
 *)

(*====================================================================
Part 3: Appending mutable lists

Recall the definition of the mutable list type from Section 15.4 of
the textbook: *)

type 'a mlist =
  | Nil
  | Cons of 'a * ('a mlist ref) ;;

(* Mutable lists are just like regular lists, except that the tail of
each "cons" is a *reference* to a mutable list, so that it can be
updated. *)

(*....................................................................
Exercise 5: Define a polymorphic function `mlist_of_list` that
converts a regular list to a mutable list, with behavior like this:

    # let xs = mlist_of_list ["a"; "b"; "c"];;
    val xs : string mlist =
      Cons ("a",
            {contents = Cons ("b", 
                              {contents = Cons ("c", 
                                                {contents = Nil})})})

    # let ys = mlist_of_list [1; 2; 3];;
    val ys : int mlist =
      Cons (1,
            {contents = Cons (2,
                              {contents = Cons (3,
                                                {contents = Nil})})})
....................................................................*)

let rec mlist_of_list (lst : 'a list) : 'a mlist =
  match lst with
  | [] -> Nil
  | hd :: tl -> Cons (hd, ref (mlist_of_list tl)) ;;

(*....................................................................
Exercise 6: Define a function `length` to compute the length of an
`mlist`. Try to do this without looking at the solution that is given
in the book. (Don't worry about cycles...yet.)

    # length Nil ;;
    - : int = 0
    # length (mlist_of_list [1;2;3;4]) ;;
    - : int = 4
....................................................................*)

let rec length (mlst : 'a mlist) : int =
  match mlst with
  | Nil -> 0
  | Cons (_hd, tl) -> 1 + length !tl ;;

(*....................................................................
Exercise 7: What is the time complexity of the `length` function in
terms of the length of its list argument? Provide the tightest
complexity class, recorded using the technique from lab 10.
....................................................................*)

type complexity =
  | Unanswered
  | Constant
  | Logarithmic
  | Linear
  | LogLinear
  | Quadratic
  | Cubic
  | Exponential ;;
  
(* ANSWER: The `length` function is linear in the length of its
   argument: O(n). *)
let length_complexity : complexity = Linear

(*....................................................................
Exercise 8: Now, define a function `mappend` that takes a *non-empty*
mutable list and a second mutable list and, as a side effect, causes
the first to become the appending of the two lists. Some questions to
think about before you get started:

 1. What is an appropriate return type for the `mappend` function?
    (You can glean our intended answer from the examples below, but
    try to think it through yourself first.)

 2. Why is there a restriction that the first list be non-empty?

 3. What is the appropriate thing to do if `mappend` is called with an
    empty mutable list as first argument?

Examples of use:

    # let m1 = mlist_of_list [1; 2; 3] ;;
    val m1 : int mlist =
      Cons (1, 
       {contents = Cons (2, {contents = Cons (3, {contents = Nil})})})

    # let m2 = mlist_of_list [4; 5; 6] ;;
    val m2 : int mlist =
      Cons (4,
       {contents = Cons (5, {contents = Cons (6, {contents = Nil})})})

    # length m1 ;;
    - : int = 3

    # mappend m1 m2 ;;
    - : unit = ()

    # length m1 ;;
    - : int = 6

    # m1 ;;
    - : int mlist =
    Cons (1,
     {contents =
       Cons (2,
        {contents =
          Cons (3,
           {contents =
             Cons (4,
              {contents = 
                Cons (5,
                 {contents = Cons (6, {contents = Nil})})})})})})
....................................................................*)

(* Answers to thought questions:

 1. What is an appropriate return type for the `mappend` function?
    (You can glean our intended answer from the examples below, but
    try to think it through yourself first.

      ANSWER: Since `mappend` is called for its side effect, `unit` is
      the appropriate return type. An alternative would be to return
      some indication as to whether the appending succeeded -- since
      it could fail if the first argument is `Nil` -- perhaps as an
      option type or boolean.

 2. Why is there a restriction that the first list be non-empty?

      ANSWER: The `mlist` type only allows tails of lists to be
      mutated, so there is no way to mutate the empty list, and
      therefore no way to append to it.

 3. What is the appropriate thing to do if `mappend` is called with an
    empty mutable list as first argument?

      ANSWER: The return type of the function is `unit`, so there are
      only two options: return `()` or raise an exception. The latter
      is a better choice, since an empty first argument is undoubtedly
      a sign of the code having gone wrong. We use the latter approach
      below.
 *)
       
let rec mappend (xs : 'a mlist) (ys : 'a mlist) : unit =
  match xs with
  | Nil -> invalid_arg "mappend: empty first argument"
  | Cons (_hd, tl) -> match !tl with
		    | Nil -> tl := ys
		    | Cons (_, _) -> mappend !tl ys ;;

(* The `invalid_arg` function is defined in the `Stdlib` module to
   raise an `Invalid_argument` exception, which is ideally suited for
   this situation. Another option is to raise a `Failure` exception
   (also defined in `Stdlib`, perhaps using the `failwith` function),
   though `Invalid_argument` seems more suitable. *)

(* What happens when you evaluate the following expressions
   sequentially in order?

      # let m = mlist_of_list [1; 2; 3] ;;
      # mappend m m ;;
      # m ;;
      # length m ;;

   Do you understand what's going on? *)

