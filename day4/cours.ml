(*module type Printable = sig
  val print : unit -> unit
end

module IntPrinter : Printable = struct
  let print () = Printf.printf "Printing an integer\n"
end

module StringPrinter = struct
  let print () = Printf.printf "Printing a string\n"
end

module CombinedPrinter : Printable = struct
  include IntPrinter
  include StringPrinter
end
let() = 
        CombinedPrinter.print ()
*)
(*
module type Monoid = sig
  type t
  val i : t
  val (@) : t -> t -> t
end

module integrationunderAdd : Monoid = struct
  type t = int
  let i = 0
  let (@) a b = a + b
end

let result = integrationunderAdd.i @ integrationunderAdd.i = 0;
*)(*
module type M = sig
  type t = int
  val i : t
  val (@) : t -> t -> t
end*)
(*
module type M : intMonoid = struct
        type t = int
        val i = 25
       (* val (@)) = Sdtlib.(-)*)
        let a b = a - b*)
module type M = sig
  type t
  val i : t
  val (@) : t -> t -> t
end

module MyModule : M = struct
  type t = int
  let i = 25
  let (@) a b = a - b
end
let result = MyModule.i - MyModule.i
let () = Printf.printf "Result: %d\n" result


