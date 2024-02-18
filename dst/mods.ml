module type Monad = sig
type 'a t
val return : 'a -> 'a t
val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module type MonadPlus = sig 
  include Monad 
  val mzero : 'a t
  val mplus : 'a t -> 'a t -> 'a t
end 

module type Stack = sig 
type 'a t
val empty : 'a t
val is_empty : 'a t -> bool
val push : 'a -> 'a t -> 'a t
val pop : 'a t -> 'a t option 
val peek : 'a t -> 'a option
val size : 'a t -> int
end 

module ListStack : Stack with type 'a t = 'a list = struct
type 'a t = 'a list
let empty = []
let is_empty = function [] -> true | _ -> false
let push = List.cons
let pop = function [] -> None | _ :: s -> Some s 
let peek = function [] -> None | x :: _ -> Some x
let size = List.length
end

module type Monoid = sig
type m
val i : m
val ( @ ) : m -> m -> m
end

module type WriterMonad  = sig 
  include Monad
  type m
  val tell: m -> unit t
  val writer: 'a * m -> 'a t
  val runWriter: 'a t -> 'a * m
end

type 'c writerT = Writer of 'c

module MakeWriter (M : Monoid) : (WriterMonad with type m = M.m) with type 'a t = ('a * M.m) writerT = struct

  type m = M.m
  let ( @ ) = M.(@)

  type 'a t = ('a * m) writerT

  let writer (r, o) = Writer (r, o)
  let tell (x : m) : unit t = Writer ((),x)
  
  let runWriter m = match m with
      | Writer (a,b) -> (a,b)

  let return x = Writer (x, M.i)

  let ( >>= ) m f = 
      let (old_v, old_w) = runWriter m in
      let (new_v, new_w) = runWriter (f old_v) in 
      writer (new_v, old_w @ new_w)

end

module MaybeMonadPlus : MonadPlus with type 'a t = 'a option = struct
  type 'a t = 'a option 
  let return x = Some x
  let ( >>= ) x f = match x with
      | None -> None 
      | Some x -> f x 
  let mzero = None 
  let mplus x y = match (x,y) with
  | None, None -> None 
  | Some x, None -> Some x
  | None, Some x -> Some x 
  | Some x, Some _ -> Some x
end


