module type Stack = sig 

  type 'a t

  val empty : 'a t

  val is_empty : 'a t -> bool

  val push : 'a -> 'a t -> 'a t

  val pop : 'a t -> 'a t option 

  val peek : 'a t -> 'a option

  val size : 'a t -> int

end


module ListStack = struct 
  type 'a t = 'a list
  let empt = []
  let is_empty s = match s with
      | [] -> true
      | _ -> false
  let push = List.cons

  let pop = function
      | [] -> None
      | _ :: s -> Some s
  let peak = function
      | [] -> None
      | x :: _ -> Some x
  let size = List.length


end

module Adstack : Stack =
   type 'a t =
       | Empty
       | Stack of 'a * 'a t
   let empty = Empty

   let is_empty s = (s = Empty)

   let push x s = Stack(x, s)

   let pop s = match s with
       | Empty -> None
       | S(_,s) -> s
   let peak = match s with
       | Empty -> None
       | Stack (e,_) -> Some e
   let size s = 
       let rec size_helper s acc = match s with
           | Empty -> acc
           | Stack(_,t) -> size_helper t(acc + 1) in
       size_helper s 0
end


