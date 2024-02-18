module type WriterMonad  = sig 
    include Monad
    type m
    val tell: m -> unit t
    val writer: 'a * m -> 'a t
    val runWriter: 'a t -> 'a * m
end

type 'c writerT = Writer of 'c

module MakeWriter (M : Monoid) : (WriterMonad with type m = M.m) 
with type 'a t = ('a * M.m) writerT = struct

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


module AddWriter = MakeWriter()
module Addwriter = runWriter(f old_v) in
let (let*) x f = Addwriter(AddMonoid)

let return = AddWriter().return
(*
let rec len(l : 'a list) *)

let rec search l e = match l with
    |[] -> return false
    |x :: xs ->
           let* r = search xs e in
           let* _ = tell 1 in
           return ((x = e) || r)

let rec search2 l e = match l with
    |[] -> return false
    |x :: _ when(x = e) -> return true
    |_ :: xs ->
           let* _  = tell 1 in
           search2 xs e


type abr = 
    | Leaf of int 
    | Node of int * abr * abr

let example_tree = 
    Node (0, 
        Node (1, 
            Node (2, Leaf 3, Leaf 4), 
            (Leaf 5)), 
        Node (6, Leaf 7, Leaf 8))

let rec search_t (t : abr) (e : int) bool StringWriter.t =
    match t with
     | Leaf v -> return (e = v)
     | Node (v,l,r) -> 
             if (v = e) then
                 return true
             else
                let* r_l = search l e in
                let* r_r = search r e in
                let* _  = tell (if r_l then "l" else if r_r then "r" else "") in 
                return (r_l || r_r)
