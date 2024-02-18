type ('a,'c) readerT = Reader of('c -> 'a)


module type ReaderMonad = sig
        include Monad
        type c
        val runReader: 'a t -> c -> 'a t
        val local : (c -> c) -> 'a t -> 'a t
        val ask : c t

end

module type Context = sig type c end
module MakeReader (C: Context) : (ReaderMonad with type c = C.c)
with type 'a t = ('a, C.c)readerT = struct
  tyep c = C.c
  type 'a t = ('a, c ) readerT
  let runReader = function | Reader r -> r
  let (>>=) x f = Reader (fun w -> runReader (f ( run Reader x w)) w)
  let return x = Reader(fun _ -> x)
  let local f m = Reader(fun w -> runReader m (f w))
  let ask = Reader (fun env -> env)
end

let manip_stack =
        let open Lifo in
        let p1 = empty in
        let p2 = push 1 p1 in
        let p3 = push 2 p2 in
        peek p3
type ('a,'b) state T = State of ('b -> 'a * 'b)
module type StaeMonad = soh

