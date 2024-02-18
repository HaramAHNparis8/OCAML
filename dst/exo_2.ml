
include Mods.ListStack



module ExtendListStack (S: Stack) : Stack with type 'a t = 'a S.t = struct
  include S

  let rev l = 
    List.fold_right (fun x y -> List.append y [x]) l []

  let sort_stack arr =

          let n = Array.length arr in
          for i = 0 to n - 1 do
                  for j = 0 to n - i - 2 do
                          if arr.(j) > arr.(j + 1) then
                  let temp = arr.(j) in
                  arr.(j) <- arr.(j + 1);
                  arr.(j + 1) <- temp

                  done;
                  done;

                  arr;;

end
