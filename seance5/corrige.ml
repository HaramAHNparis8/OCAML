let chec_duse l = match l with
    |[] -> some fold
    |_ -> ((cdr l) >>=  chec_duse) 

