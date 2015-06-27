
#use "TypeSimple.ml"

open Random
open Printf



let () = 
    Random.self_init () ;
    Printf.printf "%d" (Random.int 5)


