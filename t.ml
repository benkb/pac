(* =case kawa *)
(* import pmatch *)
(* =esac *)

let _ =
  pmatch (list 11 99) (a b (display a) (display b)) (_ (display "xxgagall"))


