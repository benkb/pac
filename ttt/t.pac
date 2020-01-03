=case gauche
use util.match
use leveling
=case ocaml
open Utils
open List
=esac

greeter => (msg)
   match msg
      | "bye" -> print_string("saybye")
      | _ -> print_string("fyou")

greeter "boo"

strs =: strsplit(" " "fuck you")


iter
   greeter
   strs

