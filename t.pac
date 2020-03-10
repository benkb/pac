=case kawa
import pmatch
=esac

pmatch [ 11 99 ] 
   | [ a b ] ->
      display a
      display b
   | _ -> display("xagagall")


