open List
open Str


let strsplit rx str = 
  Str.split (Str.regexp rx) str

let alist_get alist k = 
  List.assoc k alist
