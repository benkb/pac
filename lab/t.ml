let rec sum xs=
	match xs with
	|[a]->0
	| h::t -> h + sum t

let ff = [ 222, "jsdfsjd"; 999, "jsdf" ] 
