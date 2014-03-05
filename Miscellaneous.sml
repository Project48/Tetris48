(*
Functions that can be be useful.
*)

structure Miscellaneous = 
struct

	(* prinln s
	TYPE: string -> unit
	PRE: none
	POST: unit
	EXAMPLE: println "hi" = prints hi with a linebreak after to stdout
	*)
	fun println s = print (s ^ "\n")

	(* delay sec
	TYPE: real -> unit
	PRE: none
	POST: unit
	EXAMPLE: delay 60.0 = returns unit after 60 seconds
	*)
	fun delay sec = OS.Process.sleep (Time.fromReal sec)

	(* printInt i
	TYPE: int -> unit
	PRE: none
	POST: unit
	EXAMPLE: printInt 1 = prints 1 to stdout
	*)
	fun printInt i = print(Int.toString(i))

end
