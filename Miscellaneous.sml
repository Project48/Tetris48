(*
Blandade funktioner som kan vara bra att ha.
*)

structure Miscellaneous = 
struct
	(*print fast med radbrytning på slutet*)
	fun println s = print (s ^ "\n")


	fun delay sec = OS.Process.sleep (Time.fromReal sec)

	
	fun printInt i = print(Int.toString(i))

	
	fun range (a,b) = List.tabulate (b-a, fn x => x+a)


	fun claer () = List.app (fn s => print s) (List.tabulate (100, (fn _ => "\n")))

	
end