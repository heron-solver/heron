(**
  Union-find
*)
structure UnionFind = struct
  type t = int array
  val UF_MAX_SIZE: int = 10000

  (* Generate a union-find of size [UF_MAX_SIZE] *)
  fun make (): t =
      let val uf0 = Array.array (UF_MAX_SIZE, 0)
	   fun set_identity (i: int) =
		if i = UF_MAX_SIZE
		then ()
		else (Array.update (uf0, i, i) ; set_identity (i + 1))
      in (set_identity 0) ; uf0
      end

  (* Returns the class representative *)
  fun find (u: t) (i: int): int =
      Array.sub (u, i)

  (* Sets a common class representative *)
  fun union (u: t) (i: int) (j: int) =
      let val ri = find u i
	   val rj = find u j
	   val _ = Array.appi (fn (x, y) => if y = rj then Array.update (u, x, ri) else ()) u
      in ()
      end

  (* Prints until [max] *)
  val print (* (u: t) (max: int): unit *) =
   fn u => fn max =>
   let val rng = range0 max
   in (List.foldl (fn (n, _) => print ((Int.toString n) ^ " ")) () rng ;
	print "\n" ;
	List.foldl (fn (n, _) => print ((Int.toString (Array.sub (u, n))) ^ " ")) () rng ;
	print "\n")
   end
end
