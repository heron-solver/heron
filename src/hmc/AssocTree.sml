(**
  Association Trees
*)
structure AssocTree = struct
  datatype 'a t =
      Leaf of int * 'a
    | Node of (int * 'a) * (('a t) list)

  exception NotFoundinTree
  exception SeveralUnexpectedSubtrees

  (* In order to associate with unique identifiers *)
  val id = ref 0
  fun fresh_id () =
    let val _ = id := (!id) + 1
    in (!id)
    end

  (* Returns 'a elements as a 'a list *)
  fun list_of_t t = case t of
      Leaf (n, x) => [(n, x)]
    | Node ((n, x), subtrees) => (n, x) :: List.concat (List.map (list_of_t) subtrees)

  (* Associate value with the key [n] *)
  (* TODO: Please refactor *)
  fun assoc (n: int) (t: 'a t): 'a =
    let fun find_in_list l = case l of
        [] => raise NotFoundinTree
      | (n', x) :: l' => if n = n'
			    then x
			    else find_in_list l'
    in find_in_list (list_of_t t)
    end

  (* Returns the subtree whose *)
  fun sub (n: int) (t: 'a t): 'a t =
    let fun aux (t: 'a t): ('a t) list = case t of
      Leaf (n', x) => if n' = n
                      then [t]
                      else []
    | Node ((n', x), subtrees) => if n' = n
                                  then [t]
				      else List.concat (List.map (fn t' => aux t') subtrees)
    in case aux t of
	   []  => raise NotFoundinTree
        | [u] => u
        | _   => raise SeveralUnexpectedSubtrees
    end

  (* All the leaves of the tree [t] *)
  fun leaves (t: 'a t) = case t of
     Leaf (n, e)        => [(n, e)] 
   | Node (_, subtrees) => List.concat (List.map (leaves) subtrees)

  (* All the leaves of the tree [t] *)
  fun nodes (t: 'a t) = case t of
     Leaf _                  => [] 
   | Node ((n, e), subtrees) => (List.concat (List.map (nodes) subtrees)) @ [(n, e)]

  (* Returns a list containing all identifiers *)
  fun id_list (t: 'a t) = case t of
     Leaf (n,_)              => [n]
   | Node ((n, _), subtrees) => n :: List.concat (List.map (id_list) subtrees)

  (* Replace a leaf by a node containing subtrees with new identifiers and elements from [l]  *)
  fun grow (t: 'a t) (id: int) (l: 'a list) = case t of
      Leaf (n, x)         => if n <> id
				 then Leaf (n, x)
				 else Node ((n, x), List.foldl (fn (cf, res) => res @ [Leaf (fresh_id(), cf)]) [] l)
    | Node ((n, x), subt) => Node ((n, x), List.map (fn t' => grow t' id l) subt)

  fun count_node (t: 'a t) = case t of
      Leaf _         => 0
    | Node (_, subt) => 1 + List.foldl (fn (x, res) => res + x) 0 (List.map (fn t => count_node t) subt)

  fun count_leaf (t: 'a t) = case t of
      Leaf _         => 1
    | Node (_, subt) => List.foldl (fn (x, res) => res + x) 0 (List.map (fn t => count_leaf t) subt)

  fun info (t: 'a t) = let
    val _ = print ("[AssocTree] Leaves: " ^ (Int.toString (count_leaf t)) ^ "\n")
    val _ = print ("[AssocTree] Nodes:  " ^ (Int.toString (count_node t)) ^ "\n")
  in ()
  end
end
