(**
   Module TypeChecker

   Author : Hai Nguyen Van
            Aéropyrénées Flight Center
*)

(* Type-checker *)

exception UnconcretizedTagType of clock
fun clk_type_lookup clock_types (clk: clock): tag_t =
  case List.find (fn (clk', ty) => clk = clk') clock_types of
      NONE         => raise UnconcretizedTagType (clk)
    | SOME (_, ty) => ty

fun clk_type_declare (stmt: TESL_atomic) (clock_types: (clock * tag_t) list ref) : unit =
  clock_types :=
  uniq ((case stmt of
	TypeDecl (clk, tt, _)             => [(clk, tt)]
     | Sporadic (clk, t)                 => [(clk, type_of_tag t)]
     | Sporadics (clk, tlist)            => [(clk, type_of_tags clk tlist)]
     | TypeDeclSporadics (ty, clk, tags, _) => (clk, ty) :: (List.map (fn t => (clk, type_of_tag t)) tags)
     | TagRelation (c1, t1, c2, t2)      => [(c1, type_of_tags c1 [t1, t2]), (c2, type_of_tags c2 [t1, t2])]
     | TagRelationCst (c, t)             => [(c, type_of_tags c [t])]
     | TagRelationRefl (c1, c2)          =>
	([(c1, clk_type_lookup (!clock_types) c2), (c2, clk_type_lookup (!clock_types) c1)]
	handle UnconcretizedTagType _ =>
		([(c1, clk_type_lookup (!clock_types) c2)] handle UnconcretizedTagType _ =>
									 [(c2, clk_type_lookup (!clock_types) c1)]))
     | TagRelationClk (c1, c2, c3, c4)   =>
	let
	  val type_eval1 = [clk_type_lookup (!clock_types) c1] handle UnconcretizedTagType _ => []
	  val type_eval2 = [clk_type_lookup (!clock_types) c2] handle UnconcretizedTagType _ => []
	  val type_eval3 = [clk_type_lookup (!clock_types) c3] handle UnconcretizedTagType _ => []
	  val type_eval4 = [clk_type_lookup (!clock_types) c4] handle UnconcretizedTagType _ => []
	in (case (uniq (type_eval1 @ type_eval2 @ type_eval3 @ type_eval4)) of
		 []   => raise UnconcretizedTagType (c1) (* arbitrary choice of c1 *)
	      | ty :: _ => [(c1, ty), (c2, ty), (c3, ty), (c4, ty)])
	end
     | TagRelationPre (c1, c2)           =>
	([(c1, clk_type_lookup (!clock_types) c2), (c2, clk_type_lookup (!clock_types) c1)]
	handle UnconcretizedTagType _ =>
		([(c1, clk_type_lookup (!clock_types) c2)] handle UnconcretizedTagType _ =>
									 [(c2, clk_type_lookup (!clock_types) c1)]))
     | TagRelationReflImplies (_, c1, c2) =>
	([(c1, clk_type_lookup (!clock_types) c2), (c2, clk_type_lookup (!clock_types) c1)]
	handle UnconcretizedTagType _ =>
		([(c1, clk_type_lookup (!clock_types) c2)] handle UnconcretizedTagType _ =>
									 [(c2, clk_type_lookup (!clock_types) c1)]))
     | TagRelationFby (c1, tags, c2)     => [(c1, type_of_tags c1 tags), (c2, type_of_tags c2 tags)]
     | TagRelationFun (c, _, clist)      => [(c, Rat_t)] @ (List.map (fn c => (c, Rat_t)) clist)
     | TagRelationDer (c1, c2)           => [(c1, Rat_t), (c2, Rat_t)]
     | TimeDelayedBy (_, t, clk, _, _)   => [(clk, type_of_tag t)]
     | Periodic (c, t1, t2)              => [(c, type_of_tags c [t1, t2])]
     | TypeDeclPeriodic (ty, c, t1, t2)  => (c, ty) :: [(c, type_of_tags c [t1, t2])]
     | _                                 => []
  ) @ !clock_types)

fun type_check (clock_types: (clock * tag_t) list) =
  List.app (fn (clk, ty) => case List.find (fn (clk', ty') => clk = clk' andalso ty <> ty') clock_types of
				  NONE          => ()
				| SOME (_, ty') => raise TagTypeInconsistency (clk, ty, ty')
	    ) clock_types

