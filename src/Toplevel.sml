structure CalcLrVals =
  CalcLrValsFun(structure Token = LrParser.Token)

structure CalcLex =
  CalcLexFun(structure Tokens = CalcLrVals.Tokens)

structure CalcParser =
  Join(structure LrParser = LrParser
 structure ParserData = CalcLrVals.ParserData
 structure Lex = CalcLex)

fun invoke lexstream =
    let fun print_error (s,i:int,_) =
      TextIO.output(TextIO.stdOut,
		    "Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")
     in CalcParser.parse(0,lexstream,print_error,())
    end

val buffer : TESL_atomic list ref = ref [];

fun parse () = 
  let val lexer = CalcParser.makeLexer (fn _ =>
						   (case TextIO.inputLine TextIO.stdIn
						    of SOME s => s
						     | _ => ""))
  val dummyEOF = CalcLrVals.Tokens.EOF(0,0)
  val dummySEMI = CalcLrVals.Tokens.SEMI(0,0)
  fun loop lexer =
    let
	 val _ = print "> "
	 val (result,lexer) = invoke lexer
	 val (nextToken,lexer) = CalcParser.Stream.get lexer
	 val _ = case result
		   of SOME r =>
		      let val _ = (buffer := !buffer @ [r]) in
			TextIO.output(TextIO.stdOut, "val it = " ^ (string_of_expr r) ^ "\n") end
		     | NONE => ()
	in if CalcParser.sameToken(nextToken,dummyEOF) then !buffer
	  else loop lexer
      end
     in loop lexer
  end


val _ = print "Heron 0.1 Release\n"
val spec = parse()

(* TODO :
  - Heuristics
  - Run codirections
*)
val maxstep = case List.find (fn DirMaxstep _ => true | _ => false) spec of
    NONE => ~1
  | SOME (DirMaxstep n) => n
  | _ => raise UnexpectedMatch
val minstep = case List.find (fn DirMinstep _ => true | _ => false) spec of
    NONE => ~1
  | SOME (DirMinstep n) => n
  | _ => raise UnexpectedMatch
val heuristics = List.filter (fn DirHeuristic _ => true | _ => false) spec
val dumpres = List.exists (fn DirDumpres => true | _ => false) spec

val params = (minstep, maxstep, dumpres, ([]:system), heuristics)

val _ = solve spec params

