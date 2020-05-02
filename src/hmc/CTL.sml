structure CTL = struct
  datatype 'a t =
    True
  | False
  | Atom     of 'a
  | Not      of 'a t
  | And      of ('a t) * ('a t)
  | Or       of ('a t) * ('a t)
  | Implies  of ('a t) * ('a t)
  | Iff      of ('a t) * ('a t)
  | AX       of 'a t
  | EX       of 'a t
  | AF       of 'a t
  | EF       of 'a t
  | AG       of 'a t
  | EG       of 'a t
  | AU       of ('a t) * ('a t) 
  | EU       of ('a t) * ('a t) 

  fun toString (f: clock t) = case f of
    True             => "\226\138\164" 
  | False            => "\226\138\165" 
  | Atom (Clk cname) => cname
  | Not f'           => "\194\172 (" ^ (toString f') ^ ")"
  | And (f1, f2)     => (toString f1) ^ " \226\136\167 " ^ (toString f2) 
  | Or (f1, f2)      => (toString f1) ^ " \226\136\168 " ^ (toString f2) 
  | Implies (f1, f2) => (toString f1) ^ " \226\135\146 " ^ (toString f2) 
  | Iff (f1, f2)     => (toString f1) ^ " \226\135\148 " ^ (toString f2) 
  | AX f'            => "AX (" ^ (toString f') ^ ")" 
  | EX f'            => "EX (" ^ (toString f') ^ ")" 
  | AF f'            => "AF (" ^ (toString f') ^ ")" 
  | EF f'            => "EF (" ^ (toString f') ^ ")" 
  | AG f'            => "AG (" ^ (toString f') ^ ")" 
  | EG f'            => "EG (" ^ (toString f') ^ ")" 
  | AU (f1, f2)      => "A [" ^ (toString f1) ^ " U " ^ (toString f2) ^ "]"
  | EU (f1, f2)      => "E [" ^ (toString f1) ^ " U " ^ (toString f2) ^ "]"

  fun clocks_of_CTL (f: clock t): clock list = case f of
    True             => []
  | False            => []
  | Atom c           => [c]
  | Not f'           => clocks_of_CTL f'
  | And (f1, f2)     => (clocks_of_CTL f1) @\/ (clocks_of_CTL f2)
  | Or (f1, f2)      => (clocks_of_CTL f1) @\/ (clocks_of_CTL f2)
  | Implies (f1, f2) => (clocks_of_CTL f1) @\/ (clocks_of_CTL f2)
  | Iff (f1, f2)     => (clocks_of_CTL f1) @\/ (clocks_of_CTL f2)
  | AX f'            => clocks_of_CTL f'
  | EX f'            => clocks_of_CTL f'
  | AF f'            => clocks_of_CTL f'
  | EF f'            => clocks_of_CTL f'
  | AG f'            => clocks_of_CTL f'
  | EG f'            => clocks_of_CTL f'
  | AU (f1, f2)      => (clocks_of_CTL f1) @\/ (clocks_of_CTL f2)
  | EU (f1, f2)      => (clocks_of_CTL f1) @\/ (clocks_of_CTL f2)

end
