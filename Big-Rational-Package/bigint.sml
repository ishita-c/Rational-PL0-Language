signature BIGINT =
  sig
    type bigint
    val toString : bigint -> string
    val toBigInt : string -> bigint
    val add : bigint * bigint -> bigint
    val sub : bigint * bigint -> bigint
    val mul : bigint * bigint -> bigint
    val bigdiv : bigint * bigint -> bigint
    val lt : bigint * bigint -> bool
    val eq : bigint * bigint -> bool
    val bigabs : bigint -> bigint
    val gcd : bigint * bigint -> bigint
    val neg : bigint -> bigint;
end

structure BigInt:BIGINT =
  struct
    type bigint = int list * int;
    exception DivideByZeroException;
    exception BadFormat;

    fun removeTrailingZeros(l) = case rev l of
                      [] => [0]
                | 0 :: xs => removeTrailingZeros(rev xs)
                | _ => l 

    fun list2str(l) = let
                        fun loop([], str) = str
                                  |  loop(x :: xs, str) = loop(xs, Int.toString(x)^str)
                      in
                        loop(l, "")
                      end
    
    fun toString (b:bigint) = case b of
                         ([0],0) => "0"
                      |  (l,0) => list2str(removeTrailingZeros l)   
                      |  (l,1) => "~" ^ list2str(removeTrailingZeros l)
                      | _ => "";

    fun cleanString s = if s = "" orelse s = "0" then ""
                        else if String.sub(s,0) = #"0" then cleanString(String.extract(s,1,NONE))
                        else if String.sub(s,0) = #"~" then "~"^cleanString(String.extract(s,1,NONE))
                        else s;

    fun str2list(s) = let
                        fun loop(s, acc) =
                          if s = "" then acc
                          else
                            let
                              val c = String.sub(s, 0)
                              val d = Char.ord(c) - Char.ord(#"0")
                            in
                              loop(String.extract(s, 1, NONE), d :: acc)
                            end
                      in
                        loop(s, [])
                      end
    
    fun toBigInt (s) =  if cleanString(s) = "" then ([0],0)
                        else if cleanString(s) = "~" then ([0],0)
                        else if String.sub(s,0) = #"~" then (str2list(String.extract(s,1,NONE)), 1)
                        else (str2list(cleanString s),0);

    fun listeq ([], []) = true
      | listeq (x::xs, y::ys) = if x = y then listeq (xs, ys)
                                else false
      | listeq (_, _) = false

    fun listlt (a, b) = let
                          val ra = rev a
                          val rb = rev b
                          fun helper ([], []) = false
                          | helper ([], _) = true
                          | helper (_, []) = false
                          | helper (x::xs, y::ys) = if x > y then false
                                                    else if x < y then true
                                                    else helper (xs, ys)
                        in
                            helper (ra, rb)
                        end

    fun listadd (xs, ys, c) =
      let
        fun add ([], [], 0) = []
          | add ([], [], c) = [c]
          | add ([], y::ys, c) = ((y + c) mod 10) :: add (ys, [], (y + c) div 10)
          | add (x::xs, [], c) = ((x + c) mod 10) :: add (xs, [], (x + c) div 10)
          | add (x::xs, y::ys, c) = ((x + y + c) mod 10) :: add (xs, ys, (x + y + c) div 10)
      in
        add (xs, ys, c)
      end 

    fun listsub (xs, ys, c) =
      let
        fun sub ([], [], 0) = []
          | sub ([], [], c) = [c]
          | sub (x::xs, [], c) = ((x + c) mod 10) :: listadd (xs, [], (x + c) div 10)
          | sub ([], y::ys, c) =  ((c-y) mod 10) :: sub(ys,[],(c+y) div 10)
          | sub (x::xs, y::ys, c) = ((x+c-y) mod 10) :: sub(xs,ys,(x+c-y) div 10)                       
      in
        sub (xs, ys, c)
      end 

    fun listmul(a,l,c) = case (a,l,c) of
        (a,[],c) => [c]
      | (a,x::xs,c) => ((a*x+c) mod 10)::listmul(a,xs,(a*x+c) div 10)

    fun mul2lists(xs, l) =
      let
          fun mul2listsHelper(x, (carry, accum)) =
              let
                  val (mul, newCarry) = List.foldr (fn (y, (prod, c)) => 
                      let 
                          val p = (x * y + c) mod 10
                          val carry = (x * y + c) div 10
                      in 
                          (p::prod, carry) 
                      end) ([], carry) l
              in
                  (newCarry, (List.rev mul @ accum))
              end
          val (finalCarry, res) = List.foldr mul2listsHelper (0, []) xs
      in
          if finalCarry > 0 then finalCarry::res else res
      end

    fun replicate (0, _) = []
      | replicate (n, x) = x :: replicate (n - 1, x);

    fun makeEqualSize(a,b) = replicate(length(b)-length(a)-1,0) @ a 

    fun divisor(l1,l2,m) = let 
                            val a = mul2lists(l1,[m+1])
                            val b = l2
                          in 
                            if listlt(a,b) orelse listeq(a,b) then divisor(l1,l2,m+1)
                            else  (m,mul2lists(l1,[m]))
                          end

    fun finddivisor(l1,l2) = case (l1,l2) of
        (_,[]) => (0,[])
      | ([],_) => (0,[])
      | (x::xs,y::ys) => divisor(x::xs,y::ys,0)

    (* l1 = dividend, l2 = divisor *)
    fun listdiv(l1,l2,q) =  if removeTrailingZeros(l2) = [0] then raise DivideByZeroException
                            else if listlt(l1,l2) then ([0],l1)
                            else if listeq(l1,l2) then ([1],[0])
                            else 
                              let
                                val (s,t) = finddivisor(makeEqualSize(l2,l1),l1)
                              in
                                listdiv(listsub(l1,t,0),makeEqualSize(l2,listsub(l1,t,0)),s::q)
                              end

    fun eq (a,b) = case (a,b) of
        ((l1,0),(l2,0)) => listeq (removeTrailingZeros l1,removeTrailingZeros l2)
      | ((l1,1),(l2,1)) => listeq (removeTrailingZeros l1,removeTrailingZeros l2)
      | (_,_) => false

    fun lt (a,b) = case (a,b) of 
        ((l1,1),(l2,0)) => true
      | ((l1,0),(l2,0)) => listlt (removeTrailingZeros l1,removeTrailingZeros l2)
      | ((l1,1),(l2,1)) => not (listeq (removeTrailingZeros l1,removeTrailingZeros l2)) andalso not (listlt (removeTrailingZeros l1,removeTrailingZeros l2))
      | (_,_) => false

    fun add (a,b) = case (a,b) of
        ((l1,0),(l2,0)) => (listadd(l1,l2,0),0)
      | ((l1,1),(l2,1)) => (listadd(l1,l2,0),1)
      | ((l1,0),(l2,1)) => if (lt((l1,0),(l2,0))) then (listsub(l2,l1,0),1)
                           else (listsub(l1,l2,0),0)
      | ((l1,1),(l2,0)) => if (lt((l2,0),(l1,0))) then  (listsub(l1,l2,0),1)
                           else (listsub(l2,l1,0),0)
      | (_,_) => raise BadFormat

    fun sub (a,b) = case (a,b) of  
        ((l1,0),(l2,1)) => (listadd(l1,l2,0),0)
      | ((l1,1),(l2,0)) => (listadd(l1,l2,0),1)
      | ((l1,0),(l2,0)) => if (lt((l1,0),(l2,0))) then (listsub(l2,l1,0),1)
                           else (listsub(l1,l2,0),0)
      | ((l1,1),(l2,1)) => if (lt((l2,0),(l1,0))) then (listsub(l1,l2,0),1)
                           else (listsub(l2,l1,0),0)
      | (_,_) => raise BadFormat 

    fun mul (a,b) = case (a,b) of
        ((l1,0),(l2,0)) => (mul2lists(l1,l2),0)
      | ((l1,1),(l2,1)) => (mul2lists(l1,l2),0)
      | ((l1,0),(l2,1)) => (mul2lists(l1,l2),1)
      | ((l1,1),(l2,0)) => (mul2lists(l1,l2),1)
      | (_,_) => raise BadFormat

    fun bigdiv (a,b) = case (a,b) of
        ((l1,0),(l2,0)) => let
                            val (quotient,_) = listdiv(l1,l2,[0])
                           in
                            (quotient,0)
                           end 
      | ((l1,1),(l2,1)) => let
                            val (quotient,_) = listdiv(l1,l2,[0])
                           in
                            (quotient,0)
                           end
      | ((l1,_),(l2,_)) => let
                            val (quotient,remainder) = listdiv(l1,l2,[0])
                           in
                              if listeq(removeTrailingZeros(remainder),[0]) then (quotient,1)
                              else (listadd(quotient,[1],0),1)
                           end

    fun gcd (a,b) = case (a,b) of
        ((l1,0),([0],0)) =>  (l1,0)
      | (([0],0),(l2,0)) =>  (l2,0)
      | ((l1,0),(l2,0))  => if listeq(l1,l2) then (l1,0)
                            else if listlt(l1,l2) then gcd((l1,0),sub((l2,0),(l1,0)))
                            else gcd(sub((l1,0),(l2,0)),(l2,0))

      | (_,_)   => raise DivideByZeroException

    fun neg (a) = case a of
        ([0],0) => ([0],0)
      | (l,0)   => (l,1)
      | (l,1)   => (l,0)
      | (_,_)   => raise BadFormat


    fun bigabs (a) = case a of 
        (l,1)  => (l,0)
      | (l,0)  => (l,0)
      | (_,_)  => raise BadFormat

end;

