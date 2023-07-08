use "bigint.sml";

signature RATIONAL =
    sig
        type bigint
        type rational
        exception rat_error
        val toFraction: rational -> rational
        val make_rat: bigint * bigint -> rational
        (* val rat: bigint -> rational option *)
        val rat: bigint -> rational
        val reci: bigint -> rational option
        val neg: rational -> rational
        val inverse : rational -> rational 
        val equal : rational * rational -> bool (* equality *)
        val less : rational * rational -> bool (* less than *)
        val add : rational * rational -> rational (* addition *)
        val subtract : rational * rational -> rational (* subtraction *)
        val multiply : rational * rational -> rational (* multiplication *)
        val divide : rational * rational -> rational (* division *)
        val showRat : rational -> string
        val showDecimal : rational -> string
        val fromDecimal : string -> rational
        val toDecimal : rational -> string
        val modulo : rational * rational -> rational
    end;

functor Rational (BigInt: BIGINT): RATIONAL =
    struct
        type bigint = BigInt.bigint (*  --- int list * int --- *)
        type rational = int * bigint * bigint * bigint;
        (*
            (S,I,N,R)
                S = 0: Positive rational in decimal form
                S = 1: Negative rational in decimal form
                S = 2: rational in fractional normal form with p = I and q = N, R = 0 by convention

                N = -1 if N is empty in decimal form
        *)

        exception rat_error;

        fun pow10(x: int) = "1" ^ String.concatWith "" (List.tabulate(x, fn _ => "0"))

        fun toFraction(a) = let
                                val (S,I,N,R) = a
                                val x = BigInt.len(I)
                                val y = BigInt.len(N)
                                val z = BigInt.len(R) 
                                val p = BigInt.concat(N,I)
                                val num1 = BigInt.sub(BigInt.concat(R,p),p)
                                val den1 = BigInt.sub(BigInt.toBigInt(pow10(y+z)),BigInt.toBigInt(pow10(y)))
                                val gcd1 = BigInt.gcd(num1,den1)
                                val num = BigInt.bigdiv(num1,gcd1)
                                val den = BigInt.bigdiv(den1,gcd1)
                            in

                                if S=0 andalso BigInt.eq(N,BigInt.zero) andalso BigInt.eq(R,BigInt.zero) then (2,I,BigInt.one,BigInt.zero)
                                else if S=1 andalso BigInt.eq(N,BigInt.zero) andalso BigInt.eq(R,BigInt.zero) then (2,BigInt.neg(I),BigInt.one,BigInt.zero)
                                else if S = 0 then (2,num,den,BigInt.zero) else (2,BigInt.neg(num),den,BigInt.zero)
                            end;

        fun make_rat (a, b) = let
                                val g = BigInt.gcd(BigInt.bigabs(a),BigInt.bigabs(b))
                                val num = BigInt.bigdiv(BigInt.bigabs(a),g)
                                val den = BigInt.bigdiv(BigInt.bigabs(b),g)
                                (* val num = a
                                val den = b *)
                              in   
                                case (BigInt.sign(a),BigInt.sign(b)) of 
                                    (0,0) => (2,num,den,BigInt.zero)
                                |   (0,1) => (2,BigInt.neg(num), den,BigInt.zero)
                                |   (1,0) => (2,BigInt.neg(num), den,BigInt.zero)
                                |   (1,1) => (2,num,den,BigInt.zero)
                                |   (_,_) => raise rat_error
                              end;


        (* fun rat (a) = SOME(2, a, BigInt.one, BigInt.zero); *)
        fun rat (a) = (2, a, BigInt.one, BigInt.zero);

        fun reci (a) = case BigInt.sign(a) of
                        0 => SOME(2,BigInt.one,a,BigInt.zero)
                    |   1 => SOME(2,BigInt.neg(BigInt.one),a,BigInt.zero)
                    |   _ => raise rat_error;

        fun neg (a) = case a of 
                    (0,I,N,R) => (1,I,N,R)
                |   (1,I,N,R) => (0,I,N,R)
                |   (2,I,N,R) => (2,BigInt.neg(I),N,R)
                |   (_,_,_,_) => raise rat_error;

        fun inverse (a) = case a of 
                    (2,I,N,R) => if BigInt.sign(I) = 0 then (2,N,I,R) 
                                 else (2,BigInt.neg(N),BigInt.neg(I),R)
                |   (S,I,N,R) => inverse(toFraction((S,I,N,R)))


        fun equal (a, b) = case (a,b) of
                    ((0,I1,N1,R1),(0,I2,N2,R2)) => BigInt.eq(I1,I2) andalso BigInt.eq(N1,N2) andalso BigInt.eq(R1,R2)
                |   ((0,I1,N1,R1),(1,I2,N2,R2)) => false
                |   ((1,I1,N1,R1),(0,I2,N2,R2)) => false
                |   ((1,I1,N1,R1),(1,I2,N2,R2)) => BigInt.eq(I1,I2) andalso BigInt.eq(N1,N2) andalso BigInt.eq(R1,R2)
                |   ((2,I1,N1,_),(2,I2,N2,_)) => BigInt.eq(I1,I2) andalso BigInt.eq(N1,N2)
                |   ((2,I1,N1,R1),(0,I2,N2,R2)) => BigInt.eq(I1,I2) andalso BigInt.eq(N1,BigInt.one) andalso BigInt.eq(N2,BigInt.zero) andalso BigInt.eq(R2,BigInt.zero)
                |   ((0,I1,N1,R1),(2,I2,N2,R2)) => BigInt.eq(I1,I2) andalso BigInt.eq(N2,BigInt.one) andalso BigInt.eq(N1,BigInt.zero) andalso BigInt.eq(R1,BigInt.zero)
                |   (_,_) => false;           


        fun less (a, b) = case (a,b) of
                    ((2,I1,N1,_),(2,I2,N2,_)) => BigInt.lt(BigInt.mul(I1,N2),BigInt.mul(I2,N1))
                |   ((0,I1,N1,R1),(1,I2,N2,R2)) => false
                |   ((1,I1,N1,R1),(0,I2,N2,R2)) => true
                |   ((0,I1,N1,R1),(0,I2,N2,R2)) => if BigInt.lt(I1,I2) then true
                                                   else if BigInt.eq(I1,I2) andalso BigInt.lt(N1,N2) then true
                                                   else if BigInt.eq(I1,I2) andalso BigInt.eq(N1,N2) andalso BigInt.lt(R1,R2) then true
                                                   else false  
                |   ((1,I1,N1,R1),(1,I2,N2,R2)) => less((0,I2,N2,R2),(0,I1,N1,R1))
                |   (_,_) => raise rat_error;

        fun add (a, b) = case (a,b) of
                    ((2,I1,N1,_),(2,I2,N2,_)) => make_rat(BigInt.add(BigInt.mul(I1,N2),BigInt.mul(I2,N1)),BigInt.mul(N1,N2))
                (* |   ((0,I1,N1,R1),(0,I2,N2,R2)) => (0,BigInt.add(I1,I2),BigInt.add(N1,N2),BigInt.add(R1,R2))
                |   ((1,I1,N1,R1),(1,I2,N2,R2)) => (1,BigInt.add(I1,I2),BigInt.add(N1,N2),BigInt.add(R1,R2)) *)
                |   ((S1,I1,N1,R1),(S2,I2,N2,R2)) => add(toFraction((S1,I1,N1,R1)), toFraction((S2,I2,N2,R2)))


        fun subtract (a, b) = case (a,b) of
                    ((2,I1,N1,_),(2,I2,N2,_)) => make_rat(BigInt.sub(BigInt.mul(I1,N2),BigInt.mul(I2,N1)),BigInt.mul(N1,N2))
                (* |   ((0,I1,N1,R1),(0,I2,N2,R2)) => (0,BigInt.sub(I1,I2),BigInt.sub(N1,N2),BigInt.sub(R1,R2))
                |   ((1,I1,N1,R1),(1,I2,N2,R2)) => (1,BigInt.sub(I1,I2),BigInt.sub(N1,N2),BigInt.sub(R1,R2)) *)
                |   ((S1,I1,N1,R1),(S2,I2,N2,R2)) => subtract(toFraction((S1,I1,N1,R1)), toFraction((S2,I2,N2,R2)))

        fun multiply (a, b) = case (a,b) of
                    ((2,I1,N1,_),(2,I2,N2,_)) => make_rat(BigInt.mul(I1,I2),BigInt.mul(N1,N2))
                |   ((S1,I1,N1,R1),(S2,I2,N2,R2)) => multiply(toFraction((S1,I1,N1,R1)), toFraction((S2,I2,N2,R2)))

        fun divide (a, b) = case (a,b) of
                    ((2,I1,N1,_),(2,I2,N2,_)) => if not(BigInt.eq(I2,BigInt.zero)) then make_rat(BigInt.mul(I1,N2),BigInt.mul(N1,I2)) else raise rat_error
                |   ((S1,I1,N1,R1),(S2,I2,N2,R2)) => divide(toFraction((S1,I1,N1,R1)), toFraction((S2,I2,N2,R2)))



        fun showRat (a) = case a of 
                    (2,I,N,_) => BigInt.toString(I)^"/"^BigInt.toString(N)
                |   (S,I,N,R) => showRat(toFraction((S,I,N,R)))

        fun showDecimal (a) = case a of 
                    (0,I,N,R) => BigInt.toString(I)^"."^BigInt.toString(N)^"("^BigInt.toString(R)^")"
                |   (1,I,N,R) => "~"^BigInt.toString(I)^"."^BigInt.toString(N)^"("^BigInt.toString(R)^")"
                |   (2,I,N,_) => BigInt.toString(I)^"/"^BigInt.toString(N)
                |   (_,_,_,_) => raise rat_error;

        fun extractPair(a: string) =
                                        let
                                            val [N,x] = String.tokens (fn c => c = String.sub("(", 0)) a
                                            val len = size x
                                            val R = String.substring(x, 0, len-1)
                                        in
                                            (N,R)
                                        end

        fun fromDecimal (a) = if String.sub(a,0) = #"~" then
                                let 
                                    val [I,x] = String.tokens (fn c => c = String.sub(".", 0)) (String.extract(a,1,NONE))
                                    val (N,R) = extractPair(x)
                                in 
                                    (1,BigInt.toBigInt(I),BigInt.toBigInt(N), BigInt.toBigInt(R))
                                end
                              else if String.sub(a,0) = #"+" then
                                let 
                                    val [I,x] = String.tokens (fn c => c = String.sub(".", 0)) (String.extract(a,1,NONE))
                                    val (N,R) = extractPair(x)
                                in 
                                    (0,BigInt.toBigInt(I),BigInt.toBigInt(N), BigInt.toBigInt(R))
                                end
                              else
                                let 
                                    val [I,x] = String.tokens (fn c => c = String.sub(".", 0)) a
                                    val (N,R) = extractPair(x)
                                in 
                                    (0,BigInt.toBigInt(I),BigInt.toBigInt(N), BigInt.toBigInt(R))
                                end;

        fun toDecimal (a) = case a of 
                    (0,I,N,R) => BigInt.toString(I)^"."^BigInt.toString(N)^"("^BigInt.toString(R)^")"
                |   (1,I,N,R) => "~"^BigInt.toString(I)^"."^BigInt.toString(N)^"("^BigInt.toString(R)^")"
                |   (_,_,_,_) => raise rat_error;

        fun modulo (a, b) = let
                                val (S1,I1,N1,R1) = a
                                val (S2,I2,N2,R2) = b
                            in
                                if BigInt.lt(I1,I2) 
                                then (0,I1,BigInt.zero,BigInt.zero) 
                                else modulo((0,BigInt.sub(I1,I2),BigInt.zero,BigInt.zero),(0,I2,BigInt.zero,BigInt.zero))
                            end

end;

structure Rational = Rational(BigInt)
