use "bigint.sml";

signature RATIONAL =
    sig
        type bigint
        type rational
        exception rat_error

        val make_rat: bigint * bigint -> rational option
        val rat: bigint -> rational option
        val reci: bigint -> rational option
        val neg: rational -> rational
        val inverse : rational -> rational option
        val equal : rational * rational -> bool (* equality *)
        val less : rational * rational -> bool (* less than *)
        val add : rational * rational -> rational (* addition *)
        val subtract : rational * rational -> rational (* subtraction *)
        val multiply : rational * rational -> rational (* multiplication *)
        val divide : rational * rational -> rational option (* division *)
        val showRat : rational -> string
        val showDecimal : rational -> string
        val fromDecimal : string -> rational
        val toDecimal : rational -> string;
    end

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

        fun make_rat (a, b) = let
                                val g = BigInt.gcd(BigInt.bigabs(a),BigInt.bigabs(b))
                              in   
                                case (a,b) of 
                                    ((l1,0), (l2,0)) => (2,BigInt.bigdiv((l1,0),g), BigInt.bigdiv((l2,0),g),0)
                                |   ((l1,0), (l2,1)) => (2,BigInt.bigdiv((l1,1),g), BigInt.bigdiv((l2,0),g),0)
                                |   ((l1,1), (l2,0)) => (2,BigInt.bigdiv((l1,1),g), BigInt.bigdiv((l2,0),g),0)
                                |   ((l1,1), (l2,1)) => (2,BigInt.bigdiv((l1,0),g), BigInt.bigdiv((l2,0),g),0)
                                |   (_,_) => raise rat_error
                              end


        fun rat (a) = (2, a, ([1],0), 0);

        fun reci (a) = case a of
                        (l,0) => (2,([1],0),a,0)
                    |   (l,1) => (2,([1],1),a,0)
                    | ([0],0) => raise rat_error;

        fun neg (a) = case a of 
                    (0,I,N,R) => (1,I,N,R)
                |   (1,I,N,R) => (0,I,N,R)
                |   (2,I,N,R) => (2,BigInt.neg(I),N,R)
                |   (_,_,_,_) => raise rat_error;

        fun inverse (a) = case a of 
                    (2,(l,0),N,R) => (2,N,(l,0),R)
                |   (2,(l,1),N,R) => (2,BigInt.neg(N),(l,0),R)
                |   (_,_,_,_) => raise rat_error

        fun equal (a, b) = case (a,b) of
                    ((0,I1,N1,R1),(0,I2,N2,R2)) => BigInt.eq(I1,I2) andalso BigInt.eq(N1,N2) andalso BigInt.eq(R1,R2)
                |   ((0,I1,N1,R1),(1,I2,N2,R2)) => false
                |   ((1,I1,N1,R1),(0,I2,N2,R2)) => false
                |   ((1,I1,N1,R1),(1,I2,N2,R2)) => BigInt.eq(I1,I2) andalso BigInt.eq(N1,N2) andalso BigInt.eq(R1,R2)
                |   ((2,I1,N1,0),(2,I2,N2,0)) => BigInt.eq(I1,I2) andalso BigInt.eq(N1,N2)
                |   (_,_) => raise rat_error;            


        fun less (a, b) = case (a,b) of
                    ((2,I1,N1,0),(2,I2,N2,0)) => BigInt.lt(BigInt.mul(I1,N2),BigInt.mul(I2,N1))
                |   ((0,I1,N1,R1),(1,I2,N2,R2)) => false
                |   ((1,I1,N1,R1),(0,I2,N2,R2)) => true
                |   ((0,I1,N1,R1),(0,I2,N2,R2)) => if BigInt.lt(I1,I2) then true
                                                   else if BigInt.eq(I1,I2) andalso BigInt.lt(N1,N2) then true
                                                   else if BigInt.eq(I1,I2) andalso BigInt.eq(N1,N2) andalso BigInt.lt(R1,R2) then true
                                                   else false  
                |   ((1,I1,N1,R1),(1,I2,N2,R2)) => less((0,I2,N2,R2),(0,I1,N1,R1))
                |   (_,_) => raise rat_error;

        fun add (a, b) = case (a,b) of
                    ((2,I1,N1,0),(2,I2,N2,0)) => make_rat(BigInt.add(BigInt.mul(I1,N2),BigInt.mul(I2,N1)),BigInt.mul(N1,N2))
                |   (_,_) => raise rat_error;

        fun subtract (a, b) = case (a,b) of
                    ((2,I1,N1,0),(2,I2,N2,0)) => make_rat(BigInt.sub(BigInt.mul(I1,N2),BigInt.mul(I2,N1)),BigInt.mul(N1,N2))
                |   (_,_) => raise rat_error;

        fun multiply (a, b) = case (a,b) of
                    ((2,I1,N1,0),(2,I2,N2,0)) => make_rat(BigInt.mul(I1,I2),BigInt.mul(N1,N2))
                |   (_,_) => raise rat_error;

        fun divide (a, b) = case (a,b) of
                    ((2,I1,N1,0),(2,0,N2,0)) => raise rat_error
                |   ((2,I1,N1,0),(2,I2,N2,0)) => make_rat(BigInt.mul(I1,N2),BigInt.mul(N1,I2))
                |   (_,_) => raise rat_error;

        fun showRat (a) = case a of 
                    (2,I,N,0) => BigInt.toString(I)^"/"^BigInt.toString(N)
                |   (_,_,_,_) => raise rat_error;

        fun showDecimal (a) = case a of 
                    (0,I,N,R) => BigInt.toString(I)^"."^BigInt.toString(N)^"("^BigInt.toString(R)^")"
                |   (1,I,N,R) => "~"^BigInt.toString(I)^"."^BigInt.toString(N)^"("^BigInt.toString(R)^")"
                |   (_,_,_,_) => raise rat_error;

        fun fromDecimal (a) = if String.sub(a,0) = #"~" then
                                let 
                                    val [I,N] = String.tokens (fn c => c = String.sub(".", 0)) (String.extract(a,1,NONE))
                                in 
                                    (1,BigInt.toBigInt(I),BigInt.toBigInt(N), ([0],0))
                                end
                              else
                                let 
                                    val [I,N] = String.tokens (fn c => c = String.sub(".", 0)) a
                                in 
                                    (0,BigInt.toBigInt(I),BigInt.toBigInt(N), ([0],0))
                                end

        fun toDecimal (a) = case a of 
                    (0,I,N,R) => BigInt.toString(I)^"."^BigInt.toString(N)^"("^BigInt.toString(R)^")"
                |   (1,I,N,R) => "~"^BigInt.toString(I)^"."^BigInt.toString(N)^"("^BigInt.toString(R)^")"
                |   (_,_,_,_) => raise rat_error;

end;
    