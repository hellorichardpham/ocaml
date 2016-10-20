(* $Id: bigint.ml,v 1.5 2014-11-11 15:06:24-08 - - $ *)

open Printf

module Bigint = struct

    type sign     = Pos | Neg
    type bigint   = Bigint of sign * int list
    let  radix    = 10
    let  radixlen =  1

    let car       = List.hd
    let cdr       = List.tl
    let map       = List.map
    let reverse   = List.rev
    let strcat    = String.concat
    let strlen    = String.length
    let strsub    = String.sub
    let zero      = Bigint (Pos, [])

    let charlist_of_string str = 
        let last = strlen str - 1
        in  let rec charlist pos result =
            if pos < 0
            then result
            else charlist (pos - 1) (str.[pos] :: result)
        in  charlist last []

    let bigint_of_string str =
        let len = strlen str
        in  let to_intlist first =
                let substr = strsub str first (len - first) in
                let digit char = int_of_char char - int_of_char '0' in
                map digit (reverse (charlist_of_string substr))
            in  if   len = 0
                then zero
                else if   str.[0] = '_'
                     then Bigint (Neg, to_intlist 1)
                     else Bigint (Pos, to_intlist 0)

    let string_of_bigint (Bigint (sign, value)) =
        match value with
        | []    -> "0"
        | value -> let reversed = reverse value
                   in  strcat ""
                       ((if sign = Pos then "" else "-") ::
                        (map string_of_int reversed))

    let rec add' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let sum = car1 + car2 + carry
          in  sum mod radix :: add' cdr1 cdr2 (sum / radix)

	let rec sub' list1 list2 borrow = match (list1, list2, borrow) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], borrow   -> sub' list1 [borrow] 0
        | [], list2, borrow   -> sub' [borrow] list2 0
        | car1::cdr1, car2::cdr2, borrow ->
          let difference = car1 - car2  - borrow  (*Problem was that it was subtracting 1 - (-1) where -1 is the borrow *)
          in if(difference < 0) then 
					(difference + 10):: sub' cdr1 cdr2 (0 + 1) (*can't use -1 *)
				else difference :: sub' cdr1 cdr2 0
	

	let rec removezeros' list1 = 	match (list1) with
		| [] -> list1
		| car1::cdr1 ->
			if(List.hd(list1) = 0)
				then removezeros' (List.tl(list1))
			else (List.rev(list1))
				
				
	let rec trim' list1 = match (list1) with
        | [] -> list1
		| list1 -> let reversedNumber = List.rev(list1)
                   in removezeros' reversedNumber
	

		(*Return 0 if equal, 1 if list 1 is bigger, -1 if list2 is bigger*)
		(*
		Cases:
		1) list vs empty list
		2) empty list vs list
		3) empty list vs empty list
		4) list vs list
		The reason why we use car1::cdr1, [] instead
		of list1, [] is because list1 would match with
		empty lists as well which would interfere with
		the [], [] case
		*)
	let rec compare list1 list2 result = match (list1, list2) with
        | car1::cdr1, []       -> 1
        | [], car2::cdr2       -> -1
		| [], []		  -> result 
        | car1::cdr1, car2::cdr2 ->
			if car1 > car2
				then compare cdr1 cdr2 1
			else if car1 < car2
				then compare cdr1 cdr2 (-1)
			else 
				compare cdr1 cdr2 result
		
	let rec mul' listMultiplier listMultiplicand listPowerOf2 =
			if (compare listPowerOf2 listMultiplier 0) = 1		
				then listMultiplier, [0] 
			else let listRemainder, listProduct = 
				mul' listMultiplier (add' listMultiplicand listMultiplicand 0) (add' listPowerOf2 listPowerOf2 0)
				in if (compare listRemainder listPowerOf2 0) = -1
					then listRemainder, listProduct
					else 
						(trim'(sub' listRemainder listPowerOf2 0), trim'(add' listProduct listMultiplicand 0))

	
	let rec divrem' listDividend listDivisor listPowerOf2=
	if (compare listDivisor listDividend 0) = 1
		then [0], listDividend
		else let listQuotient, listRemainder = 
				divrem' listDividend (add' listDivisor listDivisor 0) (add' listPowerOf2 listPowerOf2 0) 
			in if (compare listRemainder listDivisor 0) = -1
				then listQuotient, listRemainder
				else 
				(trim'(add' listQuotient listPowerOf2 0), trim'(sub' listRemainder listDivisor 0))
				
	(*POWER NEEDS TO BE COMPLETED *)
	let rec power' base expt result = match expt with
		| [0]							-> result
		| expt	-> power' base (sub' expt [1] 0) (add' result base 0)
	(*
	let rec power' (base, expt, result) = match expt with
    | 0                   -> result
    | expt                -> power' (base, expt - 1, base *. result)
	*)
	
	
		(*It works when there's no carrying that occurs *)
    let add (Bigint (sign1, list1)) (Bigint (sign2, list2)) =
        if sign1 = sign2
        then Bigint (sign1, add' list1 list2 0)
		else if (sign1 = Neg && sign2 = Pos)
			then (
				if (compare list1 list2 0) = 1
					then Bigint (sign1, trim'(sub' list1 list2 0))
				else Bigint(sign2, trim'(sub' list2 list1 0))
			
			)
		else if (sign1 = Pos && sign2 = Neg)
			then (
				if (compare list1 list2 0) = 1
					then 
						Bigint (sign1, trim'(sub' list1 list2 0))
				else Bigint (sign2, trim'(sub' list2 list1 0))
			)
        else zero
		
		(*
		Cases
		sign1 = sign2:
			list1 > list2: Positive difference
			list1 < list2: negative difference (list2-list1)
		*)
	let sub (Bigint (sign1, list1)) (Bigint (sign2, list2)) =
        if sign1 = sign2
        then (
				if (compare list1 list2 0) = 1
					then Bigint (sign1, trim'(sub' list1 list2 0))
				else Bigint(Pos, trim'(sub' list2 list1 0))
			)
		else if (sign1 = Neg && sign2 = Pos)
			then (
				if (compare list1 list2 0) = 1
					then Bigint (Neg, add' list1 list2 0)
				else Bigint(Neg, add' list2 list1 0)
			)
		else if (sign1 = Pos && sign2 = Neg)
			then (
				if (compare list1 list2 0) = 1
					then 
						Bigint (Pos, add' list1 list2 0)
				else Bigint (Pos, add' list2 list1 0)
			)
        else zero

	(*_ is a wildcard. It means, I don't care what
		mul' gives me as the first parameter. 
		just give me the second 
	*)
    let mul (Bigint (sign1, list1)) (Bigint (sign2, list2)) = 
		if (sign1 = Neg && sign2 = Pos || sign1 = Pos && sign2 = Neg)
			then 
				let _, product = mul' list1 list2 [1]
				in Bigint(Neg, product)
		else 
			let _, product = mul' list1 list2 [1]
				in Bigint(Pos, product)


    let div = add

    let rem = add

    let pow = add

end
