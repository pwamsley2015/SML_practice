(*
	Write an SML function sumpairs that takes a list of integers as its only argument. 
	The function takes consecutive pairs of values, adding them and inserting the sums into a new list.
	If the original list has an odd length, then the first n-1 items are pairwise added, 
	and the n-th item is simply copied as is at the end of the new list. 

	Examples:
		sumpairs([8, 2, 3, 1, 5, 4]) would return [10, 4, 9]
		sumpairs([8, 2, 3, 1, 5, 4, 7]) would return [10, 4, 9, 7]
 *)
fun sumpairs(intList) = 
	if intList = [] then intList
	else if tl(intList) = [] then intList
	else [hd(intList) + hd(tl(intList))] @ sumpairs(tl(tl(intList))); 

(*
Write an SML function groupdupes that takes a list of integers as its only argument. The function takes runs of values (i.e., consectuive, duplicated values) in the list, copying those runs into separate sublists, returning a list of those sublists. Examples:
groupdupes([4,4,4,2,3,3,7,7,7,7,2,3,4,4,4]) would return [[4,4,4],[2],[3,3],[7,7,7,7],[2],[3],[4,4,4]]
groupdupes([8,6,7,5]) would return [[8],[6],[7],[5]]
*)

fun createListOfVal(intList, value) =
	if tl(intList) = [] andalso not (hd(intList) = value) then []
	else if tl(intList) = [] then [value]
	else if hd(intList) = value then [value] @ createListOfVal(tl(intList), value)
	else []; 

fun removeFirstDupeChain(intList) = 
	if intList = [] orelse length(intList) = 1 then []
	else if hd(intList) = hd(tl(intList)) then removeFirstDupeChain(tl(intList)) 
	else tl(intList); 

fun groupdupes(intList) = 
	if intList = [] then []
	else [createListOfVal(intList, hd(intList))] @ 
		 groupdupes(removeFirstDupeChain(intList));

(* Goldbach's conjecture says that every positive even number greater than 2 
	is the sum of two prime numbers. Write an SML function goldbach that takes a 
	single positive, even integer as its parameter and returns a list containing 
	the two prime numbers that sum to the parameter. 
	You may assume that the parameter is a positive, even integer. 
	Examples:
	goldbach(4000) would return [11, 3989] or [3989, 11]
	goldbach(3818) would return [79, 3739] or [3739, 79] *)

fun isPrimeHelper(n : int, d : int) = 
	if n mod 2 = 0 then false
	else if d <= 1 then true
	else if (n mod d) = 0 then false 
	else isPrimeHelper(n, d - 1); 

fun isPrime(n : int) = isPrimeHelper(n, n div 2);  (* lol how do you sqrt in sml *)

fun goldbachHelper(n : int, i : int) = 
	
	if isPrime(i) andalso isPrime(n - i) then [i] @ [n - i]
	else goldbachHelper(n, i + 1) 

fun goldbach(n : int) = goldbachHelper(n, 1); 

(* A bitwise AND takes two equal-length binary representations
   and performs the logical AND operation on each pair of the corresponding bits. 
   Thus, if both bits in the compared position are 1, the bit in the resulting 
   binary representation is 1. Otherwise, the result is 0. 
   For example the bitwise AND of 10110 and 10011 is 10010. 
   Write an SML function bitwise_and that takes two integers as parameters and 
   returns the 8-bit bitwise AND of the two integers' binary representations 
   as a list of 1s and 0s. You may assume that both parameters are in the range 0 through 255, inclusive.
   Examples:
		bitwise_and(31, 12) would return [0,0,0,0,1,1,0,0]
		bitwise_and(45, 172) would return [0,0,1,0,1,1,0,0]
		bitwise_and(65, 255) would return [0,1,0,0,0,0,0,1] *)

fun convertDecimalToBinary(num : int) = 
	if num <= 0 then []
	else convertDecimalToBinary(num div 2) @ [num mod 2]; 

(* Fills all 8 bits of the list *)
fun getListRepresentationOfBinary(rawBinary, lengthOfList) = 
	if length(rawBinary) = lengthOfList then rawBinary
	else [0] @ getListRepresentationOfBinary(rawBinary, lengthOfList - 1); 

(* Generic indexing *)
fun get(linkedList, indexOfElement, currPos) = 
	if indexOfElement = currPos then hd(linkedList)
	else get(tl(linkedList), indexOfElement, currPos + 1); 

(*	Awful awful awful runtime and magic numbers everywhere, 
	I swear I know how to code... *)
fun bitwiseAndHelper(n : int, m : int, index : int) = 
	if (index = 8) then []
	else if get(getListRepresentationOfBinary(convertDecimalToBinary(n), 8),
				index, 0) + 
	   		get(getListRepresentationOfBinary(convertDecimalToBinary(m), 8),
	   			index, 0) = 2
				then [1] @ bitwiseAndHelper(n, m, index + 1)
	else [0] @ bitwiseAndHelper(n, m, index + 1); 

fun bitwise_and(n : int, m : int) = bitwiseAndHelper(n, m, 0); 
