(* Kylie Heiland
  This program quick sorts an int list into ascending order.
  CSCI421
  3/10/23*)

(*Quick sorts an int list*)
fun quickSort [] = [] (*If the list is empty, an empty list is returned*)
  | quickSort [oneElement] = [oneElement] (*If the list has one element, no sorting is needed and that element is returned*)
  | quickSort(pivot::rest) =  (*Pivot is the head of the list.*)
  let
    val(smaller, larger) = List.partition(fn y => y < pivot) rest (*smaller is all of the values that are < pivot, which is found through List.partition. larger is all that are >= pivot.*) 
  in
    quickSort(smaller) @ [pivot] @ quickSort larger (*Combines the smaller sublist with the pivot and then the larger sublist*)
  end;

quickSort ([~3, 2, 1, 4, 6, 0, 7, ~2]);