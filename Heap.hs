module Heap
(pushHeap,popHeap,heapSort) where

swap::[a]->Int->Int->[a]
swap arr index1 index2
    |l==r=arr
    |otherwise=(take l arr)++[arr !! r]++(take (r-l-1) (drop (l+1) arr))++[arr !! l]++(drop (r+1) arr)
    where l=min index1 index2
          r=max index1 index2

pushHeap::(Ord a)=>[a]->a->[a]
pushHeap arr val=let addedArr=arr++[val] in ordHeap addedArr $ length addedArr - 1

ordHeap::(Ord a)=>[a]->Int->[a]
ordHeap arr 0=arr
ordHeap arr childIndex
    |arr!!childIndex>arr!!parentIndex=ordHeap (swap arr parentIndex childIndex) parentIndex
    |otherwise=arr
    where
        parentIndex=(childIndex-1)`div`2

popHeap::(Ord a)=>[a]->[a]
popHeap []=[]
popHeap [x]=[]
popHeap arr=
    let removedArr=last arr:drop 1 (take ((length arr) - 1) arr)
    in ordHeapR removedArr 0

ordHeapR::(Ord a)=>[a]->Int->[a]
ordHeapR arr addedIndex=
    if childIndex2<len
        then if arr!!childIndex1>arr!!addedIndex||arr!!childIndex2>arr!!addedIndex
            then if arr!!childIndex1>arr!!childIndex2
                then ordHeapR (swap arr addedIndex childIndex1) childIndex1
                else ordHeapR (swap arr addedIndex childIndex2) childIndex2
            else arr
        else if childIndex1>=len
            then arr
            else if arr!!childIndex1>arr!!addedIndex
                    then ordHeapR (swap arr addedIndex childIndex1) childIndex1
                    else arr
    where
        childIndex1=addedIndex*2+1
        childIndex2=addedIndex*2+2
        len=length arr

heapSort::(Ord a)=>[a]->[a]
heapSort []=[]
heapSort arr=_heapSort (foldl pushHeap [] arr)
_heapSort::(Ord a)=>[a]->[a]
_heapSort []=[]
_heapSort heap=(head heap):(_heapSort $popHeap heap)
