(* ::Package:: *)

(* ::Title:: *)
(*GammaMatricesAlgebra*)


BeginPackage["GammaMatricesAlgebra`",{"ElementaryTensorCalculus`"}]
Print["GammaMatricesAlgebra Version 1.0, 16 March 2016."];


Unprotect\[NonBreakingSpace]@@\[NonBreakingSpace]Names["GammaMatricesAlgebra`*"];
ClearAll\[NonBreakingSpace]@@\[NonBreakingSpace]Names["GammaMatricesAlgebra`*"];


GammaMatricesAlgebra::usage="GammaMatricesAlgebra is a package that implements algebra of gamma matrices 
in arbitrary dimension using the package ElementaryTensorCalculus.";

\[Gamma]::usage="\[Gamma][\[Mu]] defines a gamma matrix.";
tr::usage="tr[expr] takes the traces of gamma matrices in expr.";
Slashed::usage="Slashed[p,a] denotes a vector \!\(\*SuperscriptBox[\(p\), \(a\)]\) contracted with a gamma matrix \!\(\*SubscriptBox[\(\[Gamma]\), \(a\)]\).";
NumQ::usage="NumQ[expr] is True is expr do not contain gamma matrices and False otherwise.";
EvenGammas::usage="";

(*DimensionOfSpacetime::usage="DimensionOfSpacetime defines the dimension of spacetime. Also defined in ElementaryTensorCalculus.";*)


Begin["`Private`"]


(* set dimension to be the one defined in ElementaryTensorCalculus *)

DIM := ElementaryTensorCalculus`Private`DimensionOfSpacetime;


ClearAll[CenterDot]


SetAttributes[CenterDot,{Flat,OneIdentity}]


(* Clifford Algebra *)

gammaRule1=\[Gamma][a_]\[CenterDot]\[Gamma][b_]:>2sc[{a},{b}]-\[Gamma][b]\[CenterDot]\[Gamma][a]/;Not@OrderedQ[{a,b}];


(* Contraction of two gamma matrices *)

gammaRule2=\[Gamma][a_]\[CenterDot]\[Gamma][a_]:>DIM;


(*** This implementation of epsilonRule works for any value DimensionOfSpacetime  ***)

epsilonRule:=\[Epsilon][args1__]\[Epsilon][args2__]:>Det[Outer[sc[{#1},{#2}]&,{args1},{args2}]];


CenterDot/:d___\[CenterDot](a_+b_)\[CenterDot]c___:=d\[CenterDot]a\[CenterDot]c+d\[CenterDot]b\[CenterDot]c


Slashed[p_,a_]:=\[Gamma][a]sc[p,{a}]


(* Everything that has NumQ set to True can come out of the trace *)

NumQ[sc[_,_]|sc[_,_]sc[_,_]|\[Epsilon][a__]|_?NumericQ|_?AtomQ]=True;


NumQ/:NumQ[x_^a_]:=NumQ[x]


NumQ/:NumQ[x_ + y_] := NumQ[x]/;NumQ[x]==NumQ[y]


NumQ/:NumQ[x_ y_]:=NumQ[x]/;NumQ[x]==NumQ[y]


(* Everything that has NumQ set to False cannot come out of the trace *)

NumQ[_]=False;


CenterDot/:a_\[CenterDot]b_:=a b/;(NumQ[a]\[Or]NumQ[b])


CenterDot/:c___\[CenterDot]a_\[CenterDot]b_\[CenterDot]d___:=a(c\[CenterDot]b\[CenterDot]d)/;NumQ[a]
CenterDot/:c___\[CenterDot]a_\[CenterDot]b_\[CenterDot]d___:=b(c\[CenterDot]a\[CenterDot]d)/;NumQ[b]


CenterDot/:d___\[CenterDot](c__ a_)\[CenterDot]b___:=c(d\[CenterDot]a\[CenterDot]b)/;NumQ[c]
CenterDot/:d___\[CenterDot](c_ a__)\[CenterDot]b___:=a(d\[CenterDot]c\[CenterDot]b)/;NumQ[a]


gammaRule={gammaRule1,gammaRule2};


(*(* this function takes a list with an even number of indices and return the tensorial structure of the trace of an even number of gammas *)
(* NON-RECURSIVE IMPLEMENTATION *)

evenGammas[list_]:=Module[{listOfPairs,relevantPermutations,cycles,signatures,permutationSignature},
permutationSignature[perm_?PermutationCyclesQ]:=Apply[Times,(-1)^(Length/@First[perm]-1)];

listOfPairs=DeleteDuplicates[Sort/@DeleteDuplicates[Partition[#,2]&/@Permutations[list],(Map[Sort,#1,1]\[Equal]Map[ Sort,#2,1])&]];

Print[listOfPairs];

relevantPermutations=(listOfPairs//.{A___,{B___,{i1_,i2_},C___},D___}:>{A,{B,i1,i2,C},D});

Print[relevantPermutations];

cycles=Table[FindPermutation[relevantPermutations\[LeftDoubleBracket]i\[RightDoubleBracket],list],{i,1,Length[relevantPermutations]}];

signatures=permutationSignature/@cycles;

Plus@@(signatures Times@@@ Map[sc[{#[[1]]},{#[[2]]}]&,listOfPairs,{2}])
]*)


(* this function takes a list with an even number of indices and return the tensorial structure of the trace of an even number of gammas *)
(* RECURSIVE IMPLEMENTATION *)

EvenGammas[list_]:=Module[{permutationSignature,cycles,swap,indices},

permutationSignature[perm_?PermutationCyclesQ]:=Apply[Times,(-1)^(Length/@First[perm]-1)];

swap[l_,m_]:=Permute[l,Cycles[{{Length[l]-1-m,Length[l]-1}}]];

indices[l_]:=FoldList[swap,l,Table[i,{i,1,Length[l]-2}]];

cycles[i_]:=FindPermutation[indices[list][[i]],list];

Sum[
permutationSignature[cycles[k]]tr[Take[indices[list][[k]],Length[list]-2]]First@MapThread[sc,{{Take[indices[list][[k]],{-2}]},{Take[indices[list][[k]],{-1}]}}]
,{k,1,Length[indices[list]]}
]//Expand
]


(* tr/:tr[\[Gamma][a_]]:=0 *)

tr/:tr[{a_}]:=0


tr/:tr[a_ b_]:=a tr[b]/;NumQ[a]


tr/:tr[a_+b_]:=tr[a]+tr[b]


tr/:tr[A_]:=2^Floor[DIM/2] A/;NumQ[A]


(* tr/:tr[\[Gamma][a_]\[CenterDot]\[Gamma][b_]]:=2^Floor[DimensionOfSpacetime/2] sc[{a},{b}] *)

tr/:tr[{a_,b_}]:=2^Floor[ElementaryTensorCalculus`Private`DimensionOfSpacetime/2] sc[{a},{b}]


(* tr[\[Gamma][Subscript[a, 1]]\[CenterDot] ... \[CenterDot]\[Gamma][Subscript[a, n]]] := tr[{Subscript[a, 1],...,Subscript[a, n]}] *)

tr/:tr[gammas_]:=tr[Table[gammas[[i,1]],{i,1,Length[gammas]}]]/;MatchQ[gammas, CenterDot[\[Gamma][_],a___,\[Gamma][_]]]


(* rules to compute traces of more than two gammas using a recursive method for the trace of an even number of gammas *)

tr/:tr[list_]:=
Which[
	EvenQ[Length[list]]\[And](Length[list] > 2),
	EvenGammas[list]//Expand,
	
	OddQ[Length[list]]\[And]EvenQ[DIM],
	0,

	OddQ[Length[list]]\[And](Length[list] < DIM),
	0,

	OddQ[Length[list]]\[And]OddQ[DIM]\[And](Length[list] >= DIM),
	Module[{firstTwo=Take[list, 2], rest=Drop[list, 2], dummies=Table[Subscript[mm, i],{i,1,DIM-2}]},
		I^(Floor[DIM/2]+2)/(DIM - 2)! \[Epsilon]@@(firstTwo~Join~dummies)tr[CenterDot@@(\[Gamma]/@(Reverse[dummies]~Join~rest))]+sc[{firstTwo[[1]]},{firstTwo[[2]]}]tr[CenterDot@@(\[Gamma]/@rest)]//Expand
	]
]


(*(* rules to compute traces of more than two gammas using a closed formula for the trace of an even number of gammas *)
(* NON-RECURSIVE IMPLEMENTATION *)

tr/:tr[list_]:=
Which[
	EvenQ[Length[list]]\[And](Length[list] > 2),
	2^Floor[DimensionOfSpacetime/2]evenGammas[list]//Expand,
	
	OddQ[Length[list]]\[And]EvenQ[DimensionOfSpacetime],
	0,

	OddQ[Length[list]]\[And](Length[list] < DimensionOfSpacetime),
	0,

	OddQ[Length[list]]\[And]OddQ[DimensionOfSpacetime]\[And](Length[list] >= DimensionOfSpacetime),
	Module[{firstTwo=Take[list, 2], rest=Drop[list, 2], dummies=Table[Subscript[mm, i],{i,1,DimensionOfSpacetime-2}]},
		I^(Floor[DimensionOfSpacetime/2]+2)/(DimensionOfSpacetime - 2)! \[Epsilon]@@(firstTwo~Join~dummies)tr[CenterDot@@(\[Gamma]/@(Reverse[dummies]~Join~rest))]+sc[{firstTwo[[1]]},{firstTwo[[2]]}]tr[CenterDot@@(\[Gamma]/@rest)]//Expand
	]
]*)


End[]


EndPackage[]
