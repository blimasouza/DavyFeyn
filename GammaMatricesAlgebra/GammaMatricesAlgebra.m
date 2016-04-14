(* ::Package:: *)

(* ::Title:: *)
(*GammaMatricesAlgebra*)


BeginPackage["GammaMatricesAlgebra`",{"ElementaryTensorCalculus`"}]
Print["GammaMatricesAlgebra Version 1.0, 14 April 2016."];


Unprotect\[NonBreakingSpace]@@\[NonBreakingSpace]Names["GammaMatricesAlgebra`*"];
ClearAll\[NonBreakingSpace]@@\[NonBreakingSpace]Names["GammaMatricesAlgebra`*"];


If[!ValueQ[GammaMatricesAlgebra::usage],
GammaMatricesAlgebra::usage="GammaMatricesAlgebra is a package that implements algebra of gamma matrices \
in arbitrary dimension using the package ElementaryTensorCalculus.";
];

If[!ValueQ[\[Gamma]::usage],
\[Gamma]::usage="\[Gamma][\[Mu]] defines a gamma matrix."
];

If[!ValueQ[tr::usage],
GammaMatrixTrace::usage="GammaMatrixTrace[expr] takes the traces of gamma matrices in expr."
];

If[!ValueQ[Slashed::usage],
Slashed::usage="Slashed[p,a] denotes a vector \!\(\*SuperscriptBox[\(p\), \(a\)]\) contracted with \
a gamma matrix \!\(\*SubscriptBox[\(\[Gamma]\), \(a\)]\)."
];


Begin["`Private`"]


(* set dimension to be the one defined in ElementaryTensorCalculus *)

DIM := ElementaryTensorCalculus`Private`DimensionOfSpacetime;


(* set the metric signature to be the one defined in ElementaryTensorCalculus *)

SIGN=ElementaryTensorCalculus`Private`MetricSignature;


If[!ValueQ[SetGammaMatricesRepresentation::usage],
SetGammaMatricesRepresentation::usage="SetGammaMatricesRepresentation[x] sets the sign of the \
trace of D gamma matrices in D dimensions, for odd D."
];
SetGammaMatricesRepresentation[x_:1] := Module[{}, phase = x];

(* set it to the default value *)

SetGammaMatricesRepresentation[];


ClearAll[\[Gamma]]
\[Gamma]/:\[Gamma][a__]:=Signature[{a}]\[Gamma]@@Sort[{a}]/;!OrderedQ[{a}];
\[Gamma]/:\[Gamma][a__]:=0/;Signature[{a}]==0;
\[Gamma]/:\[Gamma][a__]:=0/;Length[{a}]>DIM;
\[Gamma]/:\[Gamma][]:=1;


checklist[L_List]:=L
checklist[L_]:=(Print["wrong input"];Abort[])


(*
	gammaeta constructs part of the list of terms that appears in the product of the \
	antisymmetric product of gamma matrices by a new gamma matrix.
*)

ClearAll[gammaeta];
gammaeta[L_,range_,\[Nu]_]:=(\[Gamma]@@checklist[Most[ReplacePart[L,#->L[[-1]]]]]sc[{L[[#]]},{\[Nu]}]&/@range)


(*
	join uses gammaeta to produce the full list of terms that appears in the product of \
	the antisymmetric product of gamma matrices by a new gamma matrix.
*)

ClearAll[join];
join[\[Gamma][\[Mu]___],\[Gamma][\[Nu]_]]:=
Module[{n,L,r},
	L={\[Mu]};
	n=Length[L];
	If[n==0,Return[{\[Gamma][\[Nu]]}]];
	r=(-checklist@gammaeta[L,Range[n-1],\[Nu]])~Join~(checklist@gammaeta[L,{n},\[Nu]]);
	{\[Gamma][\[Mu],\[Nu]]}~Join~r
]


ClearAll[CenterDot]
rules={
CenterDot[y___,a_+b_,x___] :> CenterDot[y,a,x] + CenterDot[y,b,x],
CenterDot[y___,a_*b_,x___] /; FreeQ[a,\[Gamma]|CenterDot] :> a*CenterDot[y,b,x],
CenterDot[a___,SubStar[\[Gamma]],\[Gamma][b__],c___] :> (-1)^Length[{b}] CenterDot[a,\[Gamma][b],SubStar[\[Gamma]],c],
CenterDot[\[Gamma][\[Mu]__],\[Gamma][\[Nu]_],x___] :> Total[CenterDot[#,x]&/@join[\[Gamma][\[Mu]],\[Gamma][\[Nu]]]],
CenterDot[y___,a_,x___]/;FreeQ[a,\[Gamma]|CenterDot] :> a*CenterDot[y,x]};


GammaMatrixTrace[x___]:=
Module[{r,ind},

	(* apply the Clifford algebra repeatedly *)
	r=x//.rules//Expand;

	(* throw to zero all the terms that do not contribute to the trace *)
	r=r/.CenterDot[\[Gamma][a__]]:>0/;((OddQ[DIM]&&1<=Length[{a}]<DIM)||(EvenQ[DIM]&&1<=Length[{a}]<=DIM));
	r=r/.CenterDot[SubStar[\[Gamma]]]->0;
	r=r/.CenterDot[\[Gamma][a__],SubStar[\[Gamma]]]:>0/;Length[{a}]<DIM;

	(* CenterDot[] is the identity inside of the trace *)
	r=r/.CenterDot[]:>(2^Floor[DIM/2]);

	(* if DIM is odd, replace the product of DIM gamma matrices by an epsilon *)
	r=r/.CenterDot[\[Gamma][a__]]:>(phase*I^((3DIM-1)/2-SIGN) \[Epsilon][a]2^Floor[DIM/2])/;(OddQ[DIM]&&Length[{a}]==DIM);

	(* if DIM is even, replace \[Gamma]* by an epsilon times the product of DIM gamma matrices *)
	r=r/.CenterDot[\[Gamma][a__],SubStar[\[Gamma]]]:>-((-I)^(DIM/2+1)/DIM!)\[Epsilon]@@Table[Subscript[ind, i],{i,DIM}]GammaMatrixTrace[\[Gamma][a],Sequence@@(\[Gamma]/@Table[Subscript[ind, i],{i,DIM}])]//Expand;

	(* return the result *)
	r
];


(* shortcut for the contraction of a vector with a gamma matrix *)

Slashed[p_,a_] := \[Gamma][a]sc[p,{a}];


End[]


EndPackage[]
