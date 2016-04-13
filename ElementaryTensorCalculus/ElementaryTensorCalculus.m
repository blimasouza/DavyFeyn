(* ::Package:: *)

(* ::Title:: *)
(*Elementary Tensor Calculus*)


BeginPackage["ElementaryTensorCalculus`",{"Notation`"}]
Print["ElementaryTensorCalculus Version 1.0, 12 April 2016."];


Unprotect\[NonBreakingSpace]@@\[NonBreakingSpace]Names["ElementaryTensorCalculus`*"];
ClearAll\[NonBreakingSpace]@@\[NonBreakingSpace]Names["ElementaryTensorCalculus`*"];


(* ::Section:: *)
(*Usage messages*)


If[!ValueQ[ElementaryTensorCalculus::usage],
ElementaryTensorCalculus::usage="ElementaryTensorCalculus is a simple package that \
defines the notions of scalar product ( sc[v,w] ), vector ( sc[v,{\[Mu]}] ), metric tensor \
( sc[{\[Mu]},{\[Nu]}] ), derivative with respect to a vector ( \[Delta][f,sc[x,{\[Mu]}]] ) and Levi-Civita \
symbol ( \[Epsilon][\!\(\*SubscriptBox[\(\[Mu]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Mu]\), \(2\)]\),...\!\(\*SubscriptBox[\(\[Mu]\), \(DimensionOfSpacetime\)]\)] )."	
];

If[!ValueQ[\[Epsilon]::usage],
\[Epsilon]::usage="\[Epsilon][\[Mu],\[Nu],\[Rho],...] defines the totally antisymmetric Levi-Civita symbol."	
];

If[!ValueQ[sc::usage],
sc::usage="sc[{A},{B}] defines a notion of scalar product between two vectors and at the 1
same time will be used to represent vectors and the metric tensor."	
];

If[!ValueQ[\[Delta]::usage],
\[Delta]::usage="\[Delta][expr,x] defines a notion of derivative that can be applied to our tensors."
];

If[!ValueQ[SetChainRule::usage],
SetChainRule::usage="SetChainRule[boolean] sets whether \[Delta] can or cannot use the chain rule."
];

If[!ValueQ[SetDimensionOfSpacetime::usage],
SetDimensionOfSpacetime::usage="SetDimensionOfSpacetime[D] sets to D the dimension of \
spacetime. By default it is set to 3."
];

If[!ValueQ[ShowDimensionOfSpacetime::usage],
ShowDimensionOfSpacetime::usage="ShowDimensionOfSpacetime[] displays the current value \
for the dimension of spacetime."
];

If[!ValueQ[SetMetricSignature::usage],
SetMetricSignature::usage="SetMetricSignature[p] fixes the signature of the metric to \
have p plus signs. By default the metric is set to be Lorentzian, mostly minus, i.e. p = 1."
];

If[!ValueQ[ShowMetricSignature::usage],
ShowMetricSignature::usage="ShowMetricSignature[] displays the metric signature in current use."	
];

If[!ValueQ[AddConstantToList::usage],
AddConstantToList::usage="AddConstantToList[x] adds x to the list of constants with respect \
to the derivative \[Delta]."
];

If[!ValueQ[ShowConstantsFromList::usage],
ShowConstantsFromList::usage="ShowConstantsFromList[] prints the current list of constants \
with respect to the derivative \[Delta]."
];

If[!ValueQ[VectorInsideEpsilon::usage],
VectorInsideEpsilon::usage="InternalVectorEpsilon is a boolean that decides if we want to \
represent the contraction of a vector and the Levi-Civita symbol by using the vector as an \
argument of the \[Epsilon] or not. By default it is set to False, which means that vectors are not \
used as arguments of the \[Epsilon]."
];

If[!ValueQ[SchoutenIdentity::usage],
SchoutenIdentity::usage="SchoutenIdentity[sc[A,B]\[Epsilon][C,D,\[Ellipsis]]][expr] replace all instances of \
the term sc[A,B]\[Epsilon][C,D,\[Ellipsis]] in expr using the Schouten identity."
];


Begin["`Private`"]


(* ::Section:: *)
(*To do*)


(* ::Item:: *)
(*Implement the notation using TemplateBox.*)


(* ::Section:: *)
(*Definitions*)


(* ::Subsection:: *)
(*Styles*)


(*SetOptions[EvaluationNotebook[], 
 StyleDefinitions -> 
  Notebook[{Cell[StyleData[StyleDefinitions -> "Default.nb"]],
	Cell[StyleData["Metric"],TemplateBoxOptions->{DisplayFunction->(SubscriptBox["\[Eta]", RowBox[{#, " ", #2}]]& ), Tooltip->Automatic}],
	Cell[StyleData["Vector"],TemplateBoxOptions->{DisplayFunction->(SubscriptBox[#, #2]& ),Tooltip->Automatic}],
	Cell[StyleData["ScalarProduct"],TemplateBoxOptions->{DisplayFunction->(RowBox[{"(", RowBox[{#, "\[CenterDot]", #2}], ")"}]& ), Tooltip->Automatic}],
	Cell[StyleData["IndexedVector"],TemplateBoxOptions->{DisplayFunction->(SubscriptBox[#, RowBox[{#2, ",", #3}]]& ),Tooltip->Automatic}],
	Cell[StyleData["VectorDerivative"],TemplateBoxOptions->{DisplayFunction->(RowBox[{FractionBox["\[Delta]", RowBox[{"\[Delta]", SuperscriptBox[#, #2]}]], #3}]& ),Tooltip->Automatic}]
    }]
 ]*)


(* ::Subsection:: *)
(*Set dimension*)


SetDimensionOfSpacetime[D_:3] := Module[{}, DimensionOfSpacetime = D];


ShowDimensionOfSpacetime[] := Print[DimensionOfSpacetime];


SetMetricSignature[p_:1] := Module[{}, MetricSignature = p];


ShowMetricSignature[] := Table["+",{i,MetricSignature}]~Join~Table["-",{i,DimensionOfSpacetime - MetricSignature}];


(* Set default value for DimensionOfSpacetime and MetricSignature *)

SetDimensionOfSpacetime[];
SetMetricSignature[];


(* ::Subsection:: *)
(*Definition of Levi-Civita*)


\[Epsilon][a__]:=Signature[{a}] \[Epsilon]@@Sort[{a}]/;Not@OrderedQ[{a}]


\[Epsilon][a__]:=0/;OrderedQ[{a}]\[And]Signature[{a}]==0


\[Epsilon][a___,b_?NumericQ c_,d___]:=b \[Epsilon][a,c,d]


\[Epsilon][a___,b_?NumericQ,d___] := 0


\[Epsilon][a___,b_+c_,d___] := \[Epsilon][a,b,d] + \[Epsilon][a,c,d]


(* ::Subsection:: *)
(*Scalar Product*)


Attributes[sc]={Orderless};


sc/:sc[a_,0]:=0;


sc/:sc[a_+b_,c_]:=sc[a,c]+sc[b,c];


sc/:sc[a_,num_ b_]:=num sc[a,b]/;NumericQ[num];


sc/:sc[a_,sc[x_,y_] b_]:=sc[x,y]sc[a,b];


sc/:sc[{mu_},{mu_}]:=DimensionOfSpacetime; 


sc/:sc[p_,{mu_}] sc[q_,{mu_}]:=sc[p,q];


sc/:sc[{mu_},{b_}] \[Epsilon][a___,b_,c___]:=\[Epsilon][a,mu,c];


sc/:sc[{b_},{mu_}] \[Epsilon][a___,b_,c___]:=\[Epsilon][a,mu,c];


(*** Default value for VectorInsideEpsilon is False ***)
VectorInsideEpsilon=True;


sc/:sc[p_,{b_}] \[Epsilon][a___,b_,c___] :=
If[VectorInsideEpsilon,
	\[Epsilon][a,p,c],
	HoldForm[sc[p,{b}] \[Epsilon][a,b,c]]
]


sc/:sc[p_,{mu_}]^n_:=sc[p,p]^(n/2)/;EvenQ[n];


sc/:sc[p_,{mu_}]^n_:=sc[p,p]^((n-1)/2) sc[p,{mu}]/;OddQ[n];


(* ::Subsection:: *)
(*Derivative *)


\[Delta][a_ b_,x_]:=\[Delta][a,x]b+a \[Delta][b,x]; 


\[Delta][a_ ^n_,x_]:=n \[Delta][a,x]a^(n-1); 


\[Delta][a_ +b_,y_]:=\[Delta][a,y]+ \[Delta][b,y]; 


\[Delta][a_/;NumericQ[a],y_]:=0


\[Delta][sc[X_,{mu_}],sc[Y_,{nu_}]]:=If[X===Y,sc[{mu},{nu}],0]; 


\[Delta][sc[X_,Y_],sc[Z_,{mu_}]]:=If[X===Z,sc[Y,{mu}],0]+If[Y===Z,sc[X,{mu}],0]; 


\[Delta][sc[{mu_},{nu_}],a_]:=0; 


ListOfConstants={}; 


AddConstantToList[a_] := (ListOfConstants = Union[ListOfConstants~Join~{a}])


ShowConstantsFromList[] := Print[ListOfConstants]


\[Delta][b_/;MemberQ[ListOfConstants,b],a_]:=0; 


Options[\[Delta]]={ChainRule -> True}


SetChainRule[bool_] := SetOptions[\[Delta], ChainRule -> bool]


\[Delta][\[Epsilon][a__],sc[p_,{\[Mu]_}]]:=If[FreeQ[{a},p],0,\[Epsilon][a]/.p->\[Mu]]


\[Delta][f_[a_,b___],c_, OptionsPattern[]]:=
If[OptionValue[ChainRule],
	Module[{x},D[f[x,b],x]/.x->a]\[Delta][a,c]+Sum[Module[{x},D[f@@Join[{a},Take[{b},{1,j-1}],{x},Take[{b},{j+1,Length[{b}]}]],x]/.x->{b}[[j]]]\[Delta][{b}[[j]],c],{j,1,Length[{b}]}],
	HoldForm[\[Delta][f[a,b],c]]
];


(* ::Subsection:: *)
(*Schouten identity*)


SchoutenIdentity[x_][expr_]:=
Module[{scTerm,epsilonTerm,border1,border2,possibleTerms,replacement},
	(* take the arguments inside sc *)
	scTerm=List@@Select[x,Head[#]==sc&];

	(* take the arguments inside \[Epsilon] *)
	epsilonTerm=List@@Select[x,Head[#]==\[Epsilon]&];

	(* treat the left border of the list of terms in the answer *)
	border1={{{scTerm[[1]],epsilonTerm[[1]]},Flatten@Join[{scTerm[[2]]},epsilonTerm[[2;;Length[epsilonTerm]]]]}};

	(* treat the right border of the list of terms in the answer *)
	border2={{{scTerm[[1]],epsilonTerm[[Length[epsilonTerm]]]},Flatten@Join[epsilonTerm[[1;;Length[epsilonTerm]-1]],{scTerm[[2]]}]}};

	(* all terms in the Schouten identity *)
	possibleTerms=
	Join[
		border1,
		Table[{{scTerm[[1]],epsilonTerm[[i]]},Flatten@Join[epsilonTerm[[1;;i-1]],{scTerm[[2]]},epsilonTerm[[i+1;;Length[epsilonTerm]]]]},{i,2,Length[epsilonTerm]-1}],
		border2
	];

	(* from list of possible terms to actual formula *)
	replacement=Total[Map[sc@@#[[1]]\[Epsilon]@@#[[2]]&,possibleTerms]];

	(* return the given expr after use of the Schouten identity *)
	expr//.x->replacement
]


(* ::Subsection:: *)
(*Notation*)


Notation[ParsedBoxWrapper[SubscriptBox["\[Eta]", RowBox[{"\[Mu]_", ",", "\[Nu]_"}]]]\[DoubleLongLeftRightArrow]ParsedBoxWrapper[RowBox[{"sc", "[", RowBox[{RowBox[{"{", "\[Mu]_", "}"}], ",", RowBox[{"{", "\[Nu]_", "}"}]}], "]"}]]]


Notation[ParsedBoxWrapper[SubscriptBox["X_", "\[Mu]_"]] \[DoubleLongLeftArrow] ParsedBoxWrapper[RowBox[{"sc", "[", RowBox[{"X_", ",", RowBox[{"{", "\[Mu]_", "}"}]}], "]"}]]]


Notation[ParsedBoxWrapper[SubscriptBox["X_", RowBox[{"i_", ",", "\[Mu]_"}]]] \[DoubleLongLeftArrow] ParsedBoxWrapper[RowBox[{"sc", "[", RowBox[{SubscriptBox["X_", "i_"], ",", RowBox[{"{", "\[Mu]_", "}"}]}], "]"}]]]


Notation[ParsedBoxWrapper[RowBox[{"(", RowBox[{"X_", "\[CenterDot]", "Y_"}], ")"}]] \[DoubleLongLeftRightArrow] ParsedBoxWrapper[RowBox[{"sc", "[", RowBox[{"X_", ",", "Y_"}], "]"}]]]


Notation[ParsedBoxWrapper[FractionBox[RowBox[{"\[Delta]", " ", "X_"}], RowBox[{"\[Delta]", " ", SuperscriptBox["Y_", "a_"]}]]] \[DoubleLongLeftArrow] ParsedBoxWrapper[RowBox[{"\[Delta]", "[", RowBox[{"X_", ",", RowBox[{"sc", "[", RowBox[{"Y_", ",", RowBox[{"{", "a_", "}"}]}], "]"}]}], "]"}]]] 


(* ::Subsection:: *)
(*End package*)


End[]


Protect\[NonBreakingSpace]@@\[NonBreakingSpace]{\[Delta],sc};


EndPackage[]
