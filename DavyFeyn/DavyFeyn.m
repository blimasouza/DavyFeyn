(* ::Package:: *)

(* ::Title:: *)
(*DavyFeyn*)


BeginPackage["DavyFeyn`",{"ElementaryTensorCalculus`","Notation`"}]
Print["DavyFeyn Version 1.0, 13 April 2016."];


Unprotect\[NonBreakingSpace]@@\[NonBreakingSpace]Names["DavyFeyn`*"];
(*ClearAll\[NonBreakingSpace]@@\[NonBreakingSpace]Names["DavyFeyn`*"];*)


Begin["`Private`"]


(* ::Section:: *)
(*Labels*)


DIM := ElementaryTensorCalculus`Private`DimensionOfSpacetime;


SIGN := ElementaryTensorCalculus`Private`MetricSignature;


SetInternalMomentum[int_:Global`p] := Module[{}, p = int];
ShowInternalMomentum[] := Print[p];


SetAuxiliaryExternalMomenta[ext_:Global`q] := Module[{}, q = ext];
ShowAuxiliaryExternalMomenta[] := Print[q];


SetPolarizationVectors[pol_:Global`n] := Module[{}, n = pol];
ShowPolarizationVectors[] := Print[n];


SetMassParameter[mass_:Global`m] := Module[{}, m = mass];
ShowMassParameter[] := Print[m];


SetPhysicalExternalMomenta[phys_:Global`k] := Module[{}, k = phys]
ShowPhysicalExternalMomenta[] := Print[k]


SetRegulatorsTripleK[eps_:Global`\[CurlyEpsilon],regalpha_:Global`u,regbeta_:Global`v] := 
Module[{},
	\[CurlyEpsilon] = eps;
	u = regalpha;
	v = regbeta;
]


(* 
	Set the default label for the internal momentum, the external momenta 
	and the polarization vectors.
*)

SetInternalMomentum[];
SetAuxiliaryExternalMomenta[];
SetPhysicalExternalMomenta[];
SetPolarizationVectors[];
SetMassParameter[];
SetRegulatorsTripleK[];


ShowCurrentLabels[]:=Grid[{{"Dimension of spacetime ",DIM}, {"Signature of the metric ", ShowMetricSignature[]},{"Internal momentum ",p},{"Auxiliary external momenta ",q},{"Physical external momenta ",k},{"Polarization vectors ",n},{"Mass parameter ",m}},Frame->True,Background->{None,{{LightGray,White}}}]


(* ::Section::Closed:: *)
(*Properties of the external momenta*)


(*
	adds a definition for sc.
*)

Unprotect[ElementaryTensorCalculus`sc];
sc/:sc[Subscript[k, i_],Subscript[k, i_]] := 



\!\(\*SubsuperscriptBox[\(k\), \(i\), \(2\)]\);
Protect[ElementaryTensorCalculus`sc];


SimplifyExternalMomenta3pts[expr_] := 
Module[{sub,mixedProd},
	mixedProd[i_,j_] := 1/2 (Total@Table[Subscript[k,m]^2,{m,1,3}]-2(\!\(
\*SubsuperscriptBox[\(k\), \(i\), \(2\)] + 
\*SubsuperscriptBox[\(k\), \(j\), \(2\)]\)))/;i!=j;
	sub = sc[Subscript[k, i_],Subscript[k, j_]] :> mixedProd[i,j];
	expr//.sub//Simplify//Expand
];


(* ::Section:: *)
(*Useful functions*)


FullyEvaluate[3,case_][expr_] := (ToPhysicalExternalMomenta[3][EvaluateScalarIntegrals[3,case][Integrator[3, expr]]//FunctionExpand]//Expand)//SimplifyExternalMomenta3pts;


FullyEvaluate[N_,case_][expr_] := ToPhysicalExternalMomenta[N][EvaluateScalarIntegrals[N,case][Integrator[N, expr]]//FunctionExpand]//Expand;


EvenSector[expr_] := Select[expr, FreeQ[#,\[Epsilon]]&];


OddSector[expr_] := Select[expr, !FreeQ[#,\[Epsilon]]&];


(*
	GeneralizedFrobeniousSolve implements a generalization of the function FrobeniusSolve
	solving a system of Diophantine equations instead of only one.
*)

GeneralizedFrobeniousSolve[list_,num_]:=
Module[{c,instances,conditions,max},
	(* set the max number of solutions to be returned by FindInstance *)
	max=Length[list]^10;

	(* set of equations stating that all the c_i are \[GreaterEqual] 0. *)
	conditions=Table[Subscript[c, i]>=0,{i,1,Length[list]}];

	(* solve the system of Diophantine equations using FindInstance *)
	instances=FindInstance[And@@({Sum[Subscript[c, i] list[[i]],{i,1,Length[list]}]==num}~Join~conditions),
	Table[Subscript[c, i],{i,1,Length[list]}],Integers,max];
	
	(* return all the solutions found *)
	Table[Subscript[c, i],{i,1,Length[list]}]/.instances
];


(*
	This function generates all the possible EVEN tensor structures in a correlation function.
*)

GenerateEvenTensorStructures[tensorialities__]:=
Module[{numPts=Length[{tensorialities}],tensorsAvailable,scalarsAvailable,weight,weights,tensors,scalars,weightsScalars},
	(* set of vectors available to construct tensor structures *)
	tensorsAvailable[num_]:=Table[Subscript[k, i],{i,1,num-1}]~Join~Table[Subscript[n, i],{i,1,num}];

	(* all scalars that can be constructed out of the vectors *)
	scalarsAvailable[list_]:=Union[Select[Outer[sc[#1,#2]&,list,list]//Flatten,!FreeQ[#,n]&]];

	(* 
		given a scalar, returns a list of numbers stating how many times 
		each polarization vector appears 
	*)
	weight[num_,scalar_]:=Table[Count[scalar,Subscript[n, i]],{i,1,num}];

	(* given a list of scalars, apply weight to every element in the list *)
	weights[num_,list_]:=weight[num,#]&/@list;

	tensors=tensorsAvailable[numPts];
	scalars=scalarsAvailable[tensors];
	weightsScalars=weights[numPts,scalars];

	(* 
		construct all possible combinations of tensor structure that have the 
		appropriate number of polarization vectors. 
	*)
	Times@@MapThread[Power[#1,#2]&,{scalars,#}]&/@GeneralizedFrobeniousSolve[weightsScalars,{tensorialities}]
];


GenerateOddTensorStructures[tensorialities__] := Select[GenerateTensorStructures[tensorialities],!FreeQ[#,\[Epsilon]]&];


GenerateTensorStructures[tensorialities__]:=
Module[{numPts=Length[{tensorialities}],tensorsAvailable,evenScalarsAvailable,oddScalarsAvailable,weight,weights,scalars},
	(* list of all vectors available *)
	tensorsAvailable[num_]:=Table[Subscript[k, i],{i,1,num-1}]~Join~Table[Subscript[n, i],{i,1,num}];

	(* list of all even scalar monomials that we may form by contracting our vectors *)
	evenScalarsAvailable[num_]:=Union[Select[Outer[sc[#1,#2]&,tensorsAvailable[num],tensorsAvailable[num]]//Flatten,!FreeQ[#,n]&]];

	(* list of all odd scalar monomials that we may form by contracting our vectors *)
	oddScalarsAvailable[num_]:=Select[DeleteDuplicatesBy[\[Epsilon]@@@Tuples[tensorsAvailable[num],DIM],Abs],!MatchQ[#,0]&];
	
	(* for a given scalar returns a list of with the number of times that Subscript[n, i] appears in the scalar *)
	weight[num_,scalar_]:=Table[Count[scalar,Subscript[n, i]],{i,1,num}];

	(* apply weight for all the possible monomials *)
	weights[num_]:=weight[num,#]&/@(evenScalarsAvailable[num]~Join~oddScalarsAvailable[num]);

	scalars=evenScalarsAvailable[numPts]~Join~oddScalarsAvailable[numPts];

	Select[Times@@MapThread[Power[#1,#2]&,{scalars,#}]&/@GeneralizedFrobeniousSolve[weights[numPts],{tensorialities}],FreeQ[#,\[Epsilon][__]^_]\[And](Count[#,\[Epsilon][__]])<=1&]
];


CheckTransversality[mom_,pol_][expr_] := Module[{mm},sc[mom,{mm}]\[Delta][expr,sc[pol,{mm}]]//Expand];


CheckTracelessness[pol_][expr_]:=Module[{mm},\[Delta][#,sc[pol,{mm}]]&@\[Delta][#,sc[pol,{mm}]]&@expr//Expand];


(* Error message of GeneralEvenContactTerm *)
GeneralEvenContactTerm::BadConstant="The label used for the constants is already in use. Try another label.";

GeneralEvenContactTerm[args__,consts_List:{Global`a}]/;If[ValueQ[consts],Message[GeneralEvenContactTerm::badConst];False,True] :=
Module[{structures},
	structures=GenerateEvenTensorStructures[args];
	AddConstantToList[First[consts]];
	Total[Table[Subscript[First[consts], i],{i,1,Length[structures]}]structures]
];


(* Error message of GeneralOddContactTerm *)
GeneralOddContactTerm::BadConstant="The label used for the constants is already in use. Try another label.";

GeneralOddContactTerm[args__,consts_List:{Global`b}]/;If[ValueQ[consts],Message[GeneralEvenContactTerm::badConst];False,True] :=
Module[{structures},
	structures=GenerateOddTensorStructures[args];
	AddConstantToList[First[consts]];
	Total[Table[Subscript[First[consts], i],{i,1,Length[structures]}]structures]
];


ExpandAroundIR[t1_,t2_,order_:1][expr_] := 
Module[{\[Xi]},
	Total@(ReplaceAll[Normal@Series[#,{\[Xi],0,order}],\[Xi] -> Sqrt[sc[Subscript[k, 1],Subscript[k, 1]]]/(2m)]&/@(Coefficient[expr//Expand,GenerateTensorStructures[t1,t2]]/.m->Sqrt[sc[Subscript[k, 1],Subscript[k, 1]]]/(2\[Xi]))GenerateTensorStructures[t1,t2])//Factor//PowerExpand
];


ExpandAroundUV[t1_,t2_,order_:1][expr_] := 
Module[{\[Xi]},
	Total@(ReplaceAll[Normal@Assuming[\[Xi] > 0,Series[#,{\[Xi],0,order}]],\[Xi] -> (2m)/Sqrt[sc[Subscript[k, 1],Subscript[k, 1]]]]&/@(Coefficient[expr//Expand,GenerateTensorStructures[t1,t2]]/.m->Sqrt[sc[Subscript[k, 1],Subscript[k, 1]]]/2 \[Xi])GenerateTensorStructures[t1,t2])//Factor//PowerExpand
];


EmphasizeTensorStructures[list_,modifiers_:{Bold,Blue}][expr_]:=expr/.((#->Style[#,modifiers]&)/@list);


SingularityTest[\[Alpha]_,\[Beta]_] := 
Module[{decider,convergesQ,singularityType,cases,res,singularities},

	SetAttributes[decider,Listable];
	decider[x_]:=x<=0\[And]EvenQ[x];

	(* 
		convergesQ checks if a TripleK integral converges or not 
	*)
	convergesQ[alpha_,beta_List] := alpha>-1+\!\(
\*SubsuperscriptBox[\(\[Sum]\), \(i = 1\), \(3\)]\(Abs[beta[\([i]\)]]\)\);
	
	(* 
		singularityType classifies the type of the singularity based
		on the number of plus signs in the singularity relation.
	 *)
	singularityType[signs_]:=
	Switch[Count[signs,"+"],
		0,
		"Local singularity: ",
		1,
		"Semi-local singularity: ",
		2,
		"Non-local singularity: ",
		3,
		"Non-local singularity: "
	];
	
	(*
		cases is a table with all possible combinations of signs in
		in the singularity relation.
	*)
	cases=Table[\[Alpha]+1+(-1)^i \[Beta][[1]]+(-1)^j \[Beta][[2]]+(-1)^k \[Beta][[3]],{i,0,1},{j,0,1},{k,0,1}];

	(*
		res is the list of all the positions in cases that correspond
		to a singularity.
	*)
	res=Position[decider[cases],True];

	(*
		singularities classify each singularity in res.
	*)
	singularities=singularityType/@(res/.{1->"+",2->"-"});

	If[convergesQ[\[Alpha],\[Beta]],
		"This triple-K integral converges.",
		If[FreeQ[decider[cases],True],
			"Does not converge but can\nbe analytically continued.",
			TableForm[MapThread[#1<>ToString[Subscript["k",#2] == #3,StandardForm]&,{singularities,(res/.{1->"+",2->"-"}),Table[-(1/2)cases[[Sequence@@res[[i]]]],{i,1,Length[res]}]}]]
		]
	]
]


IntegralsToCompute[N_,case_:"Massless",max_:10][numerator_] := 
Module[{integrals,prettyPrint3K},
	integrals = Union[Select[#,Head[#]==ScalarIntegral&]&/@(List@@Integrator[N,numerator])];
	
	Print["Number of integrals to compute: ", Length[integrals]];

	prettyPrint3K = ScalarIntegral[n_,d_,\[Nu]_]:>ToString[Subscript["\[ScriptCapitalI]", d/2-1,Table[d/2-Total[\[Nu]]+\[Nu][[i]],{i,1,3}]],StandardForm];	

	tablePrint[x_] :=
	Module[{len,col},
		len=Length[x];
		col=Quotient[len,max]+If[Mod[len,max]==0,0,1];
		TableForm[
			Map[
				Grid[#,Frame->All,Background->{None,{{LightGray,White}}}]&,
				If[Mod[len,max]==0,
					Table[x[[1+(i-1) max;;i max]],{i,1,col}],
					Table[x[[1+(i-1) max;;i max]],{i,1,col-1}]~Join~{x[[(col-1)max+1;;(col-1)max+Mod[len,max]]]}
				]
			],
		TableDirections->Row,TableAlignments->Top]
	];

	Switch[case,
	"Massless",
	tablePrint[#]&@({integrals/.prettyPrint3K,integrals/.ScalarIntegral[n_,d_,\[Nu]_]:>SingularityTest[d/2-1,Table[d/2-Total[\[Nu]]+\[Nu][[i]],{i,1,3}]]}//Transpose)
	]
];


CrossDiagram[ind1_,ind2_][diagram_]:=
Module[{tmp1,tmp2,subsmon1,subsmon2,subspol1,subspol2},
	subsmon1={Subscript[k, ind1]->tmp1,Subscript[k, ind2]->Subscript[k, ind1]};
	subsmon2={tmp1->Subscript[k, ind2]};
	subspol1={Subscript[n, ind1]->tmp2,Subscript[n, ind2]->Subscript[n, ind1]};
	subspol2={tmp2->Subscript[n, ind2]};
	diagram/.subsmon1/.subsmon2/.subspol1/.subspol2
];


(*
	This function is a shortcut for the operation of collecting tensor structures and
	and applying EmphasizeTensorStructures to aid the visualization.
*)

CollectTensorStructures[t__][expr_]:=Collect[expr,GenerateTensorStructures[t],Simplify]//EmphasizeTensorStructures[GenerateTensorStructures[t]];


(* ::Section::Closed:: *)
(*Projectors*)


TransverseProjector/:TransverseProjector[k_,{\[Mu]_},{\[Nu]_}]sc[k_,{\[Mu]_}]:=0
TransverseProjector/:TransverseProjector[k_,{\[Mu]_},{\[Nu]_}]sc[k_,{\[Nu]_}]:=0
TransverseProjector/:TransverseProjector[k_,{\[Mu]_},{\[Mu]_}]:=SetDimensionOfSpacetime[]-1
TransverseProjector/:TransverseProjector[k_,{\[Mu]_},{\[Nu]_}]sc[{\[Mu]_},{\[Nu]_}]:=SetDimensionOfSpacetime[]-1
TransverseProjector/:TransverseProjector[k_,{\[Mu]_},{\[Nu]_}]TransverseProjector[k_,{\[Nu]_},{\[Rho]_}]:=TransverseProjector[k,{\[Mu]},{\[Rho]}]
TransverseProjector/:\[Delta][TransverseProjector[k_,{\[Mu]_},{\[Nu]_}],sc[k_,{\[Rho]_}]]:=-(sc[k,{\[Mu]}]/sc[k,k])TransverseProjector[k,{\[Nu]},{\[Rho]}]-sc[k,{\[Nu]}]/sc[k,k] TransverseProjector[k,{\[Mu]},{\[Rho]}]

TransverseProjector/:TransverseProjector[k_,{\[Mu]_},_]sc[k_,{\[Mu]_}]:=0
TransverseProjector/:TransverseProjector[k_,_,{\[Nu]_}]sc[k_,{\[Nu]_}]:=0
TransverseProjector/:TransverseProjector[k_,{\[Mu]_},n2_]sc[n1_,{\[Mu]_}]:=TransverseProjector[k,n1,n2]
TransverseProjector/:TransverseProjector[k_,n2_,{\[Mu]_}]sc[n1_,{\[Mu]_}]:=TransverseProjector[k,n1,n2]
TransverseProjector/:TransverseProjector[k_,{\[Mu]_},n2_]sc[{\[Mu]_},{\[Nu]_}]:=TransverseProjector[k,{\[Nu]},n2]
TransverseProjector/:TransverseProjector[k_,n2_,{\[Mu]_}]sc[{\[Mu]_},{\[Nu]_}]:=TransverseProjector[k,{\[Nu]},n2]
TransverseProjector/:TransverseProjector[k_,n1_,{\[Nu]_}]TransverseProjector[k_,{\[Nu]_},n2_]:=TransverseProjector[k,n1,n2]
TransverseProjector/:TransverseProjector[k_,{\[Nu]_},n1_]TransverseProjector[k_,{\[Nu]_},n2_]:=TransverseProjector[k,n1,n2]
TransverseProjector/:TransverseProjector[k_,n1_,{\[Nu]_}]TransverseProjector[k_,n2_,{\[Nu]_}]:=TransverseProjector[k,n1,n2]
TransverseProjector/:TransverseProjector[k_,{\[Nu]_},n1_]TransverseProjector[k_,{\[Nu]_},n2_]:=TransverseProjector[k,n1,n2]

TransverseProjector/:TransverseProjector[k_,n1_,n2_]:=TransverseProjector[k,n2,n1]/;!OrderedQ[{n1,n2}]

TransverseProjector/:\[Delta][TransverseProjector[k_,n1_,n2_],sc[X_,{\[Nu]_}]]:=0/;(X=!=k)\[And](X=!=n1)\[And](X=!=n2)

TransverseProjector/:\[Delta][TransverseProjector[k_,{\[Mu]_},n_],sc[k_,{\[Rho]_}]]:=-(sc[k,{\[Mu]}]/sc[k,k])TransverseProjector[k,n,{\[Rho]}]-sc[k,n]/sc[k,k] TransverseProjector[k,{\[Mu]},{\[Rho]}]

TransverseProjector/:\[Delta][TransverseProjector[k_,n1_,n2_],sc[n1_,{\[Mu]_}]]:=TransverseProjector[k,{\[Mu]},n2]
TransverseProjector/:\[Delta][TransverseProjector[k_,{\[Mu]_},n2_],sc[n2_,{\[Nu]_}]]:=TransverseProjector[k,{\[Mu]},{\[Nu]}]


ExplicitTransverseProjector[expr_]:=expr//.TransverseProjector[k_,arg1_,arg2_]:>sc[arg1,arg2]-(sc[k,arg1]sc[k,arg2])/sc[k,k];


Notation[ParsedBoxWrapper[SubsuperscriptBox["\[DoubledPi]", RowBox[{"\[Mu]_", ",", "\[Nu]_"}], RowBox[{"(", "k_", ")"}]]] \[DoubleLongLeftArrow] ParsedBoxWrapper[RowBox[{"TransverseProjector", "[", RowBox[{"k_", ",", RowBox[{"{", "\[Mu]_", "}"}], ",", RowBox[{"{", "\[Nu]_", "}"}]}], "]"}]]]
Notation[ParsedBoxWrapper[SubscriptBox[RowBox[{"(", RowBox[{"n2_", "\[CenterDot]", SuperscriptBox["\[DoubledPi]", RowBox[{"(", "k_", ")"}]]}], ")"}], "\[Mu]_"]] \[DoubleLongLeftArrow] ParsedBoxWrapper[RowBox[{"TransverseProjector", "[", RowBox[{"k_", ",", RowBox[{"{", "\[Mu]_", "}"}], ",", "n2_"}], "]"}]]]
Notation[ParsedBoxWrapper[SubscriptBox[RowBox[{"(", RowBox[{"n2_", "\[CenterDot]", SuperscriptBox["\[DoubledPi]", RowBox[{"(", "k_", ")"}]]}], ")"}], "\[Mu]_"]] \[DoubleLongLeftArrow] ParsedBoxWrapper[RowBox[{"TransverseProjector", "[", RowBox[{"k_", ",", "n2_", ",", RowBox[{"{", "\[Mu]_", "}"}]}], "]"}]]]
Notation[ParsedBoxWrapper[RowBox[{"(", RowBox[{"n1_", "\[CenterDot]", SuperscriptBox["\[DoubledPi]", RowBox[{"(", "k_", ")"}]], "\[CenterDot]", "n2_"}], ")"}]] \[DoubleLongLeftArrow] ParsedBoxWrapper[RowBox[{"TransverseProjector", "[", RowBox[{"k_", ",", "n1_", ",", "n2_"}], "]"}]]]


GenerateTransverseTensorStructures[spin_]:=
Module[{transverseProjectors,powersEven,powersOdd,evenStructures,oddStructures},
	transverseProjectors={TransverseProjector[Subscript[k, 1],Subscript[n, 1],Subscript[n, 1]],TransverseProjector[Subscript[k, 1],Subscript[n, 1],Subscript[n, 2]],TransverseProjector[Subscript[k, 1],Subscript[n, 2],Subscript[n, 2]]};
	powersEven=GeneralizedFrobeniousSolve[{{2,0},{1,1},{0,2}},{spin,spin}];
	powersOdd=GeneralizedFrobeniousSolve[{{2,0},{1,1},{0,2}},{spin-1,spin-1}];
	evenStructures=Times@@@Table[MapThread[Power[#1,#2]&,{transverseProjectors,powersEven[[i]]}],{i,1,Length[powersEven]}];
	oddStructures=\[Epsilon][Subscript[k, 1],Subscript[n, 1],Subscript[n, 2]]Times@@@Table[MapThread[Power[#1,#2]&,{transverseProjectors,powersOdd[[i]]}],{i,1,Length[powersOdd]}];
	evenStructures~Join~oddStructures
];


GenerateEvenTracelessTransverseTensorStructure[spin_]:=\!\(
\*SubsuperscriptBox[\(\[Sum]\), \(l = 0\), \(Floor[spin/2]\)]\(
\*FractionBox[
SuperscriptBox[\((\(-1\))\), \(l\)], \(
\*SuperscriptBox[\(2\), \(2  l\)] \(l!\)\)] 
\*FractionBox[\(spin!\), \(\((spin - 2  l)\)!\)] 
\*FractionBox[\(\((spin - 
\*FractionBox[\(DIM - 5\), \(2\)] - l)\)!\), \(\((spin - 
\*FractionBox[\(DIM - 5\), \(2\)])\)!\)]\ 
\*SuperscriptBox[\(TransverseProjector[
\*SubscriptBox[\(k\), \(1\)], 
\*SubscriptBox[\(n\), \(1\)], 
\*SubscriptBox[\(n\), \(2\)]]\), \(spin - 2  l\)] 
\*SuperscriptBox[\(TransverseProjector[
\*SubscriptBox[\(k\), \(1\)], 
\*SubscriptBox[\(n\), \(1\)], 
\*SubscriptBox[\(n\), \(1\)]]\), \(l\)] 
\*SuperscriptBox[\(TransverseProjector[
\*SubscriptBox[\(k\), \(1\)], 
\*SubscriptBox[\(n\), \(2\)], 
\*SubscriptBox[\(n\), \(2\)]]\), \(l\)]\)\);


GenerateOddTracelessTransverseTensorStructure::NoOddTerms="The are no parity-odd terms in `1` dimensions.";

GenerateOddTracelessTransverseTensorStructure[spin_]/;If[DIM == 3, True, Message[GenerateOddTracelessTransverseTensorStructure::NoOddTerms,DIM]; False] := 
\[Epsilon][Subscript[k, 1],Subscript[n, 1],Subscript[n, 2]]GenerateEvenTracelessTransverseTensorStructure[spin-1]


(* Error message if WithTransverseProjectors *)
WithTransverseProjectors::NotConservedInput="The expression used is not conserved, therefore it cannot be written only in terms of projectors.";

WithTransverseProjectors[spin_][expr_] := Module[{check, transverseStructures, coefficients, coef, ansatz, toCollect, solutions},
	check = {CheckTransversality[Subscript[k, 1],Subscript[n, 1]][expr],CheckTransversality[Subscript[k, 1],Subscript[n, 2]][expr]};
	
	If[check =!= {0,0},Message[WithTransverseProjectors::NotConservedInput];Abort[]];

	transverseStructures = GenerateTransverseTensorStructures[spin];
	coefficients = Table[Subscript[coef, i],{i,1,Length[transverseStructures]}];
	ansatz = Total[coefficients transverseStructures];
	toCollect = GenerateTensorStructures[spin,spin];
	solutions = Solve[Thread[(#==0&)[Coefficient[Expand[expr-ExplicitTransverseProjector[ansatz]],toCollect]]],coefficients];
	ansatz/.First@solutions
]


(* ::Section:: *)
(*Tensor properties*)


Unprotect[ElementaryTensorCalculus`\[Delta], ElementaryTensorCalculus`ListOfConstants];
\[Delta]/:\[Delta][Subscript[q, a_],sc[Subscript[n, b_],{_}]] := 0;
\[Delta]/:\[Delta][p,sc[Subscript[n, b_],{_}]] := 0;
\[Delta]/:\[Delta][Subscript[k, a_],sc[Subscript[n, b_],{_}]] := 0;
\[Delta]/:\[Delta][Subscript[n, a_],sc[Subscript[n, b_],{_}]] := 0;
ElementaryTensorCalculus`AddConstantToList[m];
Protect[ElementaryTensorCalculus`\[Delta], ElementaryTensorCalculus`ListOfConstants];


End[]


Protect\[NonBreakingSpace]@@\[NonBreakingSpace]Names["DavyFeyn`*"];


EndPackage[]
