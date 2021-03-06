(* ::Package:: *)

BeginPackage["DavyFeyn`",{"ElementaryTensorCalculus`","Notation`"}]


Unprotect\[NonBreakingSpace]@@\[NonBreakingSpace]Names["DavyFeyn`*"];


Begin["`Private`"]


(* 
	Exchange the auxiliary external momenta q by the physical external momenta 
	k by setting Subscript[q, N] = 0. 
*)

ToPhysicalExternalMomenta[N_][expr_] := expr/.(Table[Subscript[q, i] -> Sum[Subscript[k, j],{j,1,i}],{i,1,N-1}]~Join~{Subscript[q, N] -> 0});


(*--------------------*)
(* 2-point functions *)
(*--------------------*)


I2OneMass[dim_,\[Nu]_List] :=
Block[{\[Nu]t=\!\(
\*SubsuperscriptBox[\(\[Sum]\), \(i = 1\), \(2\)]\(\[Nu][\([i]\)]\)\), regdim, signatureDependent},
	regdim = dim + \[CurlyEpsilon];
	signatureDependent = If[(SIGN == 1)\[Or](SIGN == DIM - 1),I^(1-regdim),1];
	Normal[Series[signatureDependent/(4\[Pi])^(regdim/2) (-m^2)^(regdim/2-\[Nu]t) Gamma[\[Nu]t-regdim/2]/Gamma[\[Nu]t] HypergeometricPFQ[{\[Nu]t-regdim/2,\[Nu][[1]],\[Nu][[2]]},{\[Nu]t/2,(\[Nu]t+1)/2},sc[Subscript[q, 1]-Subscript[q, 2],Subscript[q, 1]-Subscript[q, 2]]/(4m^2)],{\[CurlyEpsilon],0,0}]]
]


I2Massless[dim_,\[Nu]_List] :=
Block[{\[Nu]t=Total[\[Nu]], regdim, signatureDependent,q12},
	regdim = dim + 2 \[CurlyEpsilon];
	signatureDependent = If[(SIGN == 1)\[Or](SIGN == DIM - 1),I^(1-regdim),1];
	q12=Subscript[q, 1]-Subscript[q, 2];
	Normal[Series[signatureDependent/(4\[Pi])^(regdim/2) sc[q12,q12]^(regdim/2-\[Nu]t) (Gamma[\[Nu]t-regdim/2]Gamma[regdim/2 - \[Nu][[1]]]Gamma[regdim/2 - \[Nu][[2]]])/(Gamma[\[Nu][[1]]]Gamma[\[Nu][[2]]]Gamma[regdim - \[Nu]t]),{\[CurlyEpsilon],0,0}]]
]


(*--------------------*)
(* 3-point functions *)
(*--------------------*)


I3ZeroNormExternalMomenta[dim_,\[Nu]_List] := 
Block[{\[Nu]t=\!\(
\*SubsuperscriptBox[\(\[Sum]\), \(i = 1\), \(3\)]\(\[Nu][\([i]\)]\)\), regdim, signatureDependent},
	regdim = dim + \[CurlyEpsilon];
	signatureDependent = If[(SIGN == 1)\[Or](SIGN == DIM - 1),I^(1-regdim),1];
	Normal[Series[signatureDependent/(4\[Pi])^(regdim/2) (-m^2)^(regdim/2-\[Nu]t) Gamma[\[Nu]t-regdim/2]/Gamma[\[Nu]t] HypergeometricPFQ[{\[Nu]t-regdim/2,\[Nu][[1]],\[Nu][[2]]},{\[Nu]t/2,(\[Nu]t+1)/2},sc[Subscript[k, 3],Subscript[k, 3]]/(4m^2)],{\[CurlyEpsilon],0,0}]]
]


BesselQuick[\[Nu]_,x_] := Sqrt[\[Pi]/2] E^-x/Sqrt[x] Sum[(Abs[\[Nu]]-1/2+j)!/(j!(Abs[\[Nu]]-1/2-j)!) 1/(2x)^j,{j,0,Floor[Abs[\[Nu]]-1/2]}]


SetRegularizationScheme[alphareg_:1,betareg_:0]:=Block[{},u=alphareg;v=betareg;]
SetRegularizationScheme[];


TripleK[\[Alpha]_,\[Beta]_]/;And@@Table[OddQ[2\[Beta][[i]]],{i,1,3}]:=
Module[{convergesQ,singularitiesQ,integrand,Q,assumptions,x,tmp},
	(* convergesQ checks if a TripleK integral converges or not *)
	convergesQ[alpha_,beta_List] := alpha>-1+\!\(
\*SubsuperscriptBox[\(\[Sum]\), \(i = 1\), \(3\)]\(Abs[beta[\([i]\)]]\)\);
	(* singularitiesQ checks if a TripleK integral has singularities and if it has, return the types. *)
	singularitiesQ[alpha_,beta_List] := 
	Module[{decider,cases,singularities},
		SetAttributes[decider,Listable];
		decider[x_]:=x<=0\[And]EvenQ[x];
		cases=Table[alpha+1+(-1)^i beta[[1]]+(-1)^j beta[[2]]+(-1)^k beta[[3]],{i,0,1},{j,0,1},{k,0,1}];
		singularities=(Count[#,1]&/@Position[decider[cases],True])/.{0->"Local",1->"Semilocal",2->"Nonlocal",3->"Nonlocal"};
		If[FreeQ[decider[cases],True],None,singularities]
	];
	(* Mapping the auxiliary variables Q to the physical external momenta k *)
	Subscript[Q, 1]=Subscript[k, 3];Subscript[Q, 2]=Subscript[k, 1];Subscript[Q, 3]=Subscript[k, 2];

	integrand[a_,b_] := x^a Product[Zeta[2j+1]^b[[j]]BesselQuick[b[[j]],Zeta[2j+1]x],{j,1,3}];

	If[convergesQ[\[Alpha],\[Beta]],
		tmp=Integrate[integrand[\[Alpha],\[Beta]],{x,0,\[Infinity]},GenerateConditions->False]/.Table[Zeta[2i+1]->Subscript[Q, i],{i,1,3}],
		If[(singularitiesQ[\[Alpha],\[Beta]] == None)\[Or]( FreeQ[singularitiesQ[\[Alpha],\[Beta]],"Local"]\[And]FreeQ[singularitiesQ[\[Alpha],\[Beta]],"Semilocal"]),
			tmp=Integrate[integrand[\[Alpha] + Zeta[9],\[Beta] + Zeta[11]],{x,0,\[Infinity]},GenerateConditions->False]/.Table[Zeta[2i+1]->Subscript[Q, i],{i,1,3}]~Join~{Zeta[9] -> u \[CurlyEpsilon], Zeta[11] -> v \[CurlyEpsilon]},
			Abort[];
		]
	];
	tmp
]


EvaluateScalarIntegrals[N_,case_:"Massless"][expr_]:=
Module[{locexpr},
	locexpr = Collect[expr,_ScalarIntegral];
	Switch[N,
		2,
			Switch[
			case,

			"OneMass",
			locexpr/.ScalarIntegral[N,dim_,\[Nu]_List]:>I2OneMass[dim,\[Nu]],

			"Massless",
			locexpr/.ScalarIntegral[N,dim_,\[Nu]_List]:>I2Massless[dim,\[Nu]]
			],
		3,
			Switch[
			case,

			"ZeroNormExternalMomenta",
			locexpr/.\[ScriptCapitalI][a_,b_List]:>I3ZeroNormExternalMomenta[a,b],

			"Massless",
			locexpr/.ScalarIntegral[N,dim_,\[Nu]_List] :>			
					Module[{\[Nu]t=Total[\[Nu]],\[Alpha]=dim/2-1,\[Beta]=Table[dim/2-Total[\[Nu]]+\[Nu][[i]],{i,1,3}],regdim,regnu,signatureDependent},
						regdim = dim + 2 u \[CurlyEpsilon];
						regnu = \[Nu] + (u-v)/2 \[CurlyEpsilon];
						signatureDependent = If[(SIGN == 1)\[Or](SIGN == DIM - 1),I^(1-regdim),1];
						Normal[Series[signatureDependent 2^(-regdim/2+4)/((4\[Pi])^(regdim/2) (\!\(
\*SubsuperscriptBox[\(\[Product]\), \(j = 1\), \(3\)]\(Gamma[regnu[\([j]\)]]\)\))Gamma[regdim-Total[regnu]]) TripleK[\[Alpha],\[Beta]],{\[CurlyEpsilon],0,0}]]
					]			
			]
	]
];


SetRenormalizationScale[label_:Global`\[Mu]] := Module[{}, \[Mu] = label];
SetRenormalizationScale[];


localDivergencies = TripleK[\[Alpha]_,{\[Beta]1_,\[Beta]2_,\[Beta]3_}][p__] :>
Module[{n0, n1, n2, beta = {\[Beta]1,\[Beta]2,\[Beta]3}, mmmSingularity, mmpSingularity},
	n0 = 1/2 (Total[beta]-\[Alpha]-1);
	
	n1 = 1/2 (Total[beta]-2Last[beta]-\[Alpha]-1);

	n2 = 1/2 (2First[beta]-Total[beta]-\[Alpha]-1);

	mmmSingularity[nn_] := Simplify[Total[(\!\(
\*SubsuperscriptBox[\(\[Product]\), \(i = 1\), \(3\)]\(
\*FractionBox[
SuperscriptBox[\((\(-1\))\), \(#[\([i]\)]\)], \(
\*SuperscriptBox[\(2\), \(\(-beta[\([i]\)]\) + v\ \[CurlyEpsilon] + 2  #[\([i]\)] + 1\)] \(#[\([i]\)]!\)\)] Gamma[\(-#[\([i]\)]\) + beta[\([i]\)] + v\ \[CurlyEpsilon]] 
\*SubsuperscriptBox[\(k\), \(i\), \(2  #[\([i]\)]\)]\)\))&/@FrobeniusSolve[{1,1,1},nn]]];

	mmpSingularity[nn_] := Simplify[(Total[(Product[(-1)^#[[i]]/(2^(-beta[[i]]+v \[CurlyEpsilon]+2#[[i]]+1) #[[i]]!) Gamma[-#[[i]]+beta[[i]]+v \[CurlyEpsilon]]



\!\(\*SubsuperscriptBox[\(k\), \(i\), \(2  #\[LeftDoubleBracket]i\[RightDoubleBracket]\)]\),{i,{1,2}}])((-1)^#[[3]]/(2^(beta[[3]]+v \[CurlyEpsilon]+2#[[3]]+1) #[[3]]!) Gamma[-#[[3]]-beta[[3]]-v \[CurlyEpsilon]]



\!\(\*SubsuperscriptBox[\(k\), \(3\), \(2  beta[\([3]\)] + 2  v\ \[CurlyEpsilon] + 2  #\[LeftDoubleBracket]3\[RightDoubleBracket]\)]\))&/@FrobeniusSolve[{1,1,1},nn]]+Total[(Product[(-1)^#[[i]]/(2^(-beta[[i]]+v \[CurlyEpsilon]+2#[[i]]+1) #[[i]]!) Gamma[-#[[i]]+beta[[i]]+v \[CurlyEpsilon]]



\!\(\*SubsuperscriptBox[\(k\), \(i\), \(2  #\[LeftDoubleBracket]i\[RightDoubleBracket]\)]\),{i,{2,3}}])((-1)^#[[1]]/(2^(beta[[1]]+v \[CurlyEpsilon]+2#[[1]]+1) #[[1]]!) Gamma[-#[[1]]-beta[[1]]-v \[CurlyEpsilon]]



\!\(\*SubsuperscriptBox[\(k\), \(1\), \(2  beta\[LeftDoubleBracket]1\[RightDoubleBracket] + 2  v\ \[CurlyEpsilon] + 2  #\[LeftDoubleBracket]1\[RightDoubleBracket]\)]\))&/@FrobeniusSolve[{1,1,1},nn]]+Total[(Product[(-1)^#[[i]]/(2^(-beta[[i]]+v \[CurlyEpsilon]+2#[[i]]+1) #[[i]]!) Gamma[-#[[i]]+beta[[i]]+v \[CurlyEpsilon]]



\!\(\*SubsuperscriptBox[\(k\), \(i\), \(2  #\[LeftDoubleBracket]i\[RightDoubleBracket]\)]\),{i,{1,3}}])((-1)^#[[2]]/(2^(beta[[2]]+v \[CurlyEpsilon]+2#[[2]]+1) #[[2]]!) Gamma[-#[[2]]-beta[[2]]-v \[CurlyEpsilon]]



\!\(\*SubsuperscriptBox[\(k\), \(2\), \(2  beta\[LeftDoubleBracket]2\[RightDoubleBracket] + 2  v\ \[CurlyEpsilon] + 2  #\[LeftDoubleBracket]2\[RightDoubleBracket]\)]\))&/@FrobeniusSolve[{1,1,1},nn]])];

	If[n0<0\[Or]n2>=0,0,
		If[n1<0\[And]IntegerQ[n1],
			mmmSingularity[n0] \[Mu]^(-(u-3v)\[CurlyEpsilon])/((u-3v)\[CurlyEpsilon]),

			mmmSingularity[n0] \[Mu]^(-(u-3v)\[CurlyEpsilon])/((u-3v)\[CurlyEpsilon])+mmpSingularity[n1] \[Mu]^(-(u-v)\[CurlyEpsilon])/((u-v)\[CurlyEpsilon])
		]
	]
];


EvaluateDivergentPart[numerator_] := 
Module[{res},
	res = Collect[Integrator[3,numerator],ScalarIntegral];
	res = res/.ScalarIntegral[3,d_,\[Nu]_List]:>
		Module[{regdim,regnu,signatureDependent},
			regdim = d + 2 u \[CurlyEpsilon];
			regnu = \[Nu] + (u-v)/2 \[CurlyEpsilon];
			signatureDependent = If[(SIGN == 1)\[Or](SIGN == DIM - 1),I^(1-regdim),1];
			signatureDependent 2^(-regdim/2+4)/((4\[Pi])^(regdim/2) (\!\(
\*SubsuperscriptBox[\(\[Product]\), \(j = 1\), \(3\)]\(Gamma[regnu[\([j]\)]]\)\))Gamma[regdim-Total[regnu]])TripleK[d/2-1,Table[d/2-Total[\[Nu]]+\[Nu][[i]],{i,3}]][Subscript[k, 3],Subscript[k, 1],Subscript[k, 2]]
		];
	res = res//ToPhysicalExternalMomenta[3];
	res = res/.TripleK[\[Alpha]_,{\[Beta]1_,\[Beta]2_,\[Beta]3_}][p__] :> (TripleK[\[Alpha],Sort[{\[Beta]1,\[Beta]2,\[Beta]3}]]@@Permute[{p},FindPermutation[{\[Beta]1,\[Beta]2,\[Beta]3},Sort[{\[Beta]1,\[Beta]2,\[Beta]3}]]]);
	res = res/.TripleK[\[Alpha]_,{\[Beta]1_,\[Beta]2_,\[Beta]3_}][p__]/;\[Beta]1<0 :> First[{p}]^(-2 \[Beta]1) TripleK[\[Alpha],{-\[Beta]1,\[Beta]2,\[Beta]3}][p];
	(*res = res/.TripleK[\[Alpha]_,\[Beta]_][p__] :> (TripleK[\[Alpha],Sort[\[Beta]]]@@Permute[{p},FindPermutation[\[Beta],Sort[\[Beta]]]]);
	res = res/.TripleK[\[Alpha]_,\[Beta]_][p__]/;First[\[Beta]]<0 :> First[{p}]^(-2First[\[Beta]]) TripleK[\[Alpha],{-1,1,1}\[Beta]][p];*)
	res = Collect[res,TripleK];
	res/.localDivergencies
];


WithTripleKRepresentation[numerator_] :=
Module[{res},
	res = Collect[Integrator[3,numerator],ScalarIntegral];
	res = res/.ScalarIntegral[3,d_,\[Nu]_List]:>
		Module[{regdim,regnu,signatureDependent},
			regdim = d + 2 u \[CurlyEpsilon];
			regnu = \[Nu] + (u-v)/2 \[CurlyEpsilon];
			signatureDependent = If[(SIGN == 1)\[Or](SIGN == DIM - 1),I^(1-regdim),1];
			signatureDependent 2^(-regdim/2+4)/((4\[Pi])^(regdim/2) (\!\(
\*SubsuperscriptBox[\(\[Product]\), \(j = 1\), \(3\)]\(Gamma[regnu[\([j]\)]]\)\))Gamma[regdim-Total[regnu]])TripleK[d/2-1,Table[d/2-Total[\[Nu]]+\[Nu][[i]],{i,3}]][Subscript[k, 3],Subscript[k, 1],Subscript[k, 2]]
		];
	res = res//ToPhysicalExternalMomenta[3];
	res = res/.TripleK[\[Alpha]_,{\[Beta]1_,\[Beta]2_,\[Beta]3_}][p__] :> (TripleK[\[Alpha],Sort[{\[Beta]1,\[Beta]2,\[Beta]3}]]@@Permute[{p},FindPermutation[{\[Beta]1,\[Beta]2,\[Beta]3},Sort[{\[Beta]1,\[Beta]2,\[Beta]3}]]]);
	res = res/.TripleK[\[Alpha]_,{\[Beta]1_,\[Beta]2_,\[Beta]3_}][p__]/;\[Beta]1<0 :> First[{p}]^(-2 \[Beta]1) TripleK[\[Alpha],{-\[Beta]1,\[Beta]2,\[Beta]3}][p];
	(*res = res/.TripleK[\[Alpha]_,\[Beta]_][p__] :> (TripleK[\[Alpha],Sort[\[Beta]]]@@Permute[{p},FindPermutation[\[Beta],Sort[\[Beta]]]]);
	res = res/.TripleK[\[Alpha]_,\[Beta]_][p__]/;First[\[Beta]]<0 :> First[{p}]^(-2First[\[Beta]]) TripleK[\[Alpha],{-1,1,1}\[Beta]][p];*)
	res = Collect[res,TripleK]
];


End[]


Protect\[NonBreakingSpace]@@\[NonBreakingSpace]Names["DavyFeyn`*"];


EndPackage[]
