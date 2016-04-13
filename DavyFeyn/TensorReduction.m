(* ::Package:: *)

BeginPackage["DavyFeyn`",{"ElementaryTensorCalculus`","Notation`"}]


Unprotect\[NonBreakingSpace]@@\[NonBreakingSpace]Names["DavyFeyn`*"];


Begin["`Private`"]


(***************** 
	indices \[Rule] {{j1},...,{jM}}, multiplicity \[Rule] {\[Lambda],\[Kappa]1,...,\[Kappa]N}, where 2\[Lambda]+\[Kappa]1+...+\[Kappa]N = M
******************)

TensorStructure[indices_,multiplicity_]:=Module[{terms=List[],indloc=indices,loc=multiplicity},
(* construct the metric factors present in the tensor structure *)
While[
	loc[[1]]!=0,
	terms=terms~Join~{sc@@Take[indloc,2]};
	indloc=Drop[indloc,2];
	loc[[1]]--
];

(* construct the q's present in the tensor structure *)
Do[
	While[
		loc[[i+1]]!=0,
		terms=terms~Join~{sc[Subscript[q, i],#]&@@Take[indloc,1]};
		indloc=Drop[indloc,1];
		loc[[i+1]]--
	],
{i,1,Length[loc]-1}];

(* put everything together *)
Times@@terms
]


(*****************
	Takes multiplicity \[Rule] {\[Lambda],\[Kappa]1,...,\[Kappa]N} and returns a symmetric tensor structure with \[Lambda] factors of the 
	metric, \[Kappa]1 factors of q1, \[Kappa]2 factors of q2 and so on.
******************)

SymmetricTensorStructure[multiplicity_List]:=Module[{M,indices,terms=List[],loc=multiplicity,normalization},
(* rank of the tensor structure *)
M=2loc[[1]]+Sum[loc[[i]],{i,2,Length[loc]}];

(* list of indices *)
indices=Table[{Subscript[j, i]},{i,1,M}];

(* factors needed to correct the derivatives with respect to n *)
normalization=If[loc[[1]]!=0,2^loc[[1]] Product[loc[[i]]!,{i,Length[loc]}],Product[loc[[i]]!,{i,2,Length[loc]}]];

(* 
	i.   use TensorStructure to construct the tensor structure desired; 
	ii.  contract all indices with a polarization vector n;
	iii. derive M times with respect to n in order to produce a symmetric tensor structure;
	iv.  normalize correctly the terms.
*)
Expand[1/normalization Fold[\[Delta][#1,#2]&,Expand[Times@@(sc[n,#]&/@indices)TensorStructure[indices,loc]],(sc[n,#]&/@indices)]]
]


(*****************
	if M = 0, TensorReduction returns a single ScalarIntegral
******************)
TensorReduction[N_,0]:=ScalarIntegral[N,DIM,Table[1,{j,1,N}]];

(*****************
	if M \[NotEqual] 0, TensorReduction implements Davydychev's formula (Phys. Lett., B263:107\[Dash]111, 1991).
******************)
TensorReduction[N_,M_]:=Module[{sum},
sum=FrobeniusSolve[{2}~Join~ConstantArray[1,N],M];

(* 
	the factor of (-1)^(Mod[SIGN,DIM]+1) was taylored to work in the Euclidean and Lorentzian cases. 
*)

\!\(
\*SubsuperscriptBox[\(\[Sum]\), \(i = 1\), \(Length[sum]\)]\(
\*SuperscriptBox[\((\(-
\*FractionBox[\(1\), \(2\)]\))\), \(sum[\([i, 1]\)]\)] 
\*SuperscriptBox[\((
\*FractionBox[
SuperscriptBox[\((\(-1\))\), \(Mod[SIGN, DIM] + 1\)], \(4  \[Pi]\)])\), \(sum[\([i, 1]\)] - M\)] SymmetricTensorStructure[sum[\([i]\)]] Product[Pochhammer[1, sum[\([i, 1 + j]\)]], {j, 1, N}] ScalarIntegral[N, DIM + 2 \((M - sum[\([i, 1]\)])\), Table[1 + sum[\([i, 1 + j]\)], {j, 1, N}]]\)\)//Expand
]


(*****************
	ToContract takes a piece of a tensor structure x that contains p's and strips off them.
******************)

ToContract[_,1]=1;

ToContract[N_,x_]:=Module[{list,max,indloc,constructor,epsilon,terms=List[],pq,pn},
(*
	is there a Levi-Civita in the expression?
*)
epsilon=If[MatchQ[x,\[Epsilon][__]],x,If[Not@NumericQ[Select[x,(!FreeQ[#,\[Epsilon][__]]\[And]!FreeQ[#,p])&]],Select[x,!FreeQ[#,\[Epsilon][__]]\[And]!FreeQ[#,p]&],\[Epsilon][]]];

(*
	list of possible contractions of p's and q's
*)
pq=Table[sc[p,Subscript[q, i]],{i,1,N}];

(*
	list of possible contractions of p's and n's
*)
pn =Table[sc[p,Subscript[n, i]],{i,1,N}];

(*
	list of the number of times that each possible term appears in the tensor structure x.
	1   until  N : pq's
	N+1 until 2N : pn's
	        2N+1 : pp
	        2N+2 : epsilon
*)
list=Exponent[x,pq~Join~pn~Join~{sc[p,p],epsilon}];

(*
	number of p's in the tensor structure x.
*)
max=Plus@@((ConstantArray[1,2N]~Join~{2,1})list);

(*
	list of indices.
*)
indloc=Table[{Subscript[j, i]},{i,1,max}];

(*
	strip off the q's from pq's
*)
Do[
	While[
		list[[i]]!=0,
		terms=terms~Join~{sc[Subscript[q, i],#]&@@Take[indloc,1]};
		indloc=Drop[indloc,1];
		list[[i]]--
	],
{i,1,N}];

(*
	strip off the n's from pn's
*)
Do[
	While[
		list[[i+N]]!=0,
		terms=terms~Join~{sc[Subscript[n, i],#]&@@Take[indloc,1]};
		indloc=Drop[indloc,1];
		list[[i+N]]--
	],
{i,1,N}];

(*
	strip off metric factors from pp's
*)
While[
	list[[2N+1]]!=0,
	terms=terms~Join~{sc[#1,#2]&@@Take[indloc,2]};
	indloc=Drop[indloc,2];
	list[[2N+1]]--
];

(*
	strip off a Levi-Civita from epsilon
*)
While[
	list[[2N+2]]!=0,
	terms=terms~Join~{epsilon//.p:>Take[indloc,1][[1,1]]};
	list[[2N+2]]--
];

(*
	put everything together.
*)
Times@@terms
]


(*****************
	Integrator will that a numerator f and express it in terms of scalar integrals using TensorReduction.
******************)

Integrator[N_,f_]:=Module[{g=0,aux1,aux2,aux3,myf,withp,withoutp,pq,pn,epsilon,tensorStructure},
(*
	local version of f.
	i.  aux1 helps deal with the case in which f is a monomial;
	ii. aux2 helps deal with the case in which f is atomic.
*)
myf=Expand[aux2 aux3 f]+aux1 m;

(*
	function to select terms that contain p. 
*)
withp[x_]:=Select[x,!FreeQ[#,p]&];

(*
	function to select terms that do not contain p. 
*)
withoutp[x_]:=If[MatchQ[Select[x,FreeQ[#,p]&],sc[]],1,Select[x,FreeQ[#,p]&]];

(*
	list of pq's.
*)
pq=Table[sc[p,Subscript[q, i]],{i,1,N}];

(*
	list of pn's.
*)
pn=Table[sc[p,Subscript[n, i]],{i,1,N}];

(*
	is there a Levi-Civita in the expression?
*)
epsilon[x_]:=If[MatchQ[x,\[Epsilon][__]],x,If[Not@NumericQ[Select[x,(!FreeQ[#,\[Epsilon][__]]\[And]!FreeQ[#,p])&]],Select[x,!FreeQ[#,\[Epsilon][__]]\[And]!FreeQ[#,p]&],\[Epsilon][]]];

(*
	returns a list with the number of p's associated to each tensor structure present in a monomial.
*)
tensorStructure[x_]:=(ConstantArray[1,2N]~Join~{2,1})Exponent[withp[x],pq~Join~pn~Join~{sc[p,p],epsilon[x]}];

(*
	For every monomial in myf:
		i.   strips off the terms without p's;
		ii.  apply ToContract to the terms with p's;
		iii. apply TensorReduction using the appropriate tensoriality given by the number of p's.
*)
Do[g=g+Expand[withoutp[myf[[i]]]ToContract[N,withp[myf[[i]]]]TensorReduction[N,Total@tensorStructure[myf[[i]]]]],{i,1,Length[myf]}];

(*
	take of the auxiliary variables and return the answer.
*)
g/.{aux1->0,aux2->1,aux3->1}
]


End[]


Protect\[NonBreakingSpace]@@\[NonBreakingSpace]Names["DavyFeyn`*"];


EndPackage[]
