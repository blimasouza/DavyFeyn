(* ::Package:: *)

BeginPackage["DavyFeyn`"]

(*--------------------------------------------*)
(* Functions to perform the tensor reduction *)
(*--------------------------------------------*)

If[!ValueQ[TensorStructure::usage],
TensorStructure::usage="TensorStructure[\!\(\*
StyleBox[\"indices\",\nFontSlant->\"Italic\"]\), \!\(\*
StyleBox[\"multiplicity\",\nFontSlant->\"Italic\"]\)] takes a set of indices \
{{\!\(\*SubscriptBox[\(j\), \(1\)]\)}\[Ellipsis],{\!\(\*SubscriptBox[\(j\), \(M\)]\)}} and a list of multiplicities {\[Lambda],\!\(\*SubscriptBox[\(\[Kappa]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Kappa]\), \(N\)]\)} such that 2\[Lambda]+\!\(\*SubscriptBox[\(\[Kappa]\), \(1\)]\)+\[Ellipsis]+\!\(\*SubscriptBox[\(\[Kappa]\), \(N\)]\) = M and \
constructs a tensor structure with \[Eta] figuring \[Lambda] times, \!\(\*SubscriptBox[\(q\), \(1\)]\), \!\(\*SubscriptBox[\(\[Kappa]\), \(1\)]\) times, \!\(\*SubscriptBox[\(q\), \(2\)]\), \!\(\*SubscriptBox[\(\[Kappa]\), \(2\)]\) times and \
so on. This function is an essential piece of the function SymmetricTensorStructure."
];

If[!ValueQ[SymmetricTensorStructure::usage],
SymmetricTensorStructure::usage="SymmetricTensorStructure[multiplicity] takes a list \
of multiplicities {\[Lambda],\!\(\*SubscriptBox[\(\[Kappa]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Kappa]\), \(N\)]\)} and constructs a completly symmetric tensor structure \
with \[Eta] figuring \[Lambda] times, \!\(\*SubscriptBox[\(q\), \(1\)]\), \!\(\*SubscriptBox[\(\[Kappa]\), \(1\)]\) times, \!\(\*SubscriptBox[\(q\), \(2\)]\), \!\(\*SubscriptBox[\(\[Kappa]\), \(2\)]\) times and so on."
];

If[!ValueQ[TensorReduction::usage],
TensorReduction::usage="TensorRection[N,M] takes two numbers N and M and give the \
tensor reduction of a Feynman integral with M momenta in the numerator and N propagators \
with power 1 in terms of scalar integrals using Davydychev's formula \
(see Phys. Lett. B263 (1991) 107\[Dash]111)."
];

If[!ValueQ[ToContract::usage],
ToContract::usage="ToContract[N, x] takes a number N that represents the number of \
propagators in the corresponding Feynman integral and a tensor structure x that ONLY \
contains terms of the form p\[CenterDot]\!\(\*SubscriptBox[\(q\), \(i\)]\), p\[CenterDot]\!\(\*SubscriptBox[\(n\), \(i\)]\), p\[CenterDot]p or \!\(\*SuperscriptBox[\(p\), \(\[Mu]\)]\)\[Epsilon][\[Ellipsis],\[Mu],\[Ellipsis]] and strip of the \!\(\*SubscriptBox[\(q\), \(i\)]\)'s and \
\!\(\*SubscriptBox[\(n\), \(i\)]\)'s in order to construct the object that will be contracted with the answer of \
TensorReduction."
];

If[!ValueQ[Integrator::usage],
Integrator::usage="Integrator[N,numerator] takes a numerator of a Feynman integral \
with three propagators with power one and gives back the result of the Feynman integral \
in terms of scalar integrals in various dimensions and with various powers in each of \
the three propagators."
];

If[!ValueQ[ScalarIntegral::usage],
ScalarIntegral::usage="ScalarIntegral[N, DimensionOfSpacetime,{\!\(\*SubscriptBox[\(\[Nu]\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(\[Nu]\), \(N\)]\)}] denotes a \
Feynman integral with no tensorial structure in the numerator, N propagators each one \
with its power \!\(\*SubscriptBox[\(\[Nu]\), \(i\)]\)."
];

(*--------------------------*)
(* Functions to set labels *)
(*--------------------------*)

If[!ValueQ[SetInternalMomentum::usage],
SetInternalMomentum::usage="SetInternalMomentum[internal] sets the label to be used as \
the integrated momentum in a 1-loop Feynman diagram. By default it is set to be p."
];

If[!ValueQ[ShowInternalMomentum::usage],
ShowInternalMomentum::usage="ShowInternalMomentum[] displays the current label for the \
momentum being integrated."	
];


If[!ValueQ[SetAuxiliaryExternalMomenta::usage],
SetAuxiliaryExternalMomenta::usage="SetAuxiliaryExternalMomenta[external] sets the label \
to be used to denote external momenta in a Feynman diagram. By default it is set to be q. \
As a matter of fact, the q's do not represent the external momenta themselves but the \
difference of adjacent external momenta."
];

If[!ValueQ[ShowAuxiliaryExternalMomenta::usage],
ShowAuxiliaryExternalMomenta::usage="ShowAuxiliaryExternalMomenta[] displays the current \
label for the external momenta."	
];


If[!ValueQ[SetPhysicalExternalMomenta::usage],
SetPhysicalExternalMomenta::usage="SetPhysicalExternalMomenta[k] sets the label k to the \
physical external momenta. By default it is set to k."
];

If[!ValueQ[ShowPhysicalExternalMomenta::usage],
ShowPhysicalExternalMomenta::usage="ShowPhysicalExternalMomenta[] prints the label of the \
physical momentum in use."
];

If[!ValueQ[SetPolarizationVectors::usage],
SetPolarizationVectors::usage="SetPolarizationVectors[polarization] sets the label to be \
used for our polarization vectors, i.e. the vectors that hide the indices of the Feynman \
diagram. By default it is set to be n."
];

If[!ValueQ[ShowPolarizationVectors::usage],
ShowPolarizationVectors::usage="ShowPolarizationVectors[] displays the current label for \
the polarization vectors."
];

If[!ValueQ[SetMassParameter::usage],
SetMassParameter::usage="SetMassParameter[mass] sets the label to be used to denote the \
mass parameter. By default it is set to m."
];

If[!ValueQ[ShowMassParameter::usage],
ShowMassParameter::usage="ShowMassParameter[] displays the current label for the mass \
parameter.";
];

If[!ValueQ[ShowCurrentLabels::usage],
ShowCurrentLabels::usage="ShowCurrentLabels[] displays a table with the current value of \
the dimension of spacetime and the current labels for the internal momentum, the auxiliary \
external momenta, the physical external momenta, the polarization vectors and the mass \
parameter."
];

If[!ValueQ[ToPhysicalExternalMomenta::usage],
ToPhysicalExternalMomenta::usage="ToPhysicalExternalMomenta[N] exchange the auxiliar \
external momenta (denoted by default q) by the physical external momenta in a N-point \
function, assuming that all vertices in the 1-loop diagram connects one external leg to \
two internal one."
];

(*---------------------------------------------*)
(* Functions to evaluate the scalar integrals *)
(*---------------------------------------------*)

If[!ValueQ[EvaluateScalarIntegrals::usage],
EvaluateScalarIntegrals::usage="EvaluateScalarIntegrals[N, case][expr] takes the number \
of points of the correlations function N and an expression expr that contains \
ScalarIntegrals and evaluate them in the case requested. If no case is selected the \
integrals will be computed in the massless case. For 2-point functions, the cases \
available are OneMass and Massless, while for 3-point functions one can select among \
OneMass, Massless and ZeroNormExternalMomenta."
];

If[!ValueQ[FullyEvaluate::usage],
FullyEvaluate::usage="FullyEvaluate[N, limit][expr] take two parameters N, the number of \
propagators in the Feynman diagram and limit, which corresponds to the assumptions that \
we are using regarding the external momenta in order to evaluate the momentum integrals. \
FullyEvaluate[N, limit] acts on expr which is supposed to be the numerator of a Feynman \
integral."
];

If[!ValueQ[CrossDiagram::usage],
CrossDiagram::usage="CrossDiagram[i,j][diagram] takes two indices i and j substitutes \
\!\(\*SubscriptBox[\(k\), \(i\)]\)\[LeftRightArrow]\!\(\*SubscriptBox[\(k\), \(j\)]\) and \!\(\*SubscriptBox[\(n\), \(i\)]\)\[LeftRightArrow]\!\(\*SubscriptBox[\(n\), \(j\)]\) in diagram to produce the cross-diagram."
];

If[!ValueQ[SimplifyExternalMomenta3pts::usage],
SimplifyExternalMomenta3pts::usage="SimplifyExternalMomenta3pts[\!\(\*FormBox[
StyleBox[\"expr\",\nFontSlant->\"Italic\"],
TraditionalForm]\)] exchange the factors \
of \!\(\*FormBox[\(\*SubscriptBox[\(k\), \(i\)]\[CenterDot]\*SubscriptBox[\(k\), \(j\)]\),
TraditionalForm]\) in the 3-point function expr \!\(\*FormBox[\(\*FractionBox[\(1\), \(2\)]\\\ \((\*SubsuperscriptBox[\(k\), \(m\), \(2\)] - \*SubsuperscriptBox[\(k\), \(i\), \(2\)] - \*SubsuperscriptBox[\(k\), \(j\), \(2\)])\)\),
TraditionalForm]\), where m\[NotEqual]i\[NotEqual]j."
];

If[!ValueQ[IntegralsToCompute::usage],
IntegralsToCompute::usage="IntegralsToCompute[N, case, rows][numerator] \
takes the numerator of a Feynman integral, the number of propagators N, \
the case in which the integrals should be evaluated and a number of rows \
and displays all scalar integrals that must be evaluated. The case and the \
number of rows are optional arguments set by default to \"Massless\" and 10, \
respectively."	
];

(*-----------------------------------------------*)
(* Functions to deal with the tensor structures *)
(*-----------------------------------------------*)

If[!ValueQ[EvenSector::usage],
EvenSector::usage="EvenSector[expr] selects the terms in expr that do not contain \
an \[Epsilon]-symbol."
];

If[!ValueQ[OddSector::usage],
OddSector::usage="OddSector[expr] selects only the terms in expr that contain an \
\[Epsilon]-symbol."
];

If[!ValueQ[GeneralizedFrobeniousSolve::usage],
GeneralizedFrobeniousSolve::usage="\
GeneralizedFrobeniousSolve[{{\!\(\*SubscriptBox[\(a\), \(1, 1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(a\), \(N, 1\)]\)},\[Ellipsis],{\!\(\*SubscriptBox[\(a\), \(1, M\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(a\), \(N, M\)]\)}},{\!\(\*SubscriptBox[\(m\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(m\), \(N\)]\)}] generalizes the \
function FrobeniousSolve to N-dimensional vectors."
];

If[!ValueQ[GenerateEvenTensorStructures::usage],
GenerateEvenTensorStructures::usage="GenerateEvenTensorStructures[\!\(\*SubscriptBox[\(t\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(t\), \(N\)]\)] generates \
all the possible even tensorial structures that can be present in a N-point function of \
tensors of rank \!\(\*SubscriptBox[\(t\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(t\), \(N\)]\).";	
];

If[!ValueQ[GenerateOddTensorStructures::usage],
GenerateOddTensorStructures::usage="GenerateOddTensorStructures[\!\(\*SubscriptBox[\(t\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(t\), \(N\)]\)] generates all \
the possible odd tensorial structures that can be present in a N-point function of \
tensors of rank \!\(\*SubscriptBox[\(t\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(t\), \(N\)]\)."	
];


If[!ValueQ[GenerateTensorStructures::usage],
GenerateTensorStructures::usage="GenerateTensorStructures[\!\(\*SubscriptBox[\(t\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(t\), \(N\)]\)] generates all the \
possible tensorial structures that can be present in a N-point function of tensors of \
rank \!\(\*SubscriptBox[\(t\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(t\), \(N\)]\)."	
];

If[!ValueQ[CheckTransversality::usage],
CheckTransversality::usage="CheckTransversality[k,n][expr] exchange a polarization vector \
n by a momentum k. This function is useful to verify Ward identities."	
];

If[!ValueQ[CheckTracelessness::usage],
CheckTracelessness::usage="CheckTracelessness[n][expr] extract two polarization vectors \
with the same name from expression and contract their indices. In other words, it takes \
a trace of operator associated to the polarization vector n."	
];

If[!ValueQ[GeneralEvenContactTerm::usage],
GeneralEvenContactTerm::usage="GeneralEvenContactTerm[\!\(\*SubscriptBox[\(t\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(t\), \(N\)]\)] creates the most general \
linear combination of the even tensor structures present in a correlation function of N \
operators each being a tensor of rank \!\(\*SubscriptBox[\(t\), \(i\)]\). The label of the constants is an optional \
argument which is set by default to be \"a\". To change it to another one: \
GeneralEvenContactTerm[\!\(\*SubscriptBox[\(t\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(t\), \(N\)]\),{new_label}]."
];

If[!ValueQ[GeneralOddContactTerm::usage],
GeneralOddContactTerm::usage="GeneralOddContactTerm[\!\(\*SubscriptBox[\(t\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(t\), \(N\)]\)] creates the most general \
linear combination of the odd tensor structures present in a correlation function of N \
operators each being a tensor of rank \!\(\*SubscriptBox[\(t\), \(i\)]\). The label of the constants is an optional \
argument which is set by default to be \"a\". To change it to another one: \
GeneralOddContactTerm[\!\(\*SubscriptBox[\(t\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(t\), \(N\)]\),{new_label}]."
];

If[!ValueQ[EmphasizeTensorStructures::usage],
EmphasizeTensorStructures::usage="EmphasizeTensorStructures[terms,style][expr] takes a \
list of terms and an optinal set of styles and rewrite expr applying the desired styles \
to the list of terms. By default, style = {Bold,Blue}."
];

If[!ValueQ[CollectTensorStructures::usage],
CollectTensorStructures::usage="CollectTensorStructures[\!\(\*SubscriptBox[\(t\), \(1\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(t\), \(N\)]\)][expr] is a shortcut for \
the operation of collecting tensor structures in a given correlation function expr of \
operators of spin \!\(\*SubscriptBox[\(t\), \(1\)]\),\!\(\*SubscriptBox[\(t\), \(2\)]\),\[Ellipsis],\!\(\*SubscriptBox[\(t\), \(N\)]\)."
];

(*-----------------------------------------*)
(* Transverse-traceless tensor structures *)
(*-----------------------------------------*)

If[!ValueQ[TransverseProjector::usage],
TransverseProjector::usage="TransverseProjector[k,\!\(\*SubscriptBox[\(n\), \(1\)]\),\!\(\*SubscriptBox[\(n\), \(2\)]\)] implements the projector over \
the hyperplane transverse to the vector k. \!\(\*SubscriptBox[\(n\), \(1\)]\) and \!\(\*SubscriptBox[\(n\), \(2\)]\) denote the polarization vectors \
contracted to the indices of the projector. TransverseProjector[k,{\[Mu]},{\[Nu]}] represents \
the projector without the contraction with polatization vectors."
];

If[!ValueQ[ExplicitTransverseProjector::usage],
ExplicitTransverseProjector::usage="ExplicitTransverseProjector[expr] applies a \
substitution rule on expr which exchanges all projectors by their explicit expression."
];

If[!ValueQ[GenerateTransverseTensorStructures::usage],
GenerateTransverseTensorStructures::usage="GenerateTransverseTensorStructures[s] \
generates all transverse tensor structures that may be present in a 2-point function \
of tensor of rank s."
];

If[!ValueQ[GenerateEvenTracelessTransverseTensorStructure::usage],
GenerateEvenTracelessTransverseTensorStructure::usage="\
GenerateEvenTracelessTransverseTensorStructure[spin] generate the even traceless and \
transverse tensor structure present in a 2-point function of traceless and conserved \
spin s current."
];

If[!ValueQ[GenerateOddTracelessTransverseTensorStructure::usage],
GenerateOddTracelessTransverseTensorStructure::usage="\
GenerateOddTracelessTransverseTensorStructure[spin] generate the odd traceless and \
transverse tensor structure present in a 2-point function of traceless and conserved \
spin s current"
];

If[!ValueQ[WithTransverseProjectors::usage],
WithTransverseProjectors::usage="WithTransverseProjectors[spin][expr] takes expr which \
is expected to be a 2-point function of conserved currents and write the tensor \
structures in terms of projector."
];

(*--------------------------*)
(* Expand around UV and IR *)
(*--------------------------*)

If[!ValueQ[ExpandAroundIR::usage],
ExpandAroundIR::usage="ExpandAroundIR[\!\(\*SubscriptBox[\(t\), \(1\)]\),\!\(\*SubscriptBox[\(t\), \(2\)]\),order][expr] expands a 2-point function of \
operators of spin \!\(\*SubscriptBox[\(t\), \(1\)]\) and \!\(\*SubscriptBox[\(t\), \(2\)]\) around the IR (i.e. k/m ~ 0) until the specified order. \
Be aware that this function uses PowerExpand."
];

If[!ValueQ[ExpandAroundUV::usage],
ExpandAroundUV::usage="ExpandAroundUV[\!\(\*SubscriptBox[\(t\), \(1\)]\),\!\(\*SubscriptBox[\(t\), \(2\)]\),order][expr] expands a 2-point function of \
operators of spin \!\(\*SubscriptBox[\(t\), \(1\)]\) and \!\(\*SubscriptBox[\(t\), \(2\)]\) around the UV (i.e. m/k~0) until the specified order. \
Be aware that this function uses PowerExpand and assumes that the parameter m/k is \
positive."
];

(*--------------------------------------------*)
(* Functions to deal with triple-K integrals *)
(*--------------------------------------------*)

If[!ValueQ[SetRegulatorsTripleK::usage],
SetRegulatorsTripleK::usage="SetRegulatorsTripleK[\[CurlyEpsilon],u,v] defines the labels for the \
regulators needed to regulate a triple-K integral, namely \!\(\*SubscriptBox[\(\[ScriptCapitalI]\), \(\[Alpha] + u\[CurlyEpsilon], {\*SubscriptBox[\(\[Beta]\), \(1\)] + v\[CurlyEpsilon], \*SubscriptBox[\(\[Beta]\), \(2\)] + v\[CurlyEpsilon], \*SubscriptBox[\(\[Beta]\), \(3\)] + v\[CurlyEpsilon]}\)]\)."
];

If[!ValueQ[SetRegularizationScheme::usage],
SetRegularizationScheme::usage="SetRegularizationScheme[u,v] defines which values we \
want to use for the regulators u and v. By default, we use u=1 and v=0."
];

If[!ValueQ[BesselQuick::usage],
BesselQuick::usage="BesselQuick[\[Nu],x] computes the modified bessel function of second \
kind \!\(\*SubscriptBox[\(K\), \(\[Nu]\)]\)(x) for \[Nu]=\!\(\*FractionBox[\(1\), \(2\)]\)+n, n\[Element]\[DoubleStruckCapitalZ]."
];

If[!ValueQ[TripleK::usage],
TripleK::usage="TripleK[\[Alpha],{\!\(\*SubscriptBox[\(\[Beta]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Beta]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Beta]\), \(3\)]\)}] implements the triple-K integrals as presented in \
arXiv:1511.02357, for instance.";
];

If[!ValueQ[SingularityTest::usage],
SingularityTest::usage="SingularityTest[\[Alpha],{\!\(\*SubscriptBox[\(\[Beta]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Beta]\), \(2\)]\),\!\(\*SubscriptBox[\(\[Beta]\), \(3\)]\)}] checks whether the triple-K integral \
\!\(\*SubscriptBox[\(\[ScriptCapitalI]\), \(\[Alpha], {\*SubscriptBox[\(\[Beta]\), \(1\)], \*SubscriptBox[\(\[Beta]\), \(2\)], \*SubscriptBox[\(\[Beta]\), \(3\)]}\)]\) converges. In case it doesn't, it tells if it is possible to define the integral \
via analytic continuation or if we have singularities. We have singularities when \
\[Alpha]+1\[PlusMinus]\!\(\*SubscriptBox[\(\[Beta]\), \(1\)]\)\[PlusMinus]\!\(\*SubscriptBox[\(\[Beta]\), \(2\)]\)\[PlusMinus]\!\(\*SubscriptBox[\(\[Beta]\), \(3\)]\)=-2k, k\[Element]{0,1,2,...}. Each singularity is classified by the signs that generate \
it and by its value of k."
];

EndPackage[]
