(* ::Package:: *)

BeginPackage["CurvatureTensors`"];


(* ::Section:: *)
(*Usage *)


Conventions::usage = "Conventions[] gives a table of the chosen indices conventions of symbols";

CurvatureTensors::usage ="CurvatureTensors[metric, coordinates], receives a metric (as a square matrix) and a list of coordinates and generates the associated curvature tensors and functions:

Global`metric
Global`dimension
Global`metricDual 
Global`sqrtDet 
Global`Christoffel
Global`Riemann 
Global`Ricci 
Global`RicciUpper
Global`RicciScalar 
Global`KretschmannScalar 
Global`EinsteinTensor =
Global`EinsteinTensorPlus\[CapitalLambda] 
Global`metricComponents 
Global`metricDualComponents 
Global`ChristoffelComponents 
Global`RiemannComponents 
Global`RicciComponents
Global`EinsteinComponents 

PrettyChristoffel[] 
PrettyRiemann[]
PrettyRicci[]
PrettyEinstein[]

covariantDerivative1[] 
covariantDerivative2[] 
covariantDerivative3[]
Contract[]
Raise[]
Lower[]

It accepts two options. If the spacetime is not Lorentzian, please set 'Riemannian'->True. Given the ambiguity in the definition of the Riemann tensor, you can also toggle the convention chosen by setting 'Toggle'->True";

Contract::usage ="Contract[Tensor1, Tensor2, listIndices] receives Tensor1 and Tensor2 and a list of pair of indices {{\!\(\*SubscriptBox[\(i\), SubscriptBox[\(j\), \(1\)]]\), \!\(\*SubscriptBox[\(i\), SubscriptBox[\(j\), \(2\)]]\)},...} to contract and returns the contraction.";
Global`\[CapitalLambda]::usage = "cosmological constant."
Global`metric::usage =" metric tensor, metric[[\[Mu],\[Nu]]] = \!\(\*SubscriptBox[\(g\), \(\[Mu]\[Nu]\)]\).";
Global`dimension::usage =" dimension of the spacetime, dimension = Length[metric].";
Global`metricDual::usage =" dual metric tensor, metricDual[[\[Mu],\[Nu]]] = \!\(\*SuperscriptBox[\(g\), \(\[Mu]\[Nu]\)]\).";
Global`sqrtDet::usage ="\!\(\*SqrtBox[\(\(|\)\(Det[g]\)\(|\)\)]\)";
Global`Christoffel::usage ="Christofell symbols, Christofell[[\[Mu],\[Nu],\[Kappa]]] =  \!\(\*SuperscriptBox[SubscriptBox[\(\[CapitalGamma]\), \(\[Mu]\[Nu]\)], \(\[Kappa]\)]\) ";
Global`Riemann::usage ="Riemann tensor, Riemann[[\[Alpha],\[Beta],\[Mu],\[Kappa]]] = \!\(\*SuperscriptBox[SubscriptBox[\(R\), \(\[Alpha]\[Beta]\[Mu]\)], \(\[Kappa]\)]\)";
Global`Ricci::usage ="Ricci tensor, Ricci[[\[Alpha],\[Beta]]] = \!\(\*SubscriptBox[\(R\), \(\[Alpha]\[Beta]\)]\)";
Global`RicciUpper::usage ="Ricci tensor with raised components, RicciUpper[[\[Alpha],\[Beta]]] = \!\(\*SuperscriptBox[\(R\), \(\[Alpha]\[Beta]\)]\)";
Global`RicciScalar::usage ="Ricci scalar, RicciScalar = \!\(\*SuperscriptBox[\(g\), \(\[Alpha]\[Beta]\)]\)\!\(\*SubscriptBox[\(R\), \(\[Alpha]\[Beta]\)]\)";
Global`KretschmannScalar::usage = "Kretschmann scalar, KretschmannScalar =  \!\(\*SuperscriptBox[\(R\), \(\[Alpha]\[Beta]\[Mu]\[Kappa]\)]\)\!\(\*SubscriptBox[\(R\), \(\[Alpha]\[Beta]\[Mu]\[Kappa]\)]\)";
Global`EinsteinTensor::usage ="Einstein tensor, EinsteinTensor[[\[Mu],\[Nu]]] = \!\(\*SubscriptBox[\(R\), \(\[Mu]\[Nu]\)]\) - \!\(\*FractionBox[\(1\), \(2\)]\) RicciScalar \!\(\*SubscriptBox[\(g\), \(\[Mu]\[Nu]\)]\)";
Global`EinsteinTensorPlus\[CapitalLambda]::usage ="Einstein tensor plus cosmological constant, EinsteinTensorPlus\[CapitalLambda][[\[Mu],\[Nu]]] = \!\(\*SubscriptBox[\(R\), \(\[Mu]\[Nu]\)]\) - \!\(\*FractionBox[\(1\), \(2\)]\) RicciScalar \!\(\*SubscriptBox[\(g\), \(\[Mu]\[Nu]\)]\) + \[CapitalLambda] \!\(\*SubscriptBox[\(g\), \(\[Mu]\[Nu]\)]\)";
Global`metricComponents::usage ="Table with non-vanishing components of metric for better visualization.";
Global`metricDualComponents::usage ="Table with non-vanishing components of metricDual for better visualization.";
Global`ChristoffelComponents::usage ="Table with non-vanishing components of Christoffel for better visualization.";
Global`RiemannComponents::usage ="Table with non-vanishing components of Riemann for better visualization.";
Global`RicciComponents::usage ="Table with non-vanishing components of Ricci for better visualization.";
Global`EinsteinComponents::usage ="Table with non-vanishing components of Einstein for better visualization.";

PrettyChristoffel::usage ="PrettyChristoffel[] returns a multicolumn with non-vanishing components of Christoffel for better visualization. It accepts the options 
'columns'->n, which sets the number of columns to n; 'backgroundColors'->{color1, color2}, which sets the background colors to color1 and color2; and 'framed'->True, which adds a frame to the multicolumn.";
PrettyRiemann::usage ="PrettyRiemann[] returns a multicolumn with non-vanishing components of Riemann for better visualization. It accepts the options 
'columns'->n, which sets the number of columns to n; 'backgroundColors'->{color1, color2}, which sets the background colors to color1 and color2; and 'framed'->True, which adds a frame to the multicolumn.";
PrettyRicci::usage ="PrettyRicci[] returns a multicolumn with non-vanishing components of Ricci for better visualization. It accepts the options 
'columns'->n, which sets the number of columns to n; 'backgroundColors'->{color1, color2}, which sets the background colors to color1 and color2; and 'framed'->True, which adds a frame to the multicolumn.";
PrettyEinstein::usage ="PrettyEinstein[] returns a multicolumn with non-vanishing components of Einstein for better visualization. It accepts the options 
'columns'->n, which sets the number of columns to n; 'backgroundColors'->{color1, color2}, which sets the background colors to color1 and color2; and 'framed'->True, which adds a frame to the multicolumn.";

covariantDerivative1::usage ="covariantDerivative1[V,\[Mu],\[Alpha]] receives the list V and the indices \[Mu] and \[Alpha] and return \!\(\*SubscriptBox[\(\[Del]\), \(\[Alpha]\)]\)\!\(\*SuperscriptBox[\(V\), \(\[Mu]\)]\)";
covariantDerivative2::usage ="covariantDerivative2[A,\[Mu],\[Nu],\[Alpha]] receives the 2-dimensional table A and the indices \[Mu], \[Nu] and \[Alpha] and return \!\(\*SubscriptBox[\(\[Del]\), \(\[Alpha]\)]\)\!\(\*SuperscriptBox[\(A\), \(\[Mu]\[Nu]\)]\)";
covariantDerivative3::usage ="covariantDerivative3[B,\[Mu],\[Nu],\[Rho],\[Alpha]] receives the 3-dimensional table B and the indices \[Mu], \[Nu],\[Rho] and \[Alpha] and return \!\(\*SubscriptBox[\(\[Del]\), \(\[Alpha]\)]\)\!\(\*SuperscriptBox[\(B\), \(\[Mu]\[Nu]\[Rho]\)]\)";

Raise::usage ="Raise[t] receives a list or a table t and raises all of its components using metricDual.
Raise[t, i] receives a list or a table t and raises only index i using metricDual.";
Lower::usage ="Lower[t] receives a list or a table t and lowers all of its components using metricDual.
Lower[t, i] receives a list or a table t and lowers only index i using metricDual.";




(* ::Section:: *)
(*Messages *)


Contract::lengthDif ="The tensors should have the same Length; equivalently: the dimensions of the indices being contracted sould be same.";

Contract::sorry ="This case has not been implemented yet.";


(* ::Section:: *)
(*Definitions*)


 Begin["`Private`"];


(* ::Subsection:: *)
(*Conventions*)


Conventions[]:=Multicolumn[{
"\!\(\*
StyleBox[\"Symbols\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"Implemented\",\nFontWeight->\"Bold\"]\)",
" ",
"\!\(\*
StyleBox[\"Curvature\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"Tensors\",\nFontWeight->\"Bold\"]\)",
"metric[[\[Alpha],\[Beta]]]",
"metricDual[[\[Alpha],\[Beta]]]",
"Christoffel[[\[Mu],\[Nu],\[Kappa]]]",
"Riemann[[\[Alpha],\[Beta],\[Mu],\[Kappa]]]",
"Ricci[[\[Alpha],\[Beta]]]",
"RicciSquared",
"RicciScalar",
"KretschmannScalar",
"EinsteinTensor[[\[Mu],\[Nu]]]",
"EinsteinTensorPlus\[CapitalLambda][[\[Mu],\[Nu]]]",
" ",
"\!\(\*
StyleBox[\"Covariant\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"Derivative\",\nFontWeight->\"Bold\"]\)",
"covariantDerivative1[V][[\[Mu],\[Alpha]]]",
"covariantDerivative2[A][[\[Mu],\[Nu],\[Alpha]]]",
"covariantDerivative3[B][[\[Mu],\[Nu],\[Rho],\[Alpha]]]",
" ",
"\!\(\*
StyleBox[\"Contraction\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"of\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"Tensors\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"examples\",\nFontWeight->\"Bold\"]\)",
"Contract[V,A,{1,2}]",
"Contract[V,A,{{1,3},{2,1}}]",
" ",
"\!\(\*
StyleBox[\"Raising\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"and\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"Lowering\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"examples\",\nFontWeight->\"Bold\"]\)",
"Raise[metric,2]",
"Raise[Ricci] or Raise[Ricci, {1,2}]",
"Lower[Riemann,4]",
"\!\(\*
StyleBox[\"Convention\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\"Chosen\",\nFontWeight->\"Bold\"]\)",
" ",
" ",
"\!\(\*SubscriptBox[\(g\), \(\[Alpha]\[Beta]\)]\) ",
"\!\(\*SuperscriptBox[\(g\), \(\[Alpha]\[Beta]\)]\) ",
"\!\(\*SuperscriptBox[SubscriptBox[\(\[CapitalGamma]\), \(\[Mu]\[Nu]\)], \(\[Kappa]\)]\)",
"\!\(\*SuperscriptBox[SubscriptBox[\(R\), \(\[Alpha]\[Beta]\[Mu]\)], \(\[Kappa]\)]\) ",
"\!\(\*SubscriptBox[\(R\), \(\[Alpha]\[Beta]\)]\) ",
"\!\(\*SuperscriptBox[\(R\), \(\[Alpha]\[Beta]\)]\)\!\(\*SubscriptBox[\(R\), \(\[Alpha]\[Beta]\)]\)",
"\!\(\*SuperscriptBox[\(g\), \(\[Alpha]\[Beta]\)]\)\!\(\*SubscriptBox[\(R\), \(\[Alpha]\[Beta]\)]\) ",
"\!\(\*SubscriptBox[\(R\), \(\[Alpha]\[Beta]\[Mu]\[Kappa]\)]\)\!\(\*SuperscriptBox[\(R\), \(\[Alpha]\[Beta]\[Mu]\[Kappa]\)]\) ",
"\!\(\*SubscriptBox[\(R\), \(\[Mu]\[Nu]\)]\) - \!\(\*FractionBox[\(1\), \(2\)]\) \!\(\*SuperscriptBox[\(g\), \(\[Alpha]\[Beta]\)]\)\!\(\*SubscriptBox[\(R\), \(\[Alpha]\[Beta]\)]\) \!\(\*SubscriptBox[\(g\), \(\[Mu]\[Nu]\)]\)",
"\!\(\*SubscriptBox[\(R\), \(\[Mu]\[Nu]\)]\) - \!\(\*FractionBox[\(1\), \(2\)]\) \!\(\*SuperscriptBox[\(g\), \(\[Alpha]\[Beta]\)]\)\!\(\*SubscriptBox[\(R\), \(\[Alpha]\[Beta]\)]\) \!\(\*SubscriptBox[\(g\), \(\[Mu]\[Nu]\)]\) + \[CapitalLambda] \!\(\*SubscriptBox[\(g\), \(\[Mu]\[Nu]\)]\) ",
" ",
" ",
"\!\(\*SubscriptBox[\(\[Del]\), \(\[Alpha]\)]\)\!\(\*SuperscriptBox[\(V\), \(\[Mu]\)]\)",
"\!\(\*SubscriptBox[\(\[Del]\), \(\[Alpha]\)]\)\!\(\*SuperscriptBox[\(A\), \(\[Mu]\[Nu]\)]\) ",
"\!\(\*SubscriptBox[\(\[Del]\), \(\[Alpha]\)]\)\!\(\*SuperscriptBox[\(B\), \(\[Mu]\[Nu]\[Rho]\)]\) ",
" ",
" ",
"\!\(\*SubscriptBox[\(V\), \(\[Mu]\)]\)\!\(\*SuperscriptBox[\(A\), \(\[Nu]\[Mu]\)]\) ",
"\!\(\*SubscriptBox[\(A\), \(\[Mu]\[Nu]\)]\)\!\(\*SuperscriptBox[\(B\), \(\[Nu]\[Rho]\[Mu]\)]\) ",
" ",
" ",
"\!\(\*SuperscriptBox[SubscriptBox[\(g\), \(\[Mu]\)], \(\[Nu]\)]\) ",
"\!\(\*SuperscriptBox[\(R\), \(\[Mu]\[Nu]\)]\) ",
"\!\(\*SubscriptBox[\(R\), \(\[Alpha]\[Beta]\[Mu]\[Kappa]\)]\)"
},2,Background -> {Automatic,{{LightBlue,White}}},Frame->All,FrameStyle -> White];


(* ::Subsection:: *)
(*CurvatureTensors*)


Options[CurvatureTensors]={
"Riemannian"->False,
"Toggle"->False
};
CurvatureTensors[metric0_, coordinates_, OptionsPattern[]] := Module[{\[Alpha],\[Beta],\[Gamma],\[Delta],\[Kappa],\[Mu],\[Nu],a,b,c,d,e,i,j,k,l,x},

Global`metric = metric0;

Global`dimension = Length[Global`metric];

Global`metricDual = Inverse[Global`metric]//Simplify;

Global`sqrtDet =If[OptionValue["Riemannian"],Sqrt[Det[Global`metric]],Sqrt[-Det[Global`metric]]];

Global`Christoffel = Table[(1/2)*Sum[(Global`metricDual[[\[Kappa],\[Sigma]]])*(D[Global`metric[[\[Sigma],\[Nu]]],coordinates[[\[Mu]]] ]+D[Global`metric[[\[Sigma],\[Mu]]],coordinates[[\[Nu]]] ]-D[Global`metric[[\[Mu],\[Nu]]],coordinates[[\[Sigma]]] ]),{\[Sigma],1,Global`dimension}]//Simplify,{\[Mu],1,Global`dimension},{\[Nu],1,Global`dimension},{\[Kappa],1,Global`dimension}];

Global`Riemann = Table[(-D[Global`Christoffel[[\[Beta],\[Mu],\[Kappa]]],coordinates[[\[Alpha]]]]+D[Global`Christoffel[[\[Alpha],\[Mu],\[Kappa]]],coordinates[[\[Beta]]]]+Sum[Global`Christoffel[[\[Alpha],\[Mu],\[Lambda]]]Global`Christoffel[[\[Beta],\[Lambda],\[Kappa]]]-Global`Christoffel[[\[Beta],\[Mu],\[Lambda]]]Global`Christoffel[[\[Alpha],\[Lambda],\[Kappa]]],{\[Lambda],1,Global`dimension}])//Simplify,{\[Alpha],1,Global`dimension},{\[Beta],1,Global`dimension},{\[Mu],1,Global`dimension},{\[Kappa],1,Global`dimension}];

Global`Ricci = Table[Sum[Global`Riemann[[\[Alpha],\[Kappa],\[Beta],\[Kappa]]],{\[Kappa],1,Global`dimension}]//Simplify,{\[Alpha],1,Global`dimension},{\[Beta],1,Global`dimension}];

Global`RicciUpper = Table[Sum[Global`metricDual[[\[Beta],\[Nu]]]Global`metricDual[[\[Alpha],\[Mu]]] Global`Ricci[[\[Mu],\[Nu]]],{\[Mu],1,Global`dimension},{\[Nu],1,Global`dimension}]//Simplify,{\[Alpha],1,Global`dimension},{\[Beta],1,Global`dimension}];

Global`RicciSquared = Tr[Global`Ricci.Global`RicciUpper];

Global`RicciScalar = Sum[Global`metricDual[[\[Alpha],\[Beta]]]Global`Ricci[[\[Alpha],\[Beta]]],{\[Alpha],1,Global`dimension},{\[Beta],1,Global`dimension}]//Simplify;

Global`KretschmannScalar =  Sum[Global`metric[[d,e]]Global`Riemann[[a,b,c,e]] Global`metricDual[[a,\[Alpha]]]Global`metricDual[[b,\[Beta]]]Global`metricDual[[c,\[Chi]]] Global`Riemann[[\[Alpha],\[Beta],\[Chi],d]] ,{\[Alpha],1,Global`dimension},{\[Beta],1,Global`dimension},{\[Chi],1,Global`dimension},{a,1,Global`dimension},{b,1,Global`dimension},{c,1,Global`dimension},{d,1,Global`dimension},{e,1,Global`dimension}]//Simplify;

Global`EinsteinTensor =  Global`Ricci -  1/2 Global`RicciScalar Global`metric//Simplify;

Global`EinsteinTensorPlus\[CapitalLambda] =  Global`Ricci + (Global`\[CapitalLambda] -  1/2 Global`RicciScalar) Global`metric//Simplify;

If[OptionValue["Toggle"], {Global`Riemann, Global`Ricci, Global`RicciScalar, Global`EinsteinTensor} = {-Global`Riemann, -Global`Ricci, -Global`RicciScalar, -Global`EinsteinTensor}];


(* to better visualize the non-vanishing components *)

Global`metricComponents = DeleteCases[Flatten[Table[If[Global`metric[[i,j]]=!=0,StringForm["\!\(\*SubscriptBox[\(g\), \(`1``2`\)]\) = `3`",coordinates[[i]],coordinates[[j]],Global`metric[[i,j]]],"Zero"],{i,1,Global`dimension},{j,1,Global`dimension}],3],"Zero"];

Global`metricDualComponents = DeleteCases[Flatten[Table[If[Global`metricDual[[i,j]]=!=0,StringForm["\!\(\*SuperscriptBox[\(g\), \(`1``2`\)]\) = `3`",coordinates[[i]],coordinates[[j]],Global`metricDual[[i,j]]],"Zero"],{i,1,Global`dimension},{j,1,Global`dimension}],3],"Zero"];

Global`ChristoffelComponents = DeleteCases[Flatten[Table[If[Global`Christoffel[[i,j,k]]=!=0,StringForm["\!\(\*SuperscriptBox[SubscriptBox[\(\[CapitalGamma]\), \(`1``2`\)], \(`3`\)]\) = `4`",coordinates[[i]],coordinates[[j]],coordinates[[k]],Global`Christoffel[[i,j,k]]],"Zero"],{i,1,Global`dimension},{j,1,Global`dimension},{k,1,Global`dimension}],3],"Zero"];

Global`RiemannComponents = DeleteCases[Flatten[Table[If[Global`Riemann[[i,j,k,l]]=!=0,StringForm["\!\(\*SuperscriptBox[SubscriptBox[\(R\), \(`1``2``3`\)], \(`4`\)]\) = `5`",coordinates[[i]],coordinates[[j]],coordinates[[k]],coordinates[[l]],Global`Riemann[[i,j,k,l]]],"Zero"],{i,1,Global`dimension},{j,1,Global`dimension},{k,1,Global`dimension},{l,1,Global`dimension}],3],"Zero"];

Global`RicciComponents = DeleteCases[Flatten[Table[If[Global`Ricci[[i,j]]=!=0,StringForm["\!\(\*SubscriptBox[\(R\), \(`1``2`\)]\) = `3`",coordinates[[i]],coordinates[[j]],Global`Ricci[[i,j]]],"Zero"],{i,1,Global`dimension},{j,1,Global`dimension}],3],"Zero"];

Global`EinsteinComponents = DeleteCases[Flatten[Table[If[Global`EinsteinTensor[[i,j]]=!=0,StringForm["\!\(\*SubscriptBox[\(G\), \(`1``2`\)]\) = `3`",coordinates[[i]],coordinates[[j]],Global`EinsteinTensor[[i,j]]],"Zero"],{i,1,Global`dimension},{j,1,Global`dimension}],3],"Zero"];


(* covariant derivatives of tensors of rank 1, 2 an 3 *)

covariantDerivative1[V_,\[Mu]_,\[Alpha]_] := D[V[[\[Mu]]],coordinates[[\[Alpha]]]]+Sum[Global`Christoffel[[\[Alpha],\[Nu],\[Mu]]] V[[\[Nu]]],{\[Nu],1,Global`dimension}]//Simplify;

covariantDerivative2[A_,\[Mu]_,\[Nu]_,\[Alpha]_] := D[A[[\[Mu],\[Nu]]],coordinates[[\[Alpha]]]]+Sum[Global`Christoffel[[\[Alpha],\[Kappa],\[Mu]]] A[[\[Kappa],\[Nu]]],{\[Kappa],1,Global`dimension}]+Sum[Global`Christoffel[[\[Alpha],\[Kappa],\[Nu]]] A[[\[Mu],\[Kappa]]],{\[Kappa],1,Global`dimension}]//Simplify;

covariantDerivative3[B_,\[Mu]_,\[Nu]_,\[Rho]_,\[Alpha]_]:= D[B[[\[Mu],\[Nu],\[Rho]]],coordinates[[\[Alpha]]]]+Sum[Global`Christoffel[[\[Alpha],\[Kappa],\[Mu]]] B[[\[Kappa],\[Nu],\[Rho]]],{\[Kappa],1,Global`dimension}]+Sum[Global`Christoffel[[\[Alpha],\[Kappa],\[Nu]]] B[[\[Mu],\[Kappa],\[Rho]]],{\[Kappa],1,Global`dimension}]+Sum[Global`Christoffel[[\[Alpha],\[Kappa],\[Rho]]] B[[\[Mu],\[Nu],\[Kappa]]],{\[Kappa],1,Global`dimension}]//Simplify;

]



(* For myself, because I like it like this*)

(* How can I set the same options for several functions in a way that Options[] still makes sense for all of them?? *)
Options[PrettyChristoffel]={
"columns" -> 1,
"framed"->False,
"backgroundColors"-> {LightPurple,White}
};
Options[PrettyRiemann]={
"columns" -> 1,
"framed"->False,
"backgroundColors"-> {LightPurple,White}
};
Options[PrettyRicci]={
"columns" -> 1,
"framed"->False,
"backgroundColors"-> {LightPurple,White}
};
Options[PrettyEinstein]={
"columns" -> 1,
"framed"->False,
"backgroundColors"-> {LightPurple,White}
};

PrettyChristoffel[OptionsPattern[PrettyEinstein]] := Module[{tab},
tab = Multicolumn[Global`ChristoffelComponents,OptionValue["columns"],Spacings -> {1,1},Background -> {Automatic,{OptionValue["backgroundColors"]}},Frame->All,FrameStyle -> White,Appearance->"Horizontal"];
If[OptionValue["framed"],Return@Framed[tab],Return[tab]]
];

PrettyRiemann[OptionsPattern[PrettyEinstein]] := Module[{tab},
tab = Multicolumn[Global`RiemannComponents,OptionValue["columns"],Spacings -> {1,1},Background -> {Automatic,{OptionValue["backgroundColors"]}},Frame->All,FrameStyle -> White,Appearance->"Horizontal"];
If[OptionValue["framed"],Return@Framed[tab],Return[tab]]
];

PrettyRicci[OptionsPattern[PrettyEinstein]] := Module[{tab},
tab = Multicolumn[Global`RicciComponents,OptionValue["columns"],Spacings -> {1,1},Background -> {Automatic,{OptionValue["backgroundColors"]}},Frame->All,FrameStyle -> White,Appearance->"Horizontal"];
If[OptionValue["framed"],Return@Framed[tab],Return[tab]]
];

PrettyEinstein[OptionsPattern[]] := Module[{tab},
tab = Multicolumn[Global`EinsteinComponents,OptionValue["columns"],Spacings -> {1,1},Background -> {Automatic,{OptionValue["backgroundColors"]}},Frame->All,FrameStyle -> White,Appearance->"Horizontal"];
If[OptionValue["framed"],Return@Framed[tab],Return[tab]]
];



(* ::Subsection:: *)
(*Contract*)


Contract11[t1_,t2_]:=Module[{\[Mu],n},
n=Length[t1];
 Return[Sum[t1[[\[Mu]]]t2[[\[Mu]]],{\[Mu],1,n}]]
];

Contract12[t1_,t2_,indices_:"Default"]:=Module[{\[Mu],\[Nu],n},
n=Length[t1];
If[indices==={1,1}||indices==="Default", Return[Table[Sum[t1[[\[Mu]]]t2[[\[Mu],\[Nu]]],{\[Mu],1,n}],{\[Nu],1,n}]]];
If[indices==={1,2}, Return[Table[Sum[t1[[\[Mu]]]t2[[\[Nu],\[Mu]]],{\[Mu],1,n}],{\[Nu],1,n}]]]
];

Contract22[t1_,t2_,indices_:"Default"]:=Module[{\[Mu],\[Nu],\[Alpha],\[Beta],n},
n=Length[t1];

If[indices==={1,1}, Return[Table[Simplify@Sum[t1[[\[Mu],\[Alpha]]]t2[[\[Mu],\[Beta]]],{\[Mu],1,n}],{\[Alpha],1,n},{\[Beta],1,n}]]];
If[indices==={1,2}, Return[Table[Simplify@Sum[t1[[\[Mu],\[Alpha]]]t2[[\[Beta],\[Mu]]],{\[Mu],1,n}],{\[Alpha],1,n},{\[Beta],1,n}]]];
If[indices==={2,1}, Return[Table[Simplify@Sum[t1[[\[Alpha],\[Mu]]]t2[[\[Mu],\[Beta]]],{\[Mu],1,n}],{\[Alpha],1,n},{\[Beta],1,n}]]];
If[indices==={2,2}, Return[Table[Simplify@Sum[t1[[\[Alpha],\[Mu]]]t2[[\[Beta],\[Mu]]],{\[Mu],1,n}],{\[Alpha],1,n},{\[Beta],1,n}]]];

If[indices==={{1,1},{2,2}}||indices==="Default", Return[Simplify@Sum[t1[[\[Mu],\[Nu]]]t2[[\[Mu],\[Nu]]],{\[Mu],1,n},{\[Nu],1,n}]]];
If[indices==={{1,2},{2,1}}, Return[Simplify@Sum[t1[[\[Mu],\[Nu]]]t2[[\[Nu],\[Mu]]],{\[Mu],1,n},{\[Nu],1,n}]]]
];

Contract13[t1_,t2_,indices_:"Default"]:=Module[{\[Mu],\[Alpha],\[Beta],n}, 
n=Length[t1];
If[indices==={1,1}||indices==="Default", Return[Table[Simplify@Sum[t1[[\[Mu]]]t2[[\[Mu],\[Alpha],\[Beta]]],{\[Mu],1,n}],{\[Alpha],1,n},{\[Beta],1,n}]]];
If[indices==={1,2}, Return[Table[Simplify@Sum[t1[[\[Mu]]]t2[[\[Alpha],\[Mu],\[Beta]]],{\[Mu],1,n}],{\[Alpha],1,n},{\[Beta],1,n}]]];
If[indices==={1,3}, Return[Table[Simplify@Sum[t1[[\[Mu]]]t2[[\[Alpha],\[Beta],\[Mu]]],{\[Mu],1,n}],{\[Alpha],1,n},{\[Beta],1,n}]]]
];

Contract23[t1_,t2_,indices_:"Default"]:=Module[{\[Mu],\[Nu],\[Alpha],\[Beta],n}, 
n=Length[t1];

If[indices==={1,1}, Return[Table[Simplify@Sum[t1[[\[Mu],\[Nu]]]t2[[\[Mu],\[Alpha],\[Beta]]],{\[Mu],1,n}],{\[Nu],1,n},{\[Alpha],1,n},{\[Beta],1,n}]]];
If[indices==={1,2}, Return[Table[Simplify@Sum[t1[[\[Mu],\[Nu]]]t2[[\[Alpha],\[Mu],\[Beta]]],{\[Mu],1,n}],{\[Nu],1,n},{\[Alpha],1,n},{\[Beta],1,n}]]];
If[indices==={1,3}, Return[Table[Simplify@Sum[t1[[\[Mu],\[Nu]]]t2[[\[Alpha],\[Beta],\[Mu]]],{\[Mu],1,n}],{\[Nu],1,n},{\[Alpha],1,n},{\[Beta],1,n}]]];

If[indices==={2,1}, Return[Table[Simplify@Sum[t1[[\[Nu],\[Mu]]]t2[[\[Mu],\[Alpha],\[Beta]]],{\[Mu],1,n}],{\[Nu],1,n},{\[Alpha],1,n},{\[Beta],1,n}]]];
If[indices==={2,2}, Return[Table[Simplify@Sum[t1[[\[Nu],\[Mu]]]t2[[\[Alpha],\[Mu],\[Beta]]],{\[Mu],1,n}],{\[Nu],1,n},{\[Alpha],1,n},{\[Beta],1,n}]]];
If[indices==={2,3}, Return[Table[Simplify@Sum[t1[[\[Nu],\[Mu]]]t2[[\[Alpha],\[Beta],\[Mu]]],{\[Mu],1,n}],{\[Nu],1,n},{\[Alpha],1,n},{\[Beta],1,n}]]];

If[indices==={{1,1},{2,2}}||indices==="Default", Return[Table[Sum[t1[[\[Mu],\[Nu]]]t2[[\[Mu],\[Nu],\[Alpha]]],{\[Mu],1,n},{\[Nu],1,n}],{\[Alpha],1,n}]]];
If[indices==={{1,1},{2,3}}, Return[Table[Simplify@Sum[t1[[\[Mu],\[Nu]]]t2[[\[Mu],\[Alpha],\[Nu]]],{\[Mu],1,n},{\[Nu],1,n}],{\[Alpha],1,n}]]];
If[indices==={{1,2},{2,1}}, Return[Table[Simplify@Sum[t1[[\[Mu],\[Nu]]]t2[[\[Nu],\[Mu],\[Alpha]]],{\[Mu],1,n},{\[Nu],1,n}],{\[Alpha],1,n}]]];
If[indices==={{1,2},{2,3}}, Return[Table[Simplify@Sum[t1[[\[Mu],\[Nu]]]t2[[\[Alpha],\[Mu],\[Nu]]],{\[Mu],1,n},{\[Nu],1,n}],{\[Alpha],1,n}]]];
If[indices==={{1,3},{2,1}}, Return[Table[Simplify@Sum[t1[[\[Mu],\[Nu]]]t2[[\[Nu],\[Alpha],\[Mu]]],{\[Mu],1,n},{\[Nu],1,n}],{\[Alpha],1,n}]]];
If[indices==={{1,3},{2,2}}, Return[Table[Simplify@Sum[t1[[\[Mu],\[Nu]]]t2[[\[Alpha],\[Nu],\[Mu]]],{\[Mu],1,n},{\[Nu],1,n}],{\[Alpha],1,n}]]];
];

Contract33[t1_,t2_,indices_:"Default"]:=Module[{\[Mu],\[Nu],\[Gamma],n}, 
n=Length[t1];
If[indices==={{1,2,3},{1,2,3}}||indices==="Default", Return[Simplify@Sum[t1[[\[Mu],\[Nu],\[Gamma]]]t2[[\[Mu],\[Nu],\[Gamma]]],{\[Mu],1,n},{\[Nu],1,n},{\[Gamma],1,n}]]];
If[indices==={{1,2,3},{1,3,2}}, Return[Simplify@Sum[t1[[\[Mu],\[Nu],\[Gamma]]]t2[[\[Mu],\[Gamma],\[Nu]]],{\[Mu],1,n},{\[Nu],1,n},{\[Gamma],1,n}]]];
If[indices==={{1,2,3},{2,3,1}}, Return[Simplify@Sum[t1[[\[Mu],\[Nu],\[Gamma]]]t2[[\[Nu],\[Gamma],\[Mu]]],{\[Mu],1,n},{\[Nu],1,n},{\[Gamma],1,n}]]];
If[indices==={{1,2,3},{2,1,3}}, Return[Simplify@Sum[t1[[\[Mu],\[Nu],\[Gamma]]]t2[[\[Nu],\[Mu],\[Gamma]]],{\[Mu],1,n},{\[Nu],1,n},{\[Gamma],1,n}]]];
If[indices==={{1,2,3},{3,1,2}}, Return[Simplify@Sum[t1[[\[Mu],\[Nu],\[Gamma]]]t2[[\[Gamma],\[Mu],\[Nu]]],{\[Mu],1,n},{\[Nu],1,n},{\[Gamma],1,n}]]];
If[indices==={{1,2,3},{3,2,1}}, Return[Simplify@Sum[t1[[\[Mu],\[Nu],\[Gamma]]]t2[[\[Gamma],\[Nu],\[Mu]]],{\[Mu],1,n},{\[Nu],1,n},{\[Gamma],1,n}]]];
];

Contract14[t1_,t2_,indices_:"Default"]:=Module[{\[Mu],\[Alpha],\[Beta],\[Gamma],n}, 
n=Length[t1];
If[indices==={1,1}||indices==="Default", Return[Table[Simplify@Sum[t1[[\[Mu]]]t2[[\[Mu],\[Alpha],\[Beta],\[Gamma]]],{\[Mu],1,n}],{\[Alpha],1,n},{\[Beta],1,n},{\[Gamma],1,n}]]];
If[indices==={1,2}, Return[Table[Simplify@Sum[t1[[\[Mu]]]t2[[\[Alpha],\[Mu],\[Beta],\[Gamma]]],{\[Mu],1,n}],{\[Alpha],1,n},{\[Beta],1,n},{\[Gamma],1,n}]]];
If[indices==={1,3}, Return[Table[Simplify@Sum[t1[[\[Mu]]]t2[[\[Alpha],\[Beta],\[Mu],\[Gamma]]],{\[Mu],1,n}],{\[Alpha],1,n},{\[Beta],1,n},{\[Gamma],1,n}]]];
If[indices==={1,4}, Return[Table[Simplify@Sum[t1[[\[Mu]]]t2[[\[Alpha],\[Beta],\[Gamma],\[Mu]]],{\[Mu],1,n}],{\[Alpha],1,n},{\[Beta],1,n},{\[Gamma],1,n}]]]
];

Contract24[t1_,t2_,indices_:"Default"]:=Module[{\[Mu],\[Alpha],\[Beta],\[Gamma],n}, 
n=Length[t1];
If[indices==={1,1}, Return[Table[Simplify@Sum[t1[[\[Mu],\[Nu]]]t2[[\[Mu],\[Alpha],\[Beta],\[Gamma]]],{\[Mu],1,n}],{\[Alpha],1,n},{\[Beta],1,n},{\[Gamma],1,n},{\[Nu],1,n}]]];
If[indices==={1,2}, Return[Table[Simplify@Sum[t1[[\[Mu],\[Nu]]]t2[[\[Alpha],\[Mu],\[Beta],\[Gamma]]],{\[Mu],1,n}],{\[Alpha],1,n},{\[Beta],1,n},{\[Gamma],1,n},{\[Nu],1,n}]]];
If[indices==={1,3}, Return[Table[Simplify@Sum[t1[[\[Mu],\[Nu]]]t2[[\[Alpha],\[Beta],\[Mu],\[Gamma]]],{\[Mu],1,n}],{\[Alpha],1,n},{\[Beta],1,n},{\[Gamma],1,n},{\[Nu],1,n}]]];
If[indices==={1,4}, Return[Table[Simplify@Sum[t1[[\[Mu],\[Nu]]]t2[[\[Alpha],\[Beta],\[Gamma],\[Mu]]],{\[Mu],1,n}],{\[Alpha],1,n},{\[Beta],1,n},{\[Gamma],1,n},{\[Nu],1,n}]]];

If[indices==={2,1}, Return[Table[Simplify@Sum[t1[[\[Nu],\[Mu]]]t2[[\[Mu],\[Alpha],\[Beta],\[Gamma]]],{\[Mu],1,n}],{\[Alpha],1,n},{\[Beta],1,n},{\[Gamma],1,n},{\[Nu],1,n}]]];
If[indices==={2,2}, Return[Table[Simplify@Sum[t1[[\[Nu],\[Mu]]]t2[[\[Alpha],\[Mu],\[Beta],\[Gamma]]],{\[Mu],1,n}],{\[Alpha],1,n},{\[Beta],1,n},{\[Gamma],1,n},{\[Nu],1,n}]]];
If[indices==={2,3}, Return[Table[Simplify@Sum[t1[[\[Nu],\[Mu]]]t2[[\[Alpha],\[Beta],\[Mu],\[Gamma]]],{\[Mu],1,n}],{\[Alpha],1,n},{\[Beta],1,n},{\[Gamma],1,n},{\[Nu],1,n}]]];
If[indices==={2,4}, Return[Table[Simplify@Sum[t1[[\[Nu],\[Mu]]]t2[[\[Alpha],\[Beta],\[Gamma],\[Mu]]],{\[Mu],1,n}],{\[Alpha],1,n},{\[Beta],1,n},{\[Gamma],1,n},{\[Nu],1,n}]]];

If[indices==={{1,1},{2,2}}||indices==="Default", Return[Table[Simplify@Sum[t1[[\[Mu],\[Nu]]]t2[[\[Mu],\[Nu],\[Beta],\[Gamma]]],{\[Mu],1,n},{\[Nu],1,n}],{\[Alpha],1,n},{\[Beta],1,n},{\[Gamma],1,n}]]];
If[indices==={{1,1},{2,3}}, Return[Table[Simplify@Sum[t1[[\[Mu],\[Nu]]]t2[[\[Mu],\[Beta],\[Nu],\[Gamma]]],{\[Mu],1,n},{\[Nu],1,n}],{\[Alpha],1,n},{\[Beta],1,n},{\[Gamma],1,n}]]];
If[indices==={{1,1},{2,4}}, Return[Table[Simplify@Sum[t1[[\[Mu],\[Nu]]]t2[[\[Mu],\[Beta],\[Gamma],\[Nu]]],{\[Mu],1,n},{\[Nu],1,n}],{\[Alpha],1,n},{\[Beta],1,n},{\[Gamma],1,n}]]];
If[indices==={{1,2},{2,1}}, Return[Table[Simplify@Sum[t1[[\[Mu],\[Nu]]]t2[[\[Nu],\[Mu],\[Beta],\[Gamma]]],{\[Mu],1,n},{\[Nu],1,n}],{\[Alpha],1,n},{\[Beta],1,n},{\[Gamma],1,n}]]];
If[indices==={{1,2},{2,3}}, Return[Table[Simplify@Sum[t1[[\[Mu],\[Nu]]]t2[[\[Beta],\[Mu],\[Nu],\[Gamma]]],{\[Mu],1,n},{\[Nu],1,n}],{\[Alpha],1,n},{\[Beta],1,n},{\[Gamma],1,n}]]];
If[indices==={{1,2},{2,4}}, Return[Table[Simplify@Sum[t1[[\[Mu],\[Nu]]]t2[[\[Beta],\[Mu],\[Gamma],\[Nu]]],{\[Mu],1,n},{\[Nu],1,n}],{\[Alpha],1,n},{\[Beta],1,n},{\[Gamma],1,n}]]];
If[indices==={{1,3},{2,1}}, Return[Table[Simplify@Sum[t1[[\[Mu],\[Nu]]]t2[[\[Nu],\[Beta],\[Mu],\[Gamma]]],{\[Mu],1,n},{\[Nu],1,n}],{\[Alpha],1,n},{\[Beta],1,n},{\[Gamma],1,n}]]];
If[indices==={{1,3},{2,2}}, Return[Table[Simplify@Sum[t1[[\[Mu],\[Nu]]]t2[[\[Beta],\[Nu],\[Mu],\[Gamma]]],{\[Mu],1,n},{\[Nu],1,n}],{\[Alpha],1,n},{\[Beta],1,n},{\[Gamma],1,n}]]];
If[indices==={{1,3},{2,4}}, Return[Table[Simplify@Sum[t1[[\[Mu],\[Nu]]]t2[[\[Beta],\[Mu],\[Gamma],\[Nu]]],{\[Mu],1,n},{\[Nu],1,n}],{\[Alpha],1,n},{\[Beta],1,n},{\[Gamma],1,n}]]];
If[indices==={{1,4},{2,1}}, Return[Table[Simplify@Sum[t1[[\[Mu],\[Nu]]]t2[[\[Nu],\[Beta],\[Gamma],\[Mu]]],{\[Mu],1,n},{\[Nu],1,n}],{\[Alpha],1,n},{\[Beta],1,n},{\[Gamma],1,n}]]];
If[indices==={{1,4},{2,2}}, Return[Table[Simplify@Sum[t1[[\[Mu],\[Nu]]]t2[[\[Beta],\[Nu],\[Gamma],\[Mu]]],{\[Mu],1,n},{\[Nu],1,n}],{\[Alpha],1,n},{\[Beta],1,n},{\[Gamma],1,n}]]];
If[indices==={{1,4},{2,3}}, Return[Table[Simplify@Sum[t1[[\[Mu],\[Nu]]]t2[[\[Beta],\[Gamma],\[Nu],\[Mu]]],{\[Mu],1,n},{\[Nu],1,n}],{\[Alpha],1,n},{\[Beta],1,n},{\[Gamma],1,n}]]]
];

(*Contract34[t1_,t2_,indices_:{1,1}]*)

Contract44[t1_,t2_,indices_:"Default"]:=Module[{\[Mu],\[Nu],\[Gamma],\[Delta],\[Alpha],\[Beta],\[Xi],\[Rho],n}, 
n=Length[t1];
If[indices==={{1,2,3,4},{1,2,3,4}}||indices==="Default", Return[Sum[t1[[\[Mu],\[Nu],\[Alpha],\[Beta]]]t2[[\[Mu],\[Nu],\[Alpha],\[Beta]]],{\[Mu],1,n},{\[Nu],1,n},{\[Alpha],1,n},{\[Beta],1,n}]]];

If[indices==={1,1}, Return[Table[Simplify@Sum[t1[[\[Mu],\[Nu],\[Alpha],\[Beta]]]t2[[\[Mu],\[Gamma],\[Delta],\[Xi]]],{\[Mu],1,n}],{\[Nu],1,n},{\[Alpha],1,n},{\[Beta],1,n},{\[Gamma],1,n},{\[Delta],1,n},{\[Xi],1,n}]]];
If[indices==={1,2}, Return[Table[Simplify@Sum[t1[[\[Mu],\[Nu],\[Alpha],\[Beta]]]t2[[\[Gamma],\[Mu],\[Delta],\[Xi]]],{\[Mu],1,n}],{\[Nu],1,n},{\[Alpha],1,n},{\[Beta],1,n},{\[Gamma],1,n},{\[Delta],1,n},{\[Xi],1,n}]]];
If[indices==={1,3}, Return[Table[Simplify@Sum[t1[[\[Mu],\[Nu],\[Alpha],\[Beta]]]t2[[\[Gamma],\[Delta],\[Mu],\[Xi]]],{\[Mu],1,n}],{\[Nu],1,n},{\[Alpha],1,n},{\[Beta],1,n},{\[Gamma],1,n},{\[Delta],1,n},{\[Xi],1,n}]]];
If[indices==={1,4}, Return[Table[Simplify@Sum[t1[[\[Mu],\[Nu],\[Alpha],\[Beta]]]t2[[\[Gamma],\[Delta],\[Xi],\[Mu]]],{\[Mu],1,n}],{\[Nu],1,n},{\[Alpha],1,n},{\[Beta],1,n},{\[Gamma],1,n},{\[Delta],1,n},{\[Xi],1,n}]]];

If[indices==={2,1}, Return[Table[Simplify@Sum[t1[[\[Nu],\[Mu],\[Alpha],\[Beta]]]t2[[\[Mu],\[Gamma],\[Delta],\[Xi]]],{\[Mu],1,n}],{\[Nu],1,n},{\[Alpha],1,n},{\[Beta],1,n},{\[Gamma],1,n},{\[Delta],1,n},{\[Xi],1,n}]]];
If[indices==={2,2}, Return[Table[Simplify@Sum[t1[[\[Nu],\[Mu],\[Alpha],\[Beta]]]t2[[\[Gamma],\[Mu],\[Delta],\[Xi]]],{\[Mu],1,n}],{\[Nu],1,n},{\[Alpha],1,n},{\[Beta],1,n},{\[Gamma],1,n},{\[Delta],1,n},{\[Xi],1,n}]]];
If[indices==={2,3}, Return[Table[Simplify@Sum[t1[[\[Nu],\[Mu],\[Alpha],\[Beta]]]t2[[\[Gamma],\[Delta],\[Mu],\[Xi]]],{\[Mu],1,n}],{\[Nu],1,n},{\[Alpha],1,n},{\[Beta],1,n},{\[Gamma],1,n},{\[Delta],1,n},{\[Xi],1,n}]]];
If[indices==={2,4}, Return[Table[Simplify@Sum[t1[[\[Nu],\[Mu],\[Alpha],\[Beta]]]t2[[\[Gamma],\[Delta],\[Xi],\[Mu]]],{\[Mu],1,n}],{\[Nu],1,n},{\[Alpha],1,n},{\[Beta],1,n},{\[Gamma],1,n},{\[Delta],1,n},{\[Xi],1,n}]]];

If[indices==={3,1}, Return[Table[Simplify@Sum[t1[[\[Nu],\[Alpha],\[Mu],\[Beta]]]t2[[\[Mu],\[Gamma],\[Delta],\[Xi]]],{\[Mu],1,n}],{\[Nu],1,n},{\[Alpha],1,n},{\[Beta],1,n},{\[Gamma],1,n},{\[Delta],1,n},{\[Xi],1,n}]]];
If[indices==={3,2}, Return[Table[Simplify@Sum[t1[[\[Nu],\[Alpha],\[Mu],\[Beta]]]t2[[\[Gamma],\[Mu],\[Delta],\[Xi]]],{\[Mu],1,n}],{\[Nu],1,n},{\[Alpha],1,n},{\[Beta],1,n},{\[Gamma],1,n},{\[Delta],1,n},{\[Xi],1,n}]]];
If[indices==={3,3}, Return[Table[Simplify@Sum[t1[[\[Nu],\[Alpha],\[Mu],\[Beta]]]t2[[\[Gamma],\[Delta],\[Mu],\[Xi]]],{\[Mu],1,n}],{\[Nu],1,n},{\[Alpha],1,n},{\[Beta],1,n},{\[Gamma],1,n},{\[Delta],1,n},{\[Xi],1,n}]]];
If[indices==={3,4}, Return[Table[Simplify@Sum[t1[[\[Nu],\[Alpha],\[Mu],\[Beta]]]t2[[\[Gamma],\[Delta],\[Xi],\[Mu]]],{\[Mu],1,n}],{\[Nu],1,n},{\[Alpha],1,n},{\[Beta],1,n},{\[Gamma],1,n},{\[Delta],1,n},{\[Xi],1,n}]]];

If[indices==={4,1}, Return[Table[Simplify@Sum[t1[[\[Nu],\[Alpha],\[Beta],\[Mu]]]t2[[\[Mu],\[Gamma],\[Delta],\[Xi]]],{\[Mu],1,n}],{\[Nu],1,n},{\[Alpha],1,n},{\[Beta],1,n},{\[Gamma],1,n},{\[Delta],1,n},{\[Xi],1,n}]]];
If[indices==={4,2}, Return[Table[Simplify@Sum[t1[[\[Nu],\[Alpha],\[Beta],\[Mu]]]t2[[\[Gamma],\[Mu],\[Delta],\[Xi]]],{\[Mu],1,n}],{\[Nu],1,n},{\[Alpha],1,n},{\[Beta],1,n},{\[Gamma],1,n},{\[Delta],1,n},{\[Xi],1,n}]]];
If[indices==={4,3}, Return[Table[Simplify@Sum[t1[[\[Nu],\[Alpha],\[Beta],\[Mu]]]t2[[\[Gamma],\[Delta],\[Mu],\[Xi]]],{\[Mu],1,n}],{\[Nu],1,n},{\[Alpha],1,n},{\[Beta],1,n},{\[Gamma],1,n},{\[Delta],1,n},{\[Xi],1,n}]]];
If[indices==={4,4}, Return[Table[Simplify@Sum[t1[[\[Nu],\[Alpha],\[Beta],\[Mu]]]t2[[\[Gamma],\[Delta],\[Xi],\[Mu]]],{\[Mu],1,n}],{\[Nu],1,n},{\[Alpha],1,n},{\[Beta],1,n},{\[Gamma],1,n},{\[Delta],1,n},{\[Xi],1,n}]]]
(* 
If[indices\[Equal]{{1,1},{2,2}}, Return[Table[Sum[t1[[\[Nu],\[Alpha],\[Beta],\[Mu]]]t2[[\[Mu],\[Gamma],\[Delta],\[Xi]]],{\[Mu],1,n}],{\[Nu],1,n},{\[Alpha],1,n},{\[Beta],1,n},{\[Gamma],1,n},{\[Delta],1,n},{\[Xi],1,n}]]];............
............. uff!
I need to learn how to implement something more sofisticated. I did want something simple, but this is absurd. *)
];

Contract[t1_,t2_,indices_:"Default"]:=(
If[Length[t1]=!= Length[t2],Message[Contract::lengthDif];Return[] ];

If[Length@Dimensions@t1=== 1&&Length@Dimensions@t2===1, Return@Contract11[t1,t2]];
If[Length@Dimensions@t1===2&&Length@Dimensions@t2===2, Return@Contract22[t2,t1,indices]];
If[Length@Dimensions@t1===3&&Length@Dimensions@t2===3, Return@Contract33[t2,t1,indices]];
If[Length@Dimensions@t1===4&&Length@Dimensions@t2===4&&(indices==="Default"||indices=={{1,2,3,4},{1,2,3,4}}), Return@Contract44[t1,t2,indices]];

If[Length@Dimensions@t1===1&&Length@Dimensions@t2===2, Return@Contract12[t1,t2,indices]];
If[Length@Dimensions@t1===2&&Length@Dimensions@t2===1, Return@Contract12[t2,t1,indices]];

If[Length@Dimensions@t1===1&&Length@Dimensions@t2===3,Return@ Contract13[t1,t2,indices]];
If[Length@Dimensions@t1===3&&Length@Dimensions@t2===1, Return@Contract13[t2,t1,indices]];

If[Length@Dimensions@t1===1&&Length@Dimensions@t2===4, Return@Contract14[t1,t2,indices]];If[Length@Dimensions@t1===4&&Length@Dimensions@t2===1, Return@Contract14[t2,t1,indices]];

If[Length@Dimensions@t1===2&&Length@Dimensions@t2===3, Return@Contract23[t1,t2,indices]];If[Length@Dimensions@t1===3&&Length@Dimensions@t2===2, Return@Contract23[t2,t1,indices]];

If[Length@Dimensions@t1===2&&Length@Dimensions@t2===4, Return@Contract24[t1,t2,indices]];If[Length@Dimensions@t1===4&&Length@Dimensions@t2===2, Return@Contract24[t2,t1,indices]];

(* TODO: If[Length@Dimensions@t1\[Equal]3&&Length@Dimensions@t2\[Equal]4, Contract34[t1,t2,indices]];If[Length@Dimensions@t1\[Equal]4&&Length@Dimensions@t2\[Equal]3, Contract34[t2,t1,indices]];*)

Message[Contract::sorry];Return[]
)


(* ::Subsection:: *)
(*Raise and Lower*)


Raise[t_,i_:-1,j_:1]:= Module[{nt,ni},
nt=Length@Dimensions@t;
ni=If[i===-1,nt,Length[i]]; 
	If[nt===1,Return@Contract12[t,Global`metricDual]];
	If[nt===2,
		If[ni<2,Return@Contract22[t,Global`metricDual,Flatten[{i,j}]],
			If[ni===2,Return@Contract22[Global`metricDual,Contract22[Global`metricDual,t,{1,2}],{2,1}]
			]
		]
	];
	If[nt==3,
		If[ni<2, Return@Contract23[Global`metricDual,t,Flatten[{j,i}]],
			If[ni==2, Return@Contract23[Global`metricDual,Contract23[Global`metricDual,t,{1,2}],{2,1}],
				If[ni==3, Return@Contract23[Global`metricDual,Contract23[Global`metricDual,Contract23[Global`metricDual,t,{2,3}],{2,2}],{2,1}]
				]
			]
		]
	];
	If[nt==4,
		If[ni<2, Return@Contract24[Global`metricDual,t,Flatten[{j,i}]],
			If[ni==2, Return@Contract24[Global`metricDual,Contract24[Global`metricDual,t,{1,2}],{2,1}],
				If[ni==3, Return@Contract24[Global`metricDual,Contract24[Global`metricDual,Contract24[Global`metricDual,t,{j,1}],{j,2}],{j,3}],
					If[ni==4, Return@Contract24[Global`metricDual,Contract24[Global`metricDual,Contract24[Global`metricDual,Contract24[Global`metricDual,t,{j,1}],{j,2}],{j,3}],{j,4}]
					]	
				]
			]
		]
	]
];

Lower[t_,i_:-1,j_:1]:= Module[{nt,ni},
nt=Length@Dimensions@t;
ni=If[i===-1,nt,Length[i]]; 
	If[nt===1,Return@Contract12[t,Global`metric]];
	If[nt===2,
		If[ni<2,Return@Contract22[t,Global`metric,Flatten[{i,j}]],
			If[ni===2,Return@Contract22[Global`metric,Contract22[Global`metric,t,{1,2}],{2,1}]
			]
		]
	];
	If[nt==3,
		If[ni<2, Return@Contract23[Global`metric,t,Flatten[{j,i}]],
			If[ni==2, Return@Contract23[Global`metric,Contract23[Global`metric,t,{1,2}],{2,1}],
				If[ni==3, Return@Contract23[Global`metric,Contract23[Global`metric,Contract23[Global`metric,t,{2,3}],{2,2}],{2,1}]
				]
			]
		]
	];
	If[nt==4,
		If[ni<2, Return@Contract24[Global`metric,t,Flatten[{j,i}]],
			If[ni==2, Return@Contract24[Global`metric,Contract24[Global`metric,t,{1,2}],{2,1}],
				If[ni==3, Return@Contract24[Global`metric,Contract24[Global`metric,Contract24[Global`metric,t,{j,1}],{j,2}],{j,3}],
					If[ni==4, Return@Contract24[Global`metric,Contract24[Global`metric,Contract24[Global`metric,Contract24[Global`metric,t,{j,1}],{j,2}],{j,3}],{j,4}]
					]	
				]
			]
		]
	]
];


(* ::Chapter:: *)
(**)


 End[];


EndPackage[];

