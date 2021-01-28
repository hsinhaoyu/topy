BeginPackage["PolygonMath`"]

polygonCenter::usage="";
polygonArea::usage="";
polygonEccentricity::usage="";
polygonPolarAngle::usage="";
polygonLength::usage="";

Begin["`Private`"]
<<Topy/sphericalgeometry.m

(* lst is {pt1, pt2...} *)
(* pt1 = {logitude, latitude} in rad *)
(* Note that this is how polygon center is calculated in Tristan paper *)
polygonCenter[lst_]:=Mean[lst]
	
polygonArea[lst_]:=sphericalPolygonArea[lst]
	
polygonLength[lst_]:=
	If[Length[lst]!=4,
		Return[-99],
		Return[
			Sqrt[polygonArea[lst]]
		]
	]

(* lst is {pt1, pt2...} *)
(* pt1 = {logitude, latitude} in rad *)
polygonEccentricity[lst_]:=
	eccentricity[
		polygonCenter[lst]
	]

polygonPolarAngle[lst_]:=
	polarAngle[
		polygonCenter[lst]
	]

End[]
EndPackage[]