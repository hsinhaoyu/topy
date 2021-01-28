BeginPackage["SphericalGeometry`"]

s2c::usage="";
c2s::usage="";

eccentricity::usage="";
greatCircleDistance::usage="";
polarAngle::usage="";

sphericalAngleCAB::usage="";

sphericalPolygonArea::usage="";

Begin["`Private`"]

Needs["VectorAnalysis`"] (* for CrossProduct *)

(* ============ basic coordinate transform ========== *)
(* convert spherical coordinate to cartesian coordinate *)
s2c[{lambda_, phi_}] :=
	{
		Cos[phi]*Sin[lambda],
		Sin[phi],
		Cos[phi]*Cos[lambda]
	}

(* cartesian coordinate to spherical coordinate *)
c2s[{x_, y_, z_}] :=
	{
		Sqrt[x^2 + y^2 + z^2],
		ArcTan[z, x],
		ArcTan[Sqrt[x^2 + z^2], y]
	}

(* ============ basic geometry ============== *)

greatCircleDistance[{lambda0_, phi0_}, {lambda1_, phi1_}]:=
	ArcCos[
		s2c[{lambda0, phi0}].
		s2c[{lambda1, phi1}]
	]

(* return the angle of CAB *)
sphericalAngleCAB[{pC_, pA_, pB_}]:=
	Module[{ppA, ppB, ppC},
		ppA = s2c[pA];
		ppB = s2c[pB];
		ppC = s2c[pC];
		ArcCos[
			(CrossProduct[ppC, ppA].CrossProduct[ppB, ppA])/
			(Norm[CrossProduct[ppC, ppA]]*Norm[CrossProduct[ppB, ppA]])
		]
	]

(* ============ polar coordinates =========== *)

eccentricity[{lambda_, phi_}]:=
	ArcCos[
		Cos[lambda]*Cos[phi]
	]

polarAngle[{0, 0}] := 0

polarAngle[{lambda_, phi_}] :=
	Module[{e, r, v},
		e = N[eccentricity[{lambda, phi}]];
		v = ArcSin[Sin[phi]/Sin[e]];
		
		If[lambda >= 0,
			r = v,
			If[N[phi] == 0.0,
				r = N[Pi],
				If[phi > 0,
					r = Pi - v,
					r = -Pi - v
				]
			]
		];
		Return[N[Chop[r, 10^-4]]]
	]


(* ========== spherical area ========== *)

innerAngles[lst_] :=
	Module[{zz},
		zz = Join[{lst[[-1]]}, lst, {lst[[1]]}];
		Return[
			Table[
				{zz[[i - 1]], zz[[i]], zz[[i + 1]]},
				{i, 2, Length[zz] - 1}
			]
		]
	]
	
(* lst is {{lambda, phi}...} in rad *)
(* assume in consistent order *)
sphericalPolygonArea[lst_]:=
	Total[Map[sphericalAngleCAB, innerAngles[lst]]] - (Length[lst]-2)*Pi
	

(* ========== old ========== *)

(* not useful anymore. Kept for future reference *)
quadArea[{{lamba0_, lamba1_}, {phi0_, phi1_}}]:=
	(lamba1-lamba0) * ( Sin[phi1] - Sin[phi0])
	
(* pA, pB, pC: {lambda, phi} in rad *)
(* not used since it is just a special case of sphericalPolygonArea *)
(* kept for reference *)
sphericalTrigArea[{pA_, pB_, pC_}] := 
	Module[{a, b, c, s, e}, 
		a = greatCircleDistance[pB, pC];
		b = greatCircleDistance[pA, pC];
		c = greatCircleDistance[pA, pB];
		s = (a + b + c)/2.0;
		e = ArcTan[
				Sqrt[
					Tan[s/2.0]*
					Tan[(s - a)/2.0]*
					Tan[(s - b)/2.0]*
					Tan[(s - c)/2.0]
				]
			]*4.0;
	  Return[e]
	]

End[]
EndPackage[]