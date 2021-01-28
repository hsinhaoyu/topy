BeginPackage["SphericalProjections`"]

sphericalProjectionFunction::usage="";

Begin["`Private`"]
<< Topy/sphericalgeometry.m

(* ============ stereographic projection ========== *)
stereographicCart[{x_, y_, z_}] := {x/(1 + z), y/(1 + z)}
stereographic[{lambda_, phi_}] := 
	stereographicCart[s2c[{lambda, phi}]]

(* ============ lambert projection ========== *)
lambertCart[{x_, y_, z_}] :=
	{
		x*Sqrt[2.0/(1 + z)],
		y*Sqrt[2.0/(1 + z)]
	}

lambertCartNorthPole[{x_, y_, z_}] := {
	-x*Sqrt[2.0/(1 + y)], 
	 z*Sqrt[2.0/(1 + y)]
}
	
lambert[{lambda_, phi_}] :=
	lambertCart[s2c[{lambda, phi}]]

(* invert lambert coordinates to cartesian *)
invLambertC[{x_, y_}] := {
	Sqrt[1 - (x^2 + y^2)/4.0]*x, 
	Sqrt[1 - (x^2 + y^2)/4.0]*y, 
	1 - (x^2 + y^2)/2
}

(* invert lambert coordinates to spherical *)
invLambert[{x_,y_}]:=c2s[invLambertC[{x,y}]][[2;;3]]

(* ============ master ========== *)
sphericalProjectionFunction["lambert"] = lambert
sphericalProjectionFunction["stereographic"] = stereographic

End[]
EndPackage[]