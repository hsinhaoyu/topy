BeginPackage["retinostats`"]
<<Topy/vizhandmap.m

Options[rfStats]= Join[
	{
		RFFilterFunc	-> Function[{zz},True],
		FoveaCoords		-> {0, 0},
		StatsFunc       -> Automatic
	},
	Options[rfCoords]
];

rfStats::usage="";

Begin["`Private`"]

<<Topy/polygongeometry.m

(* lst is {polygon1, polygon2....} *)
(* polygon1 is {x1, y1, x2, y2... } *)
preprocess[lst_]:=
	Map[Partition[#, 2]&, lst]

(* lst is only single RF {{x1, y1}, {x2, y2}, {x3, y3}, {x4, y4}} *)
analyzeRF[lst_]:= {
	polygonEccentricity[lst]/Degree,
	polygonPolarAngle[lst]/Degree,
	polygonLength[lst]/Degree
}

rfStats[data0_, opts:OptionsPattern[]]:=
	Module[{data1, data2, data3, f},
		data1 = Select[data0, OptionValue[RFFilterFunc]];
		data2 = rfCoords[data1, 
					CoordsOnly	-> True,
					OutputInRad -> True,
					FilterRules[
						{opts},
						Options[rfCoords]
					]
				];

		If[OptionValue[StatsFunc]===Automatic,
			f = analyzeRF,
			f = OptionValue[StatsFunc]
		];

		data3 = analyzeRF/@preprocess[data2];
		Return[
			MapThread[
				Join[#1, #2]&,
				{data1, data3}
			]
		]
	]
End[]
EndPackage[]