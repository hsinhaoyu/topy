BeginPackage["vizHandmap2`"]

Options[generateScotomaG]={
	ScotomaStyle->Directive[{EdgeForm[{Thickness[0.005], Orange}], FaceForm[]}]
};

Options[generateBkgG] = Join[{
		ScotomaBoundary		-> {},									(* in rad *)
		LongitudeRange		-> {-50.0 Degree, 10.0 Degree},			(* in rad *)
		LatitudeRange		-> {-30.0 Degree, 30.0 Degree},			(* in rad *)
		Resolution			-> 10.0 Degree,
		CardinalStyle		-> Thickness[0.007],					(* for background *)
		FoveaCoords			-> {0.0 Degree, 0.0 Degree}				(* in rad *),
		ProjectionFunc		-> Automatic
	},
	Options[generateScotomaG],
	Options[ParametricPlot]
];

Options[shiftCenter] = {
	InputInRad			-> False,
	OutputInRad			-> False
};

(* If CoordsOnly, only return a list of coordinates. Else, return coordinates with annotations *)
Options[rfCoords]={
	CoordIndex0			-> 3,
	CoordIndex1			-> -3,
	FoveaCoords			-> {0.0 Degree, 0.0 Degree},				(* in rad *)
	InputInRad			-> False,
	OutputInRad			-> False,
	CoordsOnly          -> False
};

(* if ProjCoordsOnly is True, only return projected coordinates *)
(* if not, return a list of polygons. Can be visualized directly with Graphics *)
Options[polygonsPerTract]=Join[{
		Items			-> All,
		ProjCoordsOnly	-> False,
		ProjectionFunc	-> Automatic
	},
	Options[rfCoords]
];

(* PolygonColorFunc can be a function or a list *)
Options[polygonStylesPerTract]={
		Items				-> All,
		PolygonStyleFunc	-> Automatic,
		PolygonColorFunc	-> Automatic,
		Extras               -> None			(* allows additional style directives to be added *)
};

Options[generatePolygonCenterMarkersG] = Join[
	{
		PolygonCenterMarkerStyle		->	Automatic,
		PolygonCenterConnectorStyle		->	Automatic,
		PolygonCenterMarkers			->	False,
		PolygonCenterConnectors			->	False,
		TooltipFunc						->	None
	},
	Options[polygonsPerTract]
];

Options[generatePerTractG] = Join[
	{
		TooltipFunc		-> Function[{line}, line[[1]]],
		HidePolygons	-> False
	},
	Options[generatePolygonCenterMarkersG],
	Options[polygonsPerTract],
	Options[polygonStylesPerTract]
];

Options[vizHandmap1] = Join[
	{
		RFFilterFunc -> Function[{zz},True],
		DisplayBackground -> True
	},
	Options[generatePerTractG],
	Options[generateBkgG],
	Options[rfCoords],
	Options[Graphics]
]

(* ====== supports for virtual tracts ===== *)
(* By default, the second element of a record is the tract ID *)
(* The code assumes that one of the elements is the tract ID. If not, changeTractID needs to be rewritten *)
Options[changeTractID] = {TractIDPosition -> 2};
Options[selectRFs] = {RFIDFunc -> First};
Options[mkVirtualTracts] = Join[Options[selectRFs], Options[changeTractID]];

Options[vizHandmap2] = Join[
	{
		TractIDFunc			-> Function[{zz}, zz[[2]]],
		TractColorFunc		-> Hue,
		DisplayTracts		-> All,
		VirtualTracts       -> None
	},
	Options[mkVirtualTracts],
	Options[generatePolygonCenterMarkersG],
	Options[vizHandmap1]
];

shiftCenter::usage="";
rfCoords::usage="";

generateScotomaG::usage="";
generateBkgG::usage="";

polygonsPerTract::usage="";
polygonStylesPerTract::usage="";
generatePolygonCenterMarkersG::usage="";

generatePerTractG::usage="";
vizHandmap1::usage="";
vizHandmap2::usage="";

readRFs::usage="";
mkVirtualTracts::usage="";



Begin["`Private`"]
<<Topy/vizUtilities2D.m
<<Topy/projections.m
<<Topy/polygongeometry.m

defaultMarkerStyle = Directive[Black, PointSize[0.01]];

(* ========== basic I/O ========== *)

tt[str_] := If[StringMatchQ[StringTrim[str], NumberString],
				ToExpression[str],
				ToUpperCase[str]
			]

readRFs[filename_, opts:OptionsPattern[]] :=
	Map[
		Map[tt, StringSplit[#, "\t"]] &,
		Import[filename, "List"]
	]

(* ========== virtual tracts ============ *)

changeTractID[lst_, newID_, opts : OptionsPattern[]] :=
	Map[
		ReplacePart[#, OptionValue[TractIDPosition] -> newID] &,
		lst
	]

selectRFs[lst_, rfID_?StringQ, opts : OptionsPattern[]] :=
	Module[{f},
		f = OptionValue[RFIDFunc];
		Return[
			Select[lst, f[#] == ToUpperCase[rfID] &][[1]]
		]
	]

selectRFs[lst_, rfIDs_List, opts : OptionsPattern[]] :=
	Map[
		selectRFs[lst, #, opts] &,
		rfIDs
	]

singleListQ[lst_] := Length[Dimensions[lst]] == 1

mkVirtualTracts[lst_, rfLists_, opts : OptionsPattern[]] :=
	Module[{zz},
		zz = Map[
			selectRFs[lst, #] &,
			rfLists,
			FilterRules[{opts}, Options[selectRFs]]
		];

		Flatten[
			Table[
				changeTractID[
					zz[[i]],
					i,
					FilterRules[{opts}, Options[changeTractID]]
				],
				{i, 1, Length[zz]}
			],
			1
		]
	]

(* ========== coordinates manipulation ========== *)
myReplace[lst_, {id0_, id1_}, newval_] :=
	Module[{h, t},
		h = lst[[1 ;; id0 - 1]];
		t = lst[[id1 + 1 ;; -1]];
		Return[
			Join[h, newval, t]
		]
	]

Options[shiftCenter0] = Options[shiftCenter];
(* note: fovea is always in rad *)
(* dataline can be in rad or in degree *)
shiftCenter0[dataline_, {id0_, id1_}, fovea_, OptionsPattern[]] :=
	Module[{zz},
		zz = Partition[dataline[[id0;;id1]], 2];
		
		If[OptionValue[InputInRad],
			zz = Map[# - fovea &, zz],
			zz = Map[#*Degree - fovea &, zz]
		];
		zz = Flatten[zz];

		If[Not[OptionValue[OutputInRad]],
			zz = zz/Degree
		];
		
		Return[
			myReplace[dataline, {id0, id1}, zz]
		]
	]

shiftCenter[data_, {id0_, id1_}, fovea_, opts:OptionsPattern[]] :=
	Map[
		shiftCenter0[
			#, 
			{id0, id1}, 
			fovea, 
			FilterRules[{opts}, Options[shiftCenter0]]
		] &, 
		data
	]

(* Note that rfCoords[] works in Deg, rather than Rad, as everything else *)
rfCoords[data0_, opts:OptionsPattern[]]:=
	Module[{data1, coordExtractFunc},
		data1 = shiftCenter[
			data0,
			{OptionValue[CoordIndex0], OptionValue[CoordIndex1]},
			OptionValue[FoveaCoords],
			FilterRules[{opts}, Options[shiftCenter]]		
		];

		coordExtractFunc[line_]:=line[[OptionValue[CoordIndex0];;OptionValue[CoordIndex1]]];

		If[OptionValue[CoordsOnly],
			Map[coordExtractFunc, data1],
			data1 
		]
	]

(* ========== display elements ========== *)

(* lst = {{lambda, phi}...} in rad *)
(* fovea = {lambda, phi} in rad *)
(* TODO: does not support projection function *)
generateScotomaG[lst_, fovea_, OptionsPattern[]]:=
	Module[{zz, f},
		zz = Map[(#-fovea)&, lst];
		f = sphericalProjectionFunction["lambert"];
		Return[
			Graphics[{
				OptionValue[ScotomaStyle],
				Polygon[Map[f, zz]]
			}]
		]
	]

(* TODO: scotoma does not support multiple projection function *)
(* note: bkgG does *)
generateBkgG[opts:OptionsPattern[]]:=
	Module[{scotomaG},
		(* ===== scotoma *)
		scotomaG={};
		If[OptionValue[ScotomaBoundary]!={},
			scotomaG = generateScotomaG[
				OptionValue[ScotomaBoundary],
				OptionValue[FoveaCoords],
				FilterRules[
					{opts},
					Options[generateScotomaG]
				]
			]
		];

		(* ==== background *)
		bkgG = unitSphere2D[
			FilterRules[{opts}, Options[unitSphere2D]]
		];

		Show[bkgG, scotomaG]

	]

generatePolygonCenterMarkersG[data_, opts:OptionsPattern[]]:=
	Module[{zz, zz2, g1, o, g2, ff, labels},
		zz = polygonsPerTract[
				data,
				ProjCoordsOnly->True,
				FilterRules[
					{opts},
					Options[polygonsPerTract]
				]
		];

		zz2 = Map[polygonCenter, zz];
		
		If[OptionValue[TooltipFunc]===None,
			ff = Identity,
			
			(* else *)
			labels = Map[OptionValue[TooltipFunc], data];
			ff[x_]:= Module[{zz}, zz = Tooltip[x, labels[[1]]]; labels=Rest[labels]; Return[zz]];
		];

		(* ==== markers ==== *)
		o = OptionValue[PolygonCenterMarkerStyle];
		If[o==Automatic, o = defaultMarkerStyle];
		
		(* TODO: this is very fragile code *)
		If[OptionValue[PolygonCenterMarkers],
			If[ListQ[o],
				(* if we have style for each point *)
				g1 = Table[{o[[i]], ff[Point[zz2[[i]]]]}, {i, 1, Length[zz2]}],
				(* else, not a list. one style for all *)
				g1 = Map[{o, ff[Point[#]]}&, zz2]
			],
			g1 = {}
		];

		(* ==== connectors ==== *)
		o = OptionValue[PolygonCenterConnectorStyle];
		If[o==Automatic, o = Directive[Gray, Thickness[0.005]]];
		If[OptionValue[PolygonCenterConnectors],
			g2 = {o, Line[zz2]},
			g2 = {}
		];
		
		Return[
			Show[
				Graphics[g2],
				Graphics[g1]
			]
		]
	]

(* ========== polygons in 2D ========== *)
mkPolygon[lst_]:=
	Polygon[
		Append[
			lst,
			lst[[1]]
		]
	]

projRF[lst_,f_]:=Map[f, Partition[lst, 2]]

polygonsPerTract[data_, opts:OptionsPattern[]]:=
	Module[{data0, data1, coords, polys, f},

		If[OptionValue[Items]===All,
			data0 = data,
			data0 = data[[OptionValue[Items]]]
		];
		
		If[OptionValue[ProjectionFunc]===Automatic,
			f = sphericalProjectionFunction["lambert"],
			f = OptionValue[ProjectionFunc]
		];

		data1 = rfCoords[
			data0,
			CoordsOnly->True,
			OutputInRad->True,
			FilterRules[
				{opts},
				Options[rfCoords]
			]
		];
		
		coords = Map[projRF[#, f]&, data1];
		If[OptionValue[ProjCoordsOnly],
			polys = coords,
			polys = Map[mkPolygon, coords];
		];

		Return[polys]
	]

(* ========== putting things together ========== *)

(* BUG: does not deal with Items *)
generatePerTractG[data_, opts:OptionsPattern[]]:=
	Module[{poly, s, r, zz, g1, zzz},
		If[OptionValue[HidePolygons],
			(* if hide polygons *)
			poly = {};
			s = {};
			r = Graphics[],
			(* else *)
			poly = polygonsPerTract[
					data, 
					FilterRules[
						{opts},
						Options[polygonsPerTract]
					]
				];
				
			s = polygonStylesPerTract[
					data,
					FilterRules[
						{opts},
						Options[polygonStylesPerTract]
					]
				];
	
			If[OptionValue[TooltipFunc]===None,
				(* if we are adding tooltip *)
				r = Graphics[
					Transpose[{s, poly}]
				],
				(* else *)
				If[OptionValue[Items]===All,
					zz = Map[OptionValue[TooltipFunc], data],
					zz = Map[OptionValue[TooltipFunc], data[[OptionValue[Items]]]]
				];
				r = Graphics[
						Table[
							{s[[i]], Tooltip[poly[[i]], zz[[i]]]},
							{i, 1, Length[s]}
						]
				]
			]
		];
		
		If[OptionValue[HidePolygons],
			(* if we are not showing the polygons, transfer the styles and tooltip to markers *)			
			
			If[OptionValue[PolygonCenterMarkerStyle]===Automatic,
				zzz = defaultMarkerStyle,
				zzz = OptionValue[PolygonCenterMarkerStyle]
			];
			
			zzz = Cases[{zzz}, PointSize[___]]; (* pick only pointsize *)
			
			s = polygonStylesPerTract[
					data,
					Extras -> zzz, (* add point size information to the colors *)
					FilterRules[
						{opts},
						Options[polygonStylesPerTract]
					]
				];

			g1 = generatePolygonCenterMarkersG[
					data,
					PolygonCenterMarkers->True, (* override setting, automatically turn on markers *)
					TooltipFunc->OptionValue[TooltipFunc],
					PolygonCenterMarkerStyle->s,
					FilterRules[
						{opts},
						Options[generatePolygonCenterMarkersG]
					]
			],
			(* else *)
			g1 = generatePolygonCenterMarkersG[
					data,
					TooltipFunc->None,
					FilterRules[
						{opts},
						Options[generatePolygonCenterMarkersG]
					]
			]
		];

		Return[Show[r, g1]]
	]

vizHandmap1[data0_, opts:OptionsPattern[]]:=
	Module[{data1, g0, g1},
		data1=Select[data0, OptionValue[RFFilterFunc]];

		If[OptionValue[DisplayBackground],
			g0 = generateBkgG[
				FilterRules[
					{opts},
					Options[generateBkgG]
				]
			],
			g0 = {}
		];

		g1 = generatePerTractG[
			data1,
			FilterRules[
				{opts},
				Options[generatePerTractG]
			]
		];

		Show[g0, g1]
	]

vizHandmap2[data0_, opts:OptionsPattern[]]:=
	Module[{data1, g0, data2, cf, zz, g1, g2, tractIDs, tractLookup, items},
	
		If[Not[OptionValue[VirtualTracts]===None],
			(* if using virtual tracts *)
			data1 = mkVirtualTracts[
						data0, 
						OptionValue[VirtualTracts],
						FilterRules[
							{opts},
							Options[mkVirtualTracts]
						]
					];
					
			Return[
				vizHandmap2[
					data1,
					VirtualTracts->None,
					opts
				]
			],
		
			(* if not using virtual tracts *)
			data1 = Select[data0, OptionValue[RFFilterFunc]];

			If[OptionValue[DisplayBackground],
				g0 = generateBkgG[
					FilterRules[
						{opts},
						Options[generateBkgG]
					]
				],
				g0 = {}
			];

			data2 = GatherBy[
				data1,
				OptionValue[TractIDFunc]
			];

			(*  tract id *)

			tractIDs = 
				Map[
					OptionValue[TractIDFunc], 
					Map[First, data2]
				];
			tractLookup[tractID_]:=Position[tractIDs, tractID][[1,1]];

			(* color *)
			cf = OptionValue[TractColorFunc];
			zz = Table[cf[N[i/Length[data2]]], {i, 1, Length[data2]}];
			
			(* process Items option *)
			If[OptionValue[Items]===All,
				items = Table[All, {Length[data2]}],
				items = OptionValue[Items]
			];

			(* generate the content *)
			g1 = Table[
				generatePerTractG[
					data2[[i]],
					PolygonColorFunc->Function[{ww}, zz[[i]]],
					Items->items[[i]],
					FilterRules[
						{opts},
						Options[generatePerTractG]
					]
				],
				{i, 1, Length[data2]}
			];

			If[OptionValue[DisplayTracts]===All,
				g2 = g1,
				g2 = g1[[
						Map[
							tractLookup,
							OptionValue[DisplayTracts]
						]
				]]
			];

			Return[Show[g0, g2]]
		]

	]


(* ========== slyistic ========== *)
ipsiStyle = {
   EdgeForm[{Black, Thickness[0.004]}],
   FaceForm[Opacity[0.05]]
   };

contraStyle = {
   EdgeForm[{Black, Thickness[0.004]}],
   FaceForm[Opacity[0.3]]
   };

defaultPolygonStyleFunc[dataline_]:=
	Module[{eye, style},
		eye = dataline[[-1]];
		If[eye == "i",
			style = ipsiStyle,
			style = contraStyle
		];
		Return[style]
	]

defaultPolygonColorFunc[dataline_]:=
	Hue[Random[]]

polygonStylesPerTract[data_, opts:OptionsPattern[]]:=
	Module[{data0, polyStyle, polyColor, extras},
		If[OptionValue[Items]===All,
			data0 = data,
			data0 = data[[OptionValue[Items]]]
		];

		If[OptionValue[PolygonStyleFunc]===Automatic,
			polyStyle = Map[defaultPolygonStyleFunc, data0],
			polyStyle = Map[OptionValue[PolygonStyleFunc], data0]
		];

		If[OptionValue[PolygonColorFunc]===Automatic,
			polyColor = Map[defaultPolygonColorFunc, data0],
			If[ListQ[OptionValue[PolygonColorFunc]],
				polyColor = OptionValue[PolygonColorFunc],
				polyColor = Map[OptionValue[PolygonColorFunc], data0]
			]
		];
		
		If[OptionValue[Extras]===None,
			extras = Table[{}, {Length[polyColor]}],
			extras = Table[OptionValue[Extras], {Length[polyColor]}]
		];

		Return[
			Map[
				Apply[Directive, Flatten[#]]&,
				Transpose[{polyStyle, polyColor, extras}]
			]
		]
	]


End[]
EndPackage[]