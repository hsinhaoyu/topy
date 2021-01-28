BeginPackage["VizUtilitiez2D`"]

Options[unitSphere2D] = Join[
		{
			LongitudeRange	-> {-90.0 Degree, 90.0 Degree}, 
			LatitudeRange	-> {-90.0 Degree, 90.0 Degree},
			CardinalStyle	-> Black,
			ProjectionFunc	-> Automatic,
			Resolution		-> 10.0 Degree
		}, 
		Options[ParametricPlot]
	];

Begin["`Private`"]
<<Topy/projections.m

unitSphere2D[opts:OptionsPattern[]] :=
	Module[{x00, x11, y00, y11, n1, n2, g, g2, f},
		x00 = OptionValue[LongitudeRange][[1]];
		x11 = OptionValue[LongitudeRange][[2]];
		y00 = OptionValue[LatitudeRange][[1]];
		y11 = OptionValue[LatitudeRange][[2]];

		n1 = (x11 - x00)/OptionValue[Resolution] - 1;
		n2 = (y11 - y00)/OptionValue[Resolution] - 1;

		
		If[OptionValue[ProjectionFunc]===Automatic,
			f = sphericalProjectionFunction["lambert"],
			f = OptionValue[ProjectionFunc]
		];


		g=ParametricPlot[
			f[{x , y}],
			{x, x00, x11},
			{y, y00, y11},
			Evaluate[
				FilterRules[{opts}, Options[ParametricPlot]]
			],
			Frame -> False,
			Ticks -> False,
			Mesh -> {Ceiling[n1], Ceiling[n2]},
			Axes -> False,
			ColorFunction -> Function[{x, y, u}, White],
			BoundaryStyle -> {Thickness[0.005], Black}
		];

		g2 = Graphics[{
				OptionValue[CardinalStyle],
				Line[{
					f[{x00, 0.0}], f[{x11, 0.0}]
				}],
				Line[{
					f[{0.0, y00}], f[{0.0, y11}]
				}]
		}];

		Return[Show[g, g2]]
  ]

End[]
EndPackage[]