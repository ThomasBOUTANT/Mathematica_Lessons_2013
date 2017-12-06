
(*************************************************
 * Représentation graphique
 * d'arbres syntaxiques
 *************************************************)

BeginPackage["ArbreGraphique`"]

voir::usage =
	"voir[e] donne une représentation graphique de l'arborescence
	syntaxique de l'expression e."
	
Begin["`Private`"]

(*************************************************)
(* Calcul de l'arborescence *)

(* arbre[e,d] décompose l'expression e en ses composantes
	syntaxiques. Le point essentiel est d'empêcher toute
	évaluation lors de la décomposition.
	Le paramètre d est là pour des raisons historiques, et
	mémorise le nombre d'appels récursifs.
*)

arbre[e0_, d_] := 
	Module[{e, h, tete, s, k},
		e = e0;
		h = Depth[e];
		If[h == 2, 
			{ReleaseHold[e]},
			tete = e[[1, 0]];
			e[[1, 0]] = List;
			e = ReleaseHold[Map[Hold, e, {2}]];
			s = {tete};
			Do[
				AppendTo[s, arbre[e[[k]], d + 1]],
				{k, 1, Length[e]}
        	];
			s
		]
    ]

SetAttributes[versArbre, HoldFirst]
versArbre[e_] := arbre[Hold[e], 0]

(*************************************************)
(* Affichage graphique des arborescences *)

(* arbreGraphique[e,x1,y1,x2,y2,h] renvoie une liste de
	primitives graphiques de type Line ou Text, censés
	représenter graphiquement l'expression e.
	x1,y1 : coordonnées du coin inférieur gauche de l'image.
	x2,y2 : coordonnées du coin supérieur droit de l'image.
	h : hauteur de l'expression e.

*)

arbreGraphique[e_, x1_, y1_, x2_, y2_, 0] := 
	{{Text[ToString[e], {(x1 + x2)/2., y2}]}};
      
arbreGraphique[e_, x1_, y1_, x2_, y2_, h_] := Module[
		{k, n, s, xe, xk, yk, delta, theta, dx, dy},
		n = Length[e] - 1;
		delta = 0.1;
		If[Depth[e] > 1,
			xe = (x1 + x2)/2.;
			s = {{RGBColor[1, 1, 1], Text[ToString[e[[1]]], {xe, y2}]}};
			Do[
				xk = x1 + (k + 0.5)*(x2 - x1) / n;      
				s = Join[s, 
					arbreGraphique[
						e[[k + 2]], 
						x1 + k * (x2 - x1) / n, y1, 
						x1 + (k + 1)* (x2 - x1) / n, y2 - h, 
						h - 1]
					];
				theta = ArcTan[xk - xe, -h];
				(*dx = delta * (xk - xe) * Cos[theta];*)
				dy = delta * (-h) * Sin[theta];
				s = Join[s, {RGBColor[1,0,0],{Line[
					{{xe, y2 - dy}, {xk, y2 - h + dy}}]
					}}],
				{k, 0, n - 1}
				],
			s = {{Text[ToString[e], {(x1 + x2)/2, y2}]}}
			];
		s
		];
      
arbreGraphique[e_] := arbreGraphique[e, 0., 0., 1., 1., Depth[e]-1]

(*************************************************)
(* Fonction de visualisation *)

(* voir[e] affiche l'arborescence syntaxique de e *)

SetAttributes[voir, HoldFirst]

voir[e_,options___] := 
	Show[Graphics[arbreGraphique[versArbre[e]]], 
	options,
	ImageSize -> {400, 300}, 
	Background -> GrayLevel[0], 
	AspectRatio -> 0.75,
    TextStyle -> {FontFamily -> "Terminal", FontSize -> 12}, 
    PlotRange -> All
   	]

End[]

EndPackage[]
