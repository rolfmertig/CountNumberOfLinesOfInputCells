(* Author: Rolf Mertig, GluonVision GmbH, Berlin, Germany *)

(* Created by the Wolfram Workbench Feb 15, 2012 *)

(* License: LGPL , i.e., open source, use at your own risk *)

(* This is basically: 
http://mathematica.stackexchange.com/questions/1704/automatically-counting-the-number-of-lines-of-code-in-a-set-of-notebooks/1779#1779
*)

BeginPackage["CountNumberOfLinesOfInputCells`"]
(* Exported symbols added here with SymbolName::usage *) 
CountNumberOfLinesOfInputCells::usage = "CountNumberOfLinesOfInputCells[dir] counts the number of lines (with default 
line length 78) of input cells (at top level) of all notebooks in dir. The operation runs completely in a Mathematica kernel, using only MakeExpression and 
some fixes thereof (e.g. input cells are splitted (like Ctrl Shift M) programmatically at newlines, since MakeExpression might not work in such a case.";

Begin["`Private`"]
(* Implementation of the package *)

Options[CountNumberOfLinesOfInputCells] = {PageWidth -> 78}; 
CountNumberOfLinesOfInputCells[dirs__, OptionsPattern[]] :=
    Module[ {files, n},
        files = FileNames["*.nb", dirs];
        Print["counting lines of  ", 
        Length[files], " files in ", Riffle[Flatten[{dirs}], " "]];
        (*TODO: figure out why ParallelTable does not seem to work here; maybe Streams do not parallelize well? *)
        Total[Monitor[Table[SlocSingleNB[files[[n]], OptionValue[PageWidth]], 
           {n, 1, Length[files]}], n]]
    ]; 
SlocSingleNB[(f_String)?FileExistsQ, pagewidth_:78] :=
    Quiet[Module[ {inputcells, holdcompletelist, o, tmpfile, le},
              inputcells = (Cases[#1, Cell[_BoxData, "Input", ___]] & )[
                 Flatten[Replace[Get[f], Notebook[z_, ___] :> z] //. 
                   Cell[CellGroupData[{c__Cell}, _]] :> c]];
              (* Split inputcells at newlines. This is done because 
              MakeExpression[#,StandardForm]& @ BoxData[{ RowBox[{ RowBox[{"SetAttributes", "[", RowBox[{"f", ",", "Listable"}], "]"}], ";"}], "\n", 
                                                          RowBox[{ RowBox[{"f", "[", "x_", "]"}], ":=", RowBox[{"x", "^", "2"}]}]}] 
              does not work. This cell can be found in Defintion.nb *)
              inputcells = 
              inputcells //. {Cell[BoxData[{r1__, "\n".., r2__}], "Input", ___] :> Sequence[Cell[
              BoxData[{r1}]
              , "Input"], Cell[BoxData[{r2}], "Input"]], Cell[BoxData[{r1__, "\[IndentingNewLine]".., r2__}], 
              "Input"] :> Sequence[Cell[BoxData[{r1}], "Input"], Cell[BoxData[{r2}], "Input"]]};
              (* something is weird with Defition: e.g. 
                  MakeExpression[ RowBox[{"Definition", "[", "f", "]"}], StandardForm]
                  returns HoldComplete[Null]
                  Substitution Definition by Identity[Definition] seems to work.
              *)
              If[ !FreeQ[inputcells, "Definition"],
                  inputcells = inputcells /. 
                  "Definition" -> "Identity[Definition]"
              ];
              holdcompletelist = 
              (MakeExpression[First[#1], StandardForm] //. ErrorBox[err_] :> Sequence[] & ) /@ 
              inputcells;
              o = OpenWrite[];
              Do[WriteString[o, (StringJoin[ToString[#1, InputForm, PageWidth -> pagewidth], 
                   ";\n"] & )[Unevaluated @@ holdcompletelist[[i]]]], {i, Length[holdcompletelist]}];
              tmpfile = Close[o];
              le = Length[Import[tmpfile, "Lines"]];
              DeleteFile[tmpfile];
              le
          ]]; 
 (*
 This takes a couple of seconds and should count 1690 lines of code of 227 notebooks:

  AbsoluteTiming[CountNumberOfLinesOfInputCells[FileNameJoin@{$InstallationDirectory,"AddOns"},Infinity]]

This takes about 10 minutes and counts 13395865 lines of code of 10284 notebooks:
 AbsoluteTiming[CountNumberOfLinesOfInputCells[$InstallationDirectory, Infinity]]
*)

End[]

EndPackage[]

