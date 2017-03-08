(* Content-type: application/vnd.wolfram.cdf.text *)

(*** Wolfram CDF File ***)
(* http://www.wolfram.com/cdf *)

(* CreatedBy='Mathematica 10.0' *)

(*************************************************************************)
(*                                                                       *)
(*  The Mathematica License under which this file was created prohibits  *)
(*  restricting third parties in receipt of this file from republishing  *)
(*  or redistributing it by any means, including but not limited to      *)
(*  rights management or terms of use, without the express consent of    *)
(*  Wolfram Research, Inc. For additional information concerning CDF     *)
(*  licensing and redistribution see:                                    *)
(*                                                                       *)
(*        www.wolfram.com/cdf/adopting-cdf/licensing-options.html        *)
(*                                                                       *)
(*************************************************************************)

(*CacheID: 234*)
(* Internal cache information:
NotebookFileLineBreakTest
NotebookFileLineBreakTest
NotebookDataPosition[      1064,         20]
NotebookDataLength[     30459,        732]
NotebookOptionsPosition[     30939,        724]
NotebookOutlinePosition[     31386,        744]
CellTagsIndexPosition[     31343,        741]
WindowFrame->Normal*)

(* Beginning of Notebook Content *)
Notebook[{

Cell[CellGroupData[{
Cell[BoxData[
 RowBox[{"Manipulate", "[", 
  RowBox[{
   RowBox[{"GraphicsRow", "[", 
    RowBox[{"Quiet", "[", 
     RowBox[{"visceralGrowthF", "[", 
      RowBox[{"cl", ",", " ", "perR", ",", "apicalSR"}], "]"}], "]"}], "]"}], 
   ",", 
   RowBox[{"{", 
    RowBox[{
     RowBox[{"{", 
      RowBox[{"cl", ",", "10", ",", "\"\<Column Length\>\""}], "}"}], ",", 
     "0.1", ",", "100", ",", "0.1"}], "}"}], ",", 
   RowBox[{"{", 
    RowBox[{
     RowBox[{"{", 
      RowBox[{"perR", ",", "1", ",", "\"\<Peristome Radius\>\""}], "}"}], ",",
      "0.1", ",", "100", ",", "0.1"}], "}"}], ",", 
   RowBox[{"{", 
    RowBox[{
     RowBox[{"{", 
      RowBox[{"apicalSR", ",", "1", ",", "\"\<Apical System\>\""}], "}"}], 
     ",", "0.1", ",", "100", ",", "0.1"}], "}"}], ",", 
   RowBox[{"Initialization", "\[RuleDelayed]", 
    RowBox[{"{", 
     RowBox[{
      RowBox[{
       RowBox[{"CatEqn", "[", 
        RowBox[{"x_", ",", "a_"}], "]"}], ":=", 
       RowBox[{
        RowBox[{"a", " ", 
         RowBox[{"Cosh", "[", 
          RowBox[{"(", 
           RowBox[{"x", "/", "a"}], ")"}], "]"}]}], "-", "a"}]}], ",", 
      "\[IndentingNewLine]", 
      RowBox[{
       RowBox[{"CatLength", "[", 
        RowBox[{"pts_", ",", "a_"}], "]"}], ":=", 
       RowBox[{"Module", "[", 
        RowBox[{
         RowBox[{"{", "x", "}"}], ",", "\[IndentingNewLine]", 
         RowBox[{
          RowBox[{
           RowBox[{"catf", "[", "x_", "]"}], "=", 
           RowBox[{
            RowBox[{"a", " ", 
             RowBox[{"Cosh", "[", 
              RowBox[{"x", "/", "a"}], "]"}]}], "  ", "-", " ", "a"}]}], ";", 
          "\[IndentingNewLine]", 
          RowBox[{"NIntegrate", "[", 
           RowBox[{
            RowBox[{"Sqrt", "[", 
             RowBox[{"1", "+", 
              RowBox[{
               RowBox[{"(", 
                RowBox[{
                 RowBox[{"catf", "'"}], "[", "x", "]"}], ")"}], "^", "2"}]}], 
             "]"}], ",", 
            RowBox[{"{", 
             RowBox[{"x", ",", 
              RowBox[{"pts", "[", 
               RowBox[{"[", "1", "]"}], "]"}], ",", 
              RowBox[{"pts", "[", 
               RowBox[{"[", "2", "]"}], "]"}]}], "}"}]}], "]"}]}]}], 
        "\[IndentingNewLine]", "]"}]}], ",", "\[IndentingNewLine]", 
      RowBox[{
       RowBox[{"GetConst4", "[", "fp_", "]"}], ":=", 
       RowBox[{"ct", "/.", 
        RowBox[{"FindFit", "[", 
         RowBox[{"fp", ",", 
          RowBox[{
           RowBox[{"ct", " ", 
            RowBox[{"Cosh", "[", 
             RowBox[{"x", "/", "ct"}], "]"}]}], "-", "ct"}], " ", ",", 
          RowBox[{"{", "ct", "}"}], ",", "x", ",", 
          RowBox[{"MaxIterations", "\[Rule]", " ", "100000000000000000"}]}], 
         "]"}]}]}], ",", "\[IndentingNewLine]", 
      RowBox[{
       RowBox[{"GetX", "[", 
        RowBox[{"a_", ",", "ramb_", ",", "pc_"}], "]"}], ":=", 
       RowBox[{"ct", "/.", 
        RowBox[{"FindRoot", "[", 
         RowBox[{
          RowBox[{
           RowBox[{
            RowBox[{"a", " ", 
             RowBox[{"Cosh", "[", 
              RowBox[{"ct", "/", "a"}], "]"}]}], "  ", "-", "a"}], "\[Equal]",
            "ramb"}], ",", 
          RowBox[{"{", 
           RowBox[{"ct", ",", "pc"}], "}"}], ",", 
          RowBox[{"MaxIterations", "\[Rule]", " ", "100000000000000000"}]}], 
         "]"}]}]}], ",", "\[IndentingNewLine]", 
      RowBox[{
       RowBox[{"AmbdiametLengthR", "[", "cl_", "]"}], ":=", "cl"}], ",", 
      "\[IndentingNewLine]", 
      RowBox[{
       RowBox[{"GetCatenaryCurve", "[", 
        RowBox[{"pc_", ",", "cl_", ",", "ap_", ",", "p_"}], "]"}], ":=", 
       RowBox[{"Module", "[", 
        RowBox[{
         RowBox[{"{", 
          RowBox[{
          "ramb", ",", "a", ",", "hOfC", ",", "apL", ",", "pL", " ", ",", 
           "datas"}], "}"}], ",", "\[IndentingNewLine]", 
         RowBox[{
          RowBox[{"ramb", "=", 
           RowBox[{
            RowBox[{"AmbdiametLengthR", "[", 
             RowBox[{"(", 
              RowBox[{"cl", "-", "ap", "-", "p"}], ")"}], "]"}], "/", "2"}]}],
           ";", "\[IndentingNewLine]", 
          RowBox[{"hOfC", "=", 
           RowBox[{
            RowBox[{"(", 
             RowBox[{"cl", "-", "ap", "-", "p"}], ")"}], "*", 
            RowBox[{"pc", "/", "2"}]}]}], ";", "\[IndentingNewLine]", 
          RowBox[{"a", "=", 
           RowBox[{"GetConst4", "[", 
            RowBox[{"{", 
             RowBox[{
              RowBox[{"{", 
               RowBox[{"hOfC", ",", "ramb"}], "}"}], ",", 
              RowBox[{"{", 
               RowBox[{
                RowBox[{"-", "hOfC"}], ",", "ramb"}], "}"}], ",", 
              RowBox[{"{", 
               RowBox[{"0", ",", "0"}], "}"}]}], "}"}], "]"}]}], ";", 
          "\[IndentingNewLine]", 
          RowBox[{"apL", "=", 
           RowBox[{"GetX", "[", 
            RowBox[{"a", ",", 
             RowBox[{"(", 
              RowBox[{"ramb", "-", "ap"}], ")"}], ",", "0.001"}], "]"}]}], 
          ";", "\[IndentingNewLine]", 
          RowBox[{"pL", "=", 
           RowBox[{"GetX", "[", 
            RowBox[{"a", ",", 
             RowBox[{"(", 
              RowBox[{"ramb", "-", "p"}], ")"}], ",", 
             RowBox[{"-", "cl"}]}], "]"}]}], ";", "\[IndentingNewLine]", 
          RowBox[{"{", 
           RowBox[{"a", ",", "ramb", ",", 
            RowBox[{"EuclideanDistance", "[", 
             RowBox[{
              RowBox[{"{", 
               RowBox[{"apL", ",", "0"}], "}"}], ",", 
              RowBox[{"{", 
               RowBox[{"pL", ",", "0"}], "}"}]}], "]"}], ",", 
            RowBox[{"CatLength", "[", 
             RowBox[{
              RowBox[{"{", 
               RowBox[{"pL", ",", "apL"}], "}"}], ",", "a"}], "]"}], ",", 
            RowBox[{"Sort", "[", 
             RowBox[{"{", 
              RowBox[{"pL", ",", "apL"}], "}"}], "]"}], ",", 
            RowBox[{"Sort", "[", 
             RowBox[{"{", 
              RowBox[{
               RowBox[{"(", 
                RowBox[{"ramb", "-", "ap"}], ")"}], ",", 
               RowBox[{"(", 
                RowBox[{"ramb", "-", "p"}], ")"}]}], "}"}], "]"}], ",", 
            RowBox[{"{", 
             RowBox[{
              RowBox[{"{", 
               RowBox[{
                RowBox[{"-", "hOfC"}], ",", "ramb"}], "}"}], ",", 
              RowBox[{"{", 
               RowBox[{"0", ",", "0"}], "}"}], ",", 
              RowBox[{"{", 
               RowBox[{"hOfC", ",", "ramb"}], "}"}]}], "}"}], ",", 
            RowBox[{"{", 
             RowBox[{
              RowBox[{"{", 
               RowBox[{"apL", ",", 
                RowBox[{"(", 
                 RowBox[{"ramb", "-", "ap"}], ")"}]}], "}"}], ",", 
              RowBox[{"{", 
               RowBox[{"pL", ",", 
                RowBox[{"(", 
                 RowBox[{"ramb", "-", "p"}], ")"}]}], "}"}]}], "}"}], ",", 
            RowBox[{"1", "-", 
             RowBox[{"(", 
              RowBox[{
               RowBox[{"Abs", "[", 
                RowBox[{"(", 
                 RowBox[{
                  RowBox[{"CatLength", "[", 
                   RowBox[{
                    RowBox[{"{", 
                    RowBox[{"pL", ",", "apL"}], "}"}], ",", "a"}], "]"}], "-", 
                  RowBox[{"(", 
                   RowBox[{"cl", "-", "ap", "-", "p"}], ")"}]}], ")"}], "]"}],
                "/", 
               RowBox[{"(", 
                RowBox[{"cl", "-", "ap", "-", "p"}], ")"}]}], ")"}]}], ",", 
            RowBox[{"CatLength", "[", 
             RowBox[{
              RowBox[{"{", 
               RowBox[{"pL", ",", "apL"}], "}"}], ",", "a"}], "]"}], ",", 
            RowBox[{"(", 
             RowBox[{"cl", "-", "ap", "-", "p"}], ")"}], ",", "pc", ",", 
            RowBox[{"EuclideanDistance", "[", 
             RowBox[{
              RowBox[{"{", 
               RowBox[{"apL", ",", "0"}], "}"}], ",", 
              RowBox[{"{", 
               RowBox[{"pL", ",", "0"}], "}"}]}], "]"}]}], "}"}]}]}], 
        "\[IndentingNewLine]", "]"}]}], ",", "\[IndentingNewLine]", 
      RowBox[{
       RowBox[{"GetCatenaryCurve4", "[", 
        RowBox[{"pc_", ",", "cl_", ",", "ap_", ",", "p_"}], "]"}], ":=", 
       RowBox[{"Module", "[", 
        RowBox[{
         RowBox[{"{", 
          RowBox[{
          "ramb", ",", "a", ",", "hOfC", ",", "apL", ",", "pL", " ", ",", 
           "datas"}], "}"}], ",", "\[IndentingNewLine]", 
         RowBox[{
          RowBox[{"ramb", "=", 
           RowBox[{
            RowBox[{"AmbdiametLengthR", "[", 
             RowBox[{"(", "cl", ")"}], "]"}], "/", "2"}]}], ";", 
          "\[IndentingNewLine]", 
          RowBox[{"hOfC", "=", 
           RowBox[{
            RowBox[{"(", "cl", ")"}], "*", 
            RowBox[{"pc", "/", "2"}]}]}], ";", "\[IndentingNewLine]", 
          RowBox[{"a", "=", 
           RowBox[{"GetConst4", "[", 
            RowBox[{"{", 
             RowBox[{
              RowBox[{"{", 
               RowBox[{"hOfC", ",", "ramb"}], "}"}], ",", 
              RowBox[{"{", 
               RowBox[{
                RowBox[{"-", "hOfC"}], ",", "ramb"}], "}"}], ",", 
              RowBox[{"{", 
               RowBox[{"0", ",", "0"}], "}"}]}], "}"}], "]"}]}], ";", 
          "\[IndentingNewLine]", 
          RowBox[{"apL", "=", 
           RowBox[{"GetX", "[", 
            RowBox[{"a", ",", 
             RowBox[{"(", 
              RowBox[{"ramb", "-", "ap"}], ")"}], ",", "0.001"}], "]"}]}], 
          ";", "\[IndentingNewLine]", 
          RowBox[{"pL", "=", 
           RowBox[{"GetX", "[", 
            RowBox[{"a", ",", 
             RowBox[{"(", 
              RowBox[{"ramb", "-", "p"}], ")"}], ",", 
             RowBox[{"-", "cl"}]}], "]"}]}], ";", "\[IndentingNewLine]", 
          RowBox[{"{", 
           RowBox[{"a", ",", "ramb", ",", 
            RowBox[{"EuclideanDistance", "[", 
             RowBox[{
              RowBox[{"{", 
               RowBox[{"apL", ",", "0"}], "}"}], ",", 
              RowBox[{"{", 
               RowBox[{"pL", ",", "0"}], "}"}]}], "]"}], ",", 
            RowBox[{"CatLength", "[", 
             RowBox[{
              RowBox[{"{", 
               RowBox[{"pL", ",", "apL"}], "}"}], ",", "a"}], "]"}], ",", 
            RowBox[{"Sort", "[", 
             RowBox[{"{", 
              RowBox[{"pL", ",", "apL"}], "}"}], "]"}], ",", 
            RowBox[{"Sort", "[", 
             RowBox[{"{", 
              RowBox[{
               RowBox[{"(", 
                RowBox[{"ramb", "-", "ap"}], ")"}], ",", 
               RowBox[{"(", 
                RowBox[{"ramb", "-", "p"}], ")"}]}], "}"}], "]"}], ",", 
            RowBox[{"{", 
             RowBox[{
              RowBox[{"{", 
               RowBox[{
                RowBox[{"-", "hOfC"}], ",", "ramb"}], "}"}], ",", 
              RowBox[{"{", 
               RowBox[{"0", ",", "0"}], "}"}], ",", 
              RowBox[{"{", 
               RowBox[{"hOfC", ",", "ramb"}], "}"}]}], "}"}], ",", 
            RowBox[{"{", 
             RowBox[{
              RowBox[{"{", 
               RowBox[{"apL", ",", 
                RowBox[{"(", 
                 RowBox[{"ramb", "-", "ap"}], ")"}]}], "}"}], ",", 
              RowBox[{"{", 
               RowBox[{"pL", ",", 
                RowBox[{"(", 
                 RowBox[{"ramb", "-", "p"}], ")"}]}], "}"}]}], "}"}], ",", 
            RowBox[{"1", "-", 
             RowBox[{"(", 
              RowBox[{
               RowBox[{"Abs", "[", 
                RowBox[{"(", 
                 RowBox[{
                  RowBox[{"CatLength", "[", 
                   RowBox[{
                    RowBox[{"{", 
                    RowBox[{"pL", ",", "apL"}], "}"}], ",", "a"}], "]"}], "-", 
                  RowBox[{"(", "cl", ")"}]}], ")"}], "]"}], "/", 
               RowBox[{"(", "cl", ")"}]}], ")"}]}], ",", 
            RowBox[{"CatLength", "[", 
             RowBox[{
              RowBox[{"{", 
               RowBox[{"pL", ",", "apL"}], "}"}], ",", "a"}], "]"}], ",", 
            RowBox[{"(", "cl", ")"}], ",", "pc", ",", 
            RowBox[{"EuclideanDistance", "[", 
             RowBox[{
              RowBox[{"{", 
               RowBox[{"apL", ",", "0"}], "}"}], ",", 
              RowBox[{"{", 
               RowBox[{"pL", ",", "0"}], "}"}]}], "]"}]}], "}"}]}]}], 
        "\[IndentingNewLine]", "]"}]}], ",", "\[IndentingNewLine]", 
      RowBox[{
       RowBox[{"CreateCatCurve3D", "[", "catinfo_", "]"}], ":=", 
       RowBox[{"Module", "[", 
        RowBox[{
         RowBox[{"{", "m", "}"}], ",", "\[IndentingNewLine]", 
         RowBox[{"RevolutionPlot3D", "[", 
          RowBox[{
           RowBox[{"{", 
            RowBox[{
             RowBox[{
              RowBox[{"CatEqn", "[", 
               RowBox[{"m", ",", 
                RowBox[{"catinfo", "[", 
                 RowBox[{"[", "1", "]"}], "]"}]}], "]"}], "-", 
              RowBox[{"catinfo", "[", 
               RowBox[{"[", "2", "]"}], "]"}]}], ",", "m"}], "}"}], ",", 
           RowBox[{"{", 
            RowBox[{"m", ",", 
             RowBox[{"catinfo", "[", 
              RowBox[{"[", 
               RowBox[{"5", ",", "1"}], "]"}], "]"}], ",", 
             RowBox[{"catinfo", "[", 
              RowBox[{"[", 
               RowBox[{"5", ",", "2"}], "]"}], "]"}]}], "}"}], ",", 
           RowBox[{"{", 
            RowBox[{"t", ",", 
             RowBox[{
              RowBox[{"-", "2"}], "Pi"}], ",", 
             RowBox[{"2", "Pi"}]}], "}"}], ",", 
           RowBox[{"Axes", "\[Rule]", "False"}], ",", 
           RowBox[{"Boxed", "\[Rule]", "False"}]}], "]"}]}], 
        "\[IndentingNewLine]", "]"}]}], ",", "\[IndentingNewLine]", 
      RowBox[{
       RowBox[{"CreateCatCurve", "[", "catinfo_", "]"}], ":=", 
       RowBox[{"Module", "[", 
        RowBox[{
         RowBox[{"{", "m", "}"}], ",", "\[IndentingNewLine]", 
         RowBox[{"Show", "[", 
          RowBox[{
           RowBox[{"ListPlot", "[", 
            RowBox[{
             RowBox[{"Table", "[", 
              RowBox[{
               RowBox[{"{", 
                RowBox[{
                 RowBox[{"CatEqn", "[", 
                  RowBox[{"m", ",", 
                   RowBox[{"catinfo", "[", 
                    RowBox[{"[", "1", "]"}], "]"}]}], "]"}], ",", "m"}], 
                "}"}], ",", " ", 
               RowBox[{"{", 
                RowBox[{"m", ",", 
                 RowBox[{"catinfo", "[", 
                  RowBox[{"[", 
                   RowBox[{"5", ",", "1"}], "]"}], "]"}], ",", 
                 RowBox[{"catinfo", "[", 
                  RowBox[{"[", 
                   RowBox[{"5", ",", "2"}], "]"}], "]"}], ",", "0.01"}], 
                "}"}]}], "]"}], ",", 
             RowBox[{"Axes", "\[Rule]", "False"}], ",", 
             RowBox[{"Joined", "\[Rule]", "True"}], ",", 
             RowBox[{"AspectRatio", "\[Rule]", "1"}], ",", 
             RowBox[{"PlotStyle", "->", 
              RowBox[{"{", 
               RowBox[{"Directive", "[", 
                RowBox[{
                 RowBox[{"PointSize", "[", "0.007", "]"}], ",", "Black", ",", 
                 RowBox[{"Thickness", "[", "0.006", "]"}]}], "]"}], "}"}]}], 
             ",", "\[IndentingNewLine]", 
             RowBox[{"AxesStyle", "->", 
              RowBox[{"Directive", "[", 
               RowBox[{"Black", ",", 
                RowBox[{"FontSize", "\[Rule]", "38."}], ",", 
                RowBox[{"FontFamily", "\[Rule]", "\"\<Arial\>\""}], ",", " ", 
                RowBox[{"Thickness", "[", "0.0035", "]"}]}], "]"}]}]}], "]"}],
            ",", 
           RowBox[{"Graphics", "[", 
            RowBox[{"{", 
             RowBox[{
              RowBox[{"Line", "[", 
               RowBox[{"{", 
                RowBox[{
                 RowBox[{"{", 
                  RowBox[{
                   RowBox[{"catinfo", "[", 
                    RowBox[{"[", "2", "]"}], "]"}], ",", 
                   RowBox[{
                    RowBox[{"-", 
                    RowBox[{"catinfo", "[", 
                    RowBox[{"[", "3", "]"}], "]"}]}], "/", "2"}]}], "}"}], 
                 ",", 
                 RowBox[{"{", 
                  RowBox[{
                   RowBox[{"catinfo", "[", 
                    RowBox[{"[", "2", "]"}], "]"}], ",", 
                   RowBox[{
                    RowBox[{"catinfo", "[", 
                    RowBox[{"[", "3", "]"}], "]"}], "/", "2"}]}], "}"}]}], 
                "}"}], "]"}], ",", 
              RowBox[{"AspectRatio", "\[Rule]", "1"}]}], "}"}], "]"}]}], 
          "]"}]}], "\[IndentingNewLine]", "]"}]}], ",", "\[IndentingNewLine]", 
      RowBox[{
       RowBox[{"visceralGrowthF", "[", 
        RowBox[{"cl_", ",", " ", "perR_", ",", "apicalSR_"}], "]"}], ":=", 
       RowBox[{"Module", "[", 
        RowBox[{
         RowBox[{"{", 
          RowBox[{
          "fixed", ",", "bcol", ",", "v", ",", "catinfo", ",", "cat", ",", 
           "cat3D", ",", "graphicList", ",", "colv", ",", "temp", ",", 
           "solut"}], "}"}], ",", "\[IndentingNewLine]", 
         RowBox[{
          RowBox[{"v", "=", "0.1"}], ";", "\[IndentingNewLine]", 
          RowBox[{"graphicList", "=", 
           RowBox[{"{", "}"}]}], ";", "\[IndentingNewLine]", 
          RowBox[{"colv", "=", 
           RowBox[{"{", "}"}]}], ";", "\[IndentingNewLine]", 
          RowBox[{"For", "[", 
           RowBox[{
            RowBox[{"v", "=", "0.01"}], ",", 
            RowBox[{"v", "<", "1"}], ",", "\[IndentingNewLine]", 
            RowBox[{"(*", 
             RowBox[{
             "this", " ", "get", " ", "the", " ", "length", " ", "of", " ", 
              "the", " ", "column", " ", "and", " ", "assigns", " ", "a", " ",
               "new", " ", "relative", " ", "peristome"}], "*)"}], 
            "\[IndentingNewLine]", 
            RowBox[{
             RowBox[{"catinfo", "=", 
              RowBox[{"GetCatenaryCurve4", "[", 
               RowBox[{"v", ",", "cl", ",", "apicalSR", ",", "perR"}], 
               "]"}]}], ";", "\[IndentingNewLine]", 
             RowBox[{"AppendTo", "[", 
              RowBox[{"colv", ",", 
               RowBox[{"Join", "[", 
                RowBox[{
                 RowBox[{"{", 
                  RowBox[{"Take", "[", 
                   RowBox[{"catinfo", ",", 
                    RowBox[{"-", "5"}]}], "]"}], "}"}], ",", 
                 RowBox[{"{", "catinfo", "}"}]}], "]"}]}], "]"}], ";", 
             "\[IndentingNewLine]", 
             RowBox[{"v", "=", 
              RowBox[{"v", "+", "0.01"}]}], ";"}]}], "\[IndentingNewLine]", 
           "]"}], ";", "\[IndentingNewLine]", 
          RowBox[{"solut", "=", 
           RowBox[{"Last", "[", 
            RowBox[{"Last", "[", 
             RowBox[{"Sort", "[", "colv", "]"}], "]"}], "]"}]}], ";", 
          "\[IndentingNewLine]", 
          RowBox[{"{", 
           RowBox[{
            RowBox[{"CreateCatCurve", "[", "solut", "]"}], ",", 
            RowBox[{"CreateCatCurve3D", "[", "solut", "]"}]}], "}"}]}]}], 
        "\[IndentingNewLine]", "]"}]}]}], "}"}]}]}], "]"}]], "Input"],

Cell[BoxData[
 TagBox[
  StyleBox[
   DynamicModuleBox[{$CellContext`apicalSR$$ = 1, $CellContext`cl$$ = 
    10, $CellContext`perR$$ = 1, Typeset`show$$ = True, 
    Typeset`bookmarkList$$ = {}, Typeset`bookmarkMode$$ = "Menu", 
    Typeset`animator$$, Typeset`animvar$$ = 1, Typeset`name$$ = 
    "\"untitled\"", Typeset`specs$$ = {{{
       Hold[$CellContext`cl$$], 10, "Column Length"}, 0.1, 100, 0.1}, {{
       Hold[$CellContext`perR$$], 1, "Peristome Radius"}, 0.1, 100, 0.1}, {{
       Hold[$CellContext`apicalSR$$], 1, "Apical System"}, 0.1, 100, 0.1}}, 
    Typeset`size$$ = {360., {92., 96.}}, Typeset`update$$ = 0, 
    Typeset`initDone$$, Typeset`skipInitDone$$ = 
    False, $CellContext`cl$471$$ = 0, $CellContext`perR$476$$ = 
    0, $CellContext`apicalSR$477$$ = 0}, 
    DynamicBox[Manipulate`ManipulateBoxes[
     1, StandardForm, 
      "Variables" :> {$CellContext`apicalSR$$ = 1, $CellContext`cl$$ = 
        10, $CellContext`perR$$ = 1}, "ControllerVariables" :> {
        Hold[$CellContext`cl$$, $CellContext`cl$471$$, 0], 
        Hold[$CellContext`perR$$, $CellContext`perR$476$$, 0], 
        Hold[$CellContext`apicalSR$$, $CellContext`apicalSR$477$$, 0]}, 
      "OtherVariables" :> {
       Typeset`show$$, Typeset`bookmarkList$$, Typeset`bookmarkMode$$, 
        Typeset`animator$$, Typeset`animvar$$, Typeset`name$$, 
        Typeset`specs$$, Typeset`size$$, Typeset`update$$, Typeset`initDone$$,
         Typeset`skipInitDone$$}, "Body" :> GraphicsRow[
        Quiet[
         $CellContext`visceralGrowthF[$CellContext`cl$$, $CellContext`perR$$, \
$CellContext`apicalSR$$]]], 
      "Specifications" :> {{{$CellContext`cl$$, 10, "Column Length"}, 0.1, 
         100, 0.1}, {{$CellContext`perR$$, 1, "Peristome Radius"}, 0.1, 100, 
         0.1}, {{$CellContext`apicalSR$$, 1, "Apical System"}, 0.1, 100, 
         0.1}}, "Options" :> {}, "DefaultOptions" :> {}],
     ImageSizeCache->{405., {164., 169.}},
     SingleEvaluation->True],
    Deinitialization:>None,
    DynamicModuleValues:>{},
    Initialization:>({$CellContext`CatEqn[
         Pattern[$CellContext`x, 
          Blank[]], 
         Pattern[$CellContext`a, 
          Blank[]]] := $CellContext`a 
         Cosh[$CellContext`x/$CellContext`a] - $CellContext`a, \
$CellContext`CatLength[
         Pattern[$CellContext`pts, 
          Blank[]], 
         Pattern[$CellContext`a, 
          Blank[]]] := Module[{$CellContext`x}, $CellContext`catf[
            Pattern[$CellContext`x, 
             Blank[]]] = $CellContext`a 
            Cosh[$CellContext`x/$CellContext`a] - $CellContext`a; NIntegrate[
           Sqrt[
           1 + Derivative[
              1][$CellContext`catf][$CellContext`x]^2], {$CellContext`x, 
            Part[$CellContext`pts, 1], 
            Part[$CellContext`pts, 2]}]], $CellContext`GetConst4[
         Pattern[$CellContext`fp, 
          Blank[]]] := ReplaceAll[$CellContext`ct, 
         FindFit[$CellContext`fp, $CellContext`ct 
           Cosh[$CellContext`x/$CellContext`ct] - $CellContext`ct, \
{$CellContext`ct}, $CellContext`x, MaxIterations -> 
          100000000000000000]], $CellContext`GetX[
         Pattern[$CellContext`a, 
          Blank[]], 
         Pattern[$CellContext`ramb, 
          Blank[]], 
         Pattern[$CellContext`pc, 
          Blank[]]] := ReplaceAll[$CellContext`ct, 
         FindRoot[$CellContext`a 
            Cosh[$CellContext`ct/$CellContext`a] - $CellContext`a == \
$CellContext`ramb, {$CellContext`ct, $CellContext`pc}, MaxIterations -> 
          100000000000000000]], $CellContext`AmbdiametLengthR[
         Pattern[$CellContext`cl, 
          Blank[]]] := $CellContext`cl, $CellContext`GetCatenaryCurve[
         Pattern[$CellContext`pc, 
          Blank[]], 
         Pattern[$CellContext`cl, 
          Blank[]], 
         Pattern[$CellContext`ap, 
          Blank[]], 
         Pattern[$CellContext`p, 
          Blank[]]] := 
       Module[{$CellContext`ramb, $CellContext`a, $CellContext`hOfC, \
$CellContext`apL, $CellContext`pL, $CellContext`datas}, $CellContext`ramb = \
$CellContext`AmbdiametLengthR[$CellContext`cl - $CellContext`ap - \
$CellContext`p]/
           2; $CellContext`hOfC = ($CellContext`cl - $CellContext`ap - \
$CellContext`p) ($CellContext`pc/
            2); $CellContext`a = $CellContext`GetConst4[{{$CellContext`hOfC, \
$CellContext`ramb}, {-$CellContext`hOfC, $CellContext`ramb}, {0, 
             0}}]; $CellContext`apL = $CellContext`GetX[$CellContext`a, \
$CellContext`ramb - $CellContext`ap, 
            0.001]; $CellContext`pL = $CellContext`GetX[$CellContext`a, \
$CellContext`ramb - $CellContext`p, -$CellContext`cl]; {$CellContext`a, \
$CellContext`ramb, 
           EuclideanDistance[{$CellContext`apL, 0}, {$CellContext`pL, 0}], 
           $CellContext`CatLength[{$CellContext`pL, $CellContext`apL}, \
$CellContext`a], 
           Sort[{$CellContext`pL, $CellContext`apL}], 
           
           Sort[{$CellContext`ramb - $CellContext`ap, $CellContext`ramb - \
$CellContext`p}], {{-$CellContext`hOfC, $CellContext`ramb}, {0, 
            0}, {$CellContext`hOfC, $CellContext`ramb}}, {{$CellContext`apL, \
$CellContext`ramb - $CellContext`ap}, {$CellContext`pL, $CellContext`ramb - \
$CellContext`p}}, 1 - 
           Abs[$CellContext`CatLength[{$CellContext`pL, $CellContext`apL}, \
$CellContext`a] - ($CellContext`cl - $CellContext`ap - \
$CellContext`p)]/($CellContext`cl - $CellContext`ap - $CellContext`p), 
           $CellContext`CatLength[{$CellContext`pL, $CellContext`apL}, \
$CellContext`a], $CellContext`cl - $CellContext`ap - $CellContext`p, \
$CellContext`pc, 
           
           EuclideanDistance[{$CellContext`apL, 0}, {$CellContext`pL, 
             0}]}], $CellContext`GetCatenaryCurve4[
         Pattern[$CellContext`pc, 
          Blank[]], 
         Pattern[$CellContext`cl, 
          Blank[]], 
         Pattern[$CellContext`ap, 
          Blank[]], 
         Pattern[$CellContext`p, 
          Blank[]]] := 
       Module[{$CellContext`ramb, $CellContext`a, $CellContext`hOfC, \
$CellContext`apL, $CellContext`pL, $CellContext`datas}, $CellContext`ramb = \
$CellContext`AmbdiametLengthR[$CellContext`cl]/
           2; $CellContext`hOfC = $CellContext`cl ($CellContext`pc/
            2); $CellContext`a = $CellContext`GetConst4[{{$CellContext`hOfC, \
$CellContext`ramb}, {-$CellContext`hOfC, $CellContext`ramb}, {0, 
             0}}]; $CellContext`apL = $CellContext`GetX[$CellContext`a, \
$CellContext`ramb - $CellContext`ap, 
            0.001]; $CellContext`pL = $CellContext`GetX[$CellContext`a, \
$CellContext`ramb - $CellContext`p, -$CellContext`cl]; {$CellContext`a, \
$CellContext`ramb, 
           EuclideanDistance[{$CellContext`apL, 0}, {$CellContext`pL, 0}], 
           $CellContext`CatLength[{$CellContext`pL, $CellContext`apL}, \
$CellContext`a], 
           Sort[{$CellContext`pL, $CellContext`apL}], 
           
           Sort[{$CellContext`ramb - $CellContext`ap, $CellContext`ramb - \
$CellContext`p}], {{-$CellContext`hOfC, $CellContext`ramb}, {0, 
            0}, {$CellContext`hOfC, $CellContext`ramb}}, {{$CellContext`apL, \
$CellContext`ramb - $CellContext`ap}, {$CellContext`pL, $CellContext`ramb - \
$CellContext`p}}, 1 - 
           Abs[$CellContext`CatLength[{$CellContext`pL, $CellContext`apL}, \
$CellContext`a] - $CellContext`cl]/$CellContext`cl, 
           $CellContext`CatLength[{$CellContext`pL, $CellContext`apL}, \
$CellContext`a], $CellContext`cl, $CellContext`pc, 
           
           EuclideanDistance[{$CellContext`apL, 0}, {$CellContext`pL, 
             0}]}], $CellContext`CreateCatCurve3D[
         Pattern[$CellContext`catinfo, 
          Blank[]]] := Module[{$CellContext`m}, 
         RevolutionPlot3D[{$CellContext`CatEqn[$CellContext`m, 
             Part[$CellContext`catinfo, 1]] - 
           Part[$CellContext`catinfo, 2], $CellContext`m}, {$CellContext`m, 
           Part[$CellContext`catinfo, 5, 1], 
           Part[$CellContext`catinfo, 5, 2]}, {$CellContext`t, (-2) Pi, 2 Pi},
           Axes -> False, Boxed -> False]], $CellContext`CreateCatCurve[
         Pattern[$CellContext`catinfo, 
          Blank[]]] := Module[{$CellContext`m}, 
         Show[
          ListPlot[
           Table[{
             $CellContext`CatEqn[$CellContext`m, 
              
              Part[$CellContext`catinfo, 
               1]], $CellContext`m}, {$CellContext`m, 
             Part[$CellContext`catinfo, 5, 1], 
             Part[$CellContext`catinfo, 5, 2], 0.01}], Axes -> False, Joined -> 
           True, AspectRatio -> 1, PlotStyle -> {
             Directive[
              PointSize[0.007], Black, 
              Thickness[0.006]]}, AxesStyle -> 
           Directive[Black, FontSize -> 38., FontFamily -> "Arial", 
             Thickness[0.0035]]], 
          Graphics[{
            Line[{{
               
               Part[$CellContext`catinfo, 2], (-
                Part[$CellContext`catinfo, 3])/2}, {
               Part[$CellContext`catinfo, 2], Part[$CellContext`catinfo, 3]/
               2}}], AspectRatio -> 1}]]], $CellContext`visceralGrowthF[
         Pattern[$CellContext`cl, 
          Blank[]], 
         Pattern[$CellContext`perR, 
          Blank[]], 
         Pattern[$CellContext`apicalSR, 
          Blank[]]] := 
       Module[{$CellContext`fixed, $CellContext`bcol, $CellContext`v, \
$CellContext`catinfo, $CellContext`cat, $CellContext`cat3D, \
$CellContext`graphicList, $CellContext`colv, $CellContext`temp, \
$CellContext`solut}, $CellContext`v = 
          0.1; $CellContext`graphicList = {}; $CellContext`colv = {}; 
         For[$CellContext`v = 0.01, $CellContext`v < 
           1, $CellContext`catinfo = \
$CellContext`GetCatenaryCurve4[$CellContext`v, $CellContext`cl, \
$CellContext`apicalSR, $CellContext`perR]; AppendTo[$CellContext`colv, 
             Join[{
               
               Take[$CellContext`catinfo, -5]}, {$CellContext`catinfo}]]; \
$CellContext`v = $CellContext`v + 0.01; Null]; $CellContext`solut = Last[
            Last[
             Sort[$CellContext`colv]]]; {
           $CellContext`CreateCatCurve[$CellContext`solut], 
           $CellContext`CreateCatCurve3D[$CellContext`solut]}]}; 
     Typeset`initDone$$ = True),
    SynchronousInitialization->True,
    UnsavedVariables:>{Typeset`initDone$$},
    UntrackedVariables:>{Typeset`size$$}], "Manipulate",
   Deployed->True,
   StripOnInput->False],
  Manipulate`InterpretManipulate[1]]], "Output"]
}, Open  ]]
},
WindowSize->{1253, 896},
Visible->True,
ScrollingOptions->{"VerticalScrollRange"->Fit},
ShowCellBracket->Automatic,
CellContext->Notebook,
TrackCellChangeTimes->False,
FrontEndVersion->"10.0 for Mac OS X x86 (32-bit, 64-bit Kernel) (June 27, \
2014)",
StyleDefinitions->"Default.nb"
]
(* End of Notebook Content *)

(* Internal cache information *)
(*CellTagsOutline
CellTagsIndex->{}
*)
(*CellTagsIndex
CellTagsIndex->{}
*)
(*NotebookFileOutline
Notebook[{
Cell[CellGroupData[{
Cell[1486, 35, 19003, 472, 913, "Input"],
Cell[20492, 509, 10431, 212, 350, "Output"]
}, Open  ]]
}
]
*)

(* End of internal cache information *)

(* NotebookSignature ax0yAWBLxxKXmAg4Wwt4arwy *)
