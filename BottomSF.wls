#!/usr/bin/env wolframscript
(* ::Package:: *)

(*Globals*)
sizeToChar = <|200 -> "\|01fac2", 50 -> "\|01f496", 10 -> "\:2728", 5 -> "\|01f97a", 1 -> ",", 0 -> "\:2764\:fe0f"|>;
byteSeparator = "\|01f449\|01f448";

(*Encoder*)
encode[inString_]:=Block[{inputStr, sizeList, currentVal, convert, target},
	inputStr = ToCharacterCode[inString, "UTF-8"];
	convert[char_]:=(
		sizeList = {};
		currentVal = char;
		target := First@Select[Keys@sizeToChar, currentVal >= #&];
		If[target == 0,
			AppendTo[sizeList, 0],
			While[target != 0,
				AppendTo[sizeList, target];
				currentVal -= target;
			]
		];
		Return@StringJoin[sizeToChar /@ sizeList, byteSeparator];
	);
	Return@StringJoin[convert /@ inputStr]
];

(*Decoder*)
decode[encodedString_]:=Block[{charToSize,countInstances,splitString},
	charToSize = Association[Rule @@@ Reverse /@ KeyValueMap[List, sizeToChar]];
	countInstances[char_]:=(StringCount[char,#]*charToSize@#&) /@ Keys@charToSize;
	splitString = StringSplit[encodedString,byteSeparator];
	Return@FromCharacterCode[Total /@ countInstances /@ splitString,"UTF-8"]
];

(*Output writer*)
writeOutput[string_,filename_]:=Block[{file},
	file = OpenWrite[filename,CharacterEncoding -> "UTF-8"];
	WriteString[file,string];
	Close@file;
]

printUsage[]:=Print["Usage: wolframscript "<>$ScriptCommandLine[[1]]<>" [--bottomify/--regress] input_file output_file"];

main[]:=Block[{file},
	If[Length@$ScriptCommandLine < 4,
		printUsage[];
		Return[];
	];
	Switch[$ScriptCommandLine[[2]],
	"--bottomify",
	file = Import[$ScriptCommandLine[[3]],"Text",CharacterEncoding -> "UTF-8"];
	If[!FailureQ@file,
		writeOutput[encode@file,$ScriptCommandLine[[4]]],
		Print@"Failed to import input file."
	];,
	"--regress",
	file = Import[$ScriptCommandLine[[3]],"Text",CharacterEncoding -> "UTF-8"];
	If[!FailureQ@file,
		writeOutput[decode@file,$ScriptCommandLine[[4]]],
		Print@"Failed to import input file."
	];,
	_,
	printUsage[];
	]
];
main[];
