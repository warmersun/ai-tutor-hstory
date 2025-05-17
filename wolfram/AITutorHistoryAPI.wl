(* ::Package:: *)

(* ::Section:: *)
(*init*)


(* ::Subsection:: *)
(*toCamelCase*)


(* ::Text:: *)
(*from https://resources.wolframcloud.com/FunctionRepository/resources/ToCamelCase/*)


ToCamelCase[""] := ""
 
ToCamelCase[s_String] := First[ToCamelCase[{s}]]
 
ToCamelCase[s:{__String}] := With[{words = StringSplit[StringTrim[s]]}, (StringJoin[Decapitalize[First[#1, ""]], Capitalize /@ Replace[#1, x:{__} :> Rest[x]]] & ) /@ words]
 
ToCamelCase[x_] := (Message[ToCamelCase::invld, x]; $Failed)

ToCamelCase::invld = "Encountered non-string argument: ``";


(* ::Subsection:: *)
(*my helpers*)


entityValueStartDate[e_Entity]:=
If[MissingQ[EntityValue[e,"StartDate"]],InfinitePast,EntityValue[e,"StartDate"]]


entityValueEndDate[e_Entity]:=
If[MissingQ[EntityValue[e,"EndDate"]],InfiniteFuture,EntityValue[e,"EndDate"]]


interpreterDate[d_String]:=Module[{int},
int =Interpreter["Integer"][d];
If[IntegerQ[int],
	If[int<0,
		Interpreter["Date"][IntegerString[Abs[int]]<>" BC"],
		Interpreter["Date"][IntegerString[Abs[int]]<>" AD"]],
	Interpreter["Date"][d]]
]


toEntityMilitaryConflictString[militaryConflictStr_String]:=
Module[{mc,militaryConflict, freeformResult, isEntity, entityType, isMilitaryConflict},
mc=TextCases[militaryConflictStr,"MilitaryConflict"->"Interpretation",VerifyInterpretation->True];
If[Length[mc]==0,
(* TextCases didn't find anything, try Entity *)
militaryConflict= Entity["MilitaryConflict",Capitalize[ToCamelCase[StringReplace[RemoveDiacritics[militaryConflictStr],{"-"->" ", "\[Dash]"->" "}]]]],
(* TextCases found something *)
militaryConflict=Entity["MilitaryConflict",Capitalize[ToCamelCase[mc[[1]]]]]
];
If[MissingQ[EntityValue[militaryConflict,"Name"]],
(* Vrification failed, try again... *)
militaryConflict=Entity["MilitaryConflict",Capitalize[ToCamelCase[StringReplace[RemoveDiacritics[militaryConflictStr],{"-"->" ", "\[Dash]"->" "}]]]];
If[MissingQ[EntityValue[militaryConflict,"Name"]],
(* Verification failed; try FreeformEvaluate *)
(*Step 1:Use FreeformEvaluate[] to get the result*)
freeformResult=FreeformEvaluate[militaryConflictStr];
(*Step 2:Check if the result is an Entity*)
isEntity=Head[freeformResult]===Entity;
(*Step 3:If it is an Entity,check the EntityType*) 
If[isEntity,entityType=EntityType[freeformResult],entityType=None];
(*Step 4:Check if the EntityType is "MilitaryConflict"*)
isMilitaryConflict=entityType==="MilitaryConflict";
(*If it's MilitaryConflict then good, otherwise return Missing*)
If[isMilitaryConflict, Return[freeformResult], Return[Missing["Unknown"]]]
,
(* verified *) 
Return[militaryConflict]
]
,
(* verified *)
Return[militaryConflict]
]
]


toEntityHistoricalCountryString[historicalCountryStr_String]:= Module[{historicalCountry},
historicalCountry = Interpreter["HistoricalCountry"][historicalCountryStr];
If[FailureQ[historicalCountry],
Entity["HistoricalCountry",Capitalize@ToCamelCase@historicalCountryStr],
historicalCountry]
]


(* ::Section::Closed:: *)
(*util*)


clearEntityValueCache = APIFunction[{}, Module[{},Internal`ClearEntityValueCache[]]&]


(* ::Subsection:: *)
(*deploy*)


coClearEntityValueCache = CloudDeploy[clearEntityValueCache, "clearEntityValueCache", Permissions->{PermissionsKey[PERMISSIONKEY]->"Execute"}]


(* ::Section::Closed:: *)
(*getHistoricalCountriesByCurrentCountryAndYear*)


getHistoricalCountriesByCurrentCountryAndYear = 
APIFunction[{"currentCountry"->"Country", "year"->"String"}, 
TimeConstrained[
With[{dateObject = DateInterval[DateObject[interpreterDate[#year],"Year"]]},
ExportString[
EntityValue[
Part[#,1]&/@
Check[
GeoIdentify[Dated["HistoricalCountry", dateObject], #currentCountry],
GeoIdentify[Dated["HistoricalCountry", interpreterDate[#year]], #currentCountry],
GeoIdentify::timeout
],
"CanonicalName"],
"RawJSON"]],
28, Failure["TimeConstrained",<|"MessageTemplate"->"Request timed out"|>]]&,
{"String","JSON","JSON"}]


(* ::Subsubsection:: *)
(*test*)


getHistoricalCountriesByCurrentCountryAndYear[<|"currentCountry"->"Romania", "year"->"200 BC"|>]


getHistoricalCountriesByCurrentCountryAndYear[<|"currentCountry"->"China", "year"->"2023"|>]


getHistoricalCountriesByCurrentCountryAndYear[<|"currentCountry"->"Hungary", "year"->"1526"|>]


getHistoricalCountriesByCurrentCountryAndYear[<|"currentCountry"->"Mexico", "year"->"900"|>]


getHistoricalCountriesByCurrentCountryAndYear[<|"currentCountry"->"Canada", "year"->"1867"|>]


(* ::Subsection:: *)
(*deploy*)


coGetHistoricalCountriesByCurrentCountryAndYear = CloudDeploy[getHistoricalCountriesByCurrentCountryAndYear, "getHistoricalCountriesByCurrentCountryAndYear", Permissions->{PermissionsKey[PERMISSIONKEY]->"Execute"} ]


(* ::Section::Closed:: *)
(*getHistoricalEvents*)


getHistoricalEvents =
APIFunction[{"year"->"String","yearsBack"->"Integer"->1}, 
	TimeConstrained[
		Module[{historicalEvents},
			With[{yearDateObject=interpreterDate[#year]},
				historicalEvents = Normal[EntityValue[EntityClass["HistoricalEvent",
					{"StartDate"-> LessEqualThan[DateObject[yearDateObject,"Year"]], "EndDate"->GreaterEqualThan[DatePlus[yearDateObject,Quantity[-1*#yearsBack,"Years"]]]}],
				"CountriesInvolved","NonMissingEntityAssociation"]];
				ExportString[FindGraphCommunities[Graph[Flatten[Table[Rule[#,Keys[historicalEvents][[i]]]&/@Values[historicalEvents][[i]], {i, Length[historicalEvents]}]/. Entity[type_,name_] -> EntityValue[Entity[type,name],"Name"]]]],"RawJSON"]
	]],28,Failure["TimeConstrained",<|"MessageTemplate"->"Request timed out"|>]]&, {"String", "JSON", "JSON"}]


(* ::Subsubsection:: *)
(*test*)


getHistoricalEvents[<|"year"->"1545", "yearsBack"->5|>]


getHistoricalEvents[<|"year"->"68"|>]


(* ::Subsection:: *)
(*deploy*)


coGetHistoricalEvents = CloudDeploy[getHistoricalEvents, "getHistoricalEvents", Permissions->{PermissionsKey[PERMISSIONKEY]->"Execute"}]


(* ::Section::Closed:: *)
(*getMilitaryConflicts*)


getMilitaryConflicts = APIFunction[{"year"->"String", "yearsBack"->"Integer"->1},
TimeConstrained[Module[{wars},
With[{yearDateObject=interpreterDate[#year]},
wars = Normal[
Flatten/@ Join[
EntityValue[
FilteredEntityClass[EntityClass["MilitaryConflict",{"Type","war"}],
EntityFunction[war,DateObject[war["StartDate"],"Year"]<=DateObject[yearDateObject,"Year"] && DateObject[war["EndDate"],"Year"]>=DatePlus[yearDateObject,Quantity[-1*#yearsBack,"Years"]]]],
"MainActors","NonMissingEntityAssociation"],
EntityValue[
FilteredEntityClass[EntityClass["MilitaryConflict",{"Type","revolution"}],EntityFunction[war,DateObject[war["StartDate"],"Year"]<=DateObject[yearDateObject,"Year"] && DateObject[war["EndDate"],"Year"]>=DatePlus[yearDateObject,Quantity[-1*#yearsBack,"Years"]]]],
				"MainActors","NonMissingEntityAssociation"]
]
		];
		ExportString[
			FindGraphCommunities[Graph[Flatten[Table[Rule[#,Keys[wars][[i]]]&/@Values[wars][[i]], {i, Length[wars]}]/. Entity[type_,name_] -> EntityValue[Entity[type,name],"Name"]]]],
			"RawJSON", "ConversionRules"->{ Missing[_]:>"" }]
	]],28,Failure["TimeConstrained",<|"MessageTemplate"->"Request timed out"|>]]&, {"String", "JSON", "JSON"}]


(* ::Subsubsection:: *)
(*test*)


getMilitaryConflicts[<|"year"->"896"|>]


getMilitaryConflicts[<|"year"->"896", "yearsBack"->100|>]


getMilitaryConflicts[<|"year"->"1917"|>]


(* ::Subsection:: *)
(*deploy*)


coGetMilitaryConflicts = CloudDeploy[getMilitaryConflicts, "getMilitaryConflicts", Permissions->{PermissionsKey[PERMISSIONKEY]->"Execute"}]


(* ::Section::Closed:: *)
(*SYNC/ASYNC timelineOfHistoricalPeriodsAndNotablePeople*)


timelineHistoricalPeriodsAndNotablePeopleSync = APIFunction[{"year"->"String"}, 
	TimeConstrained[
		Module[{historicalPeriods, p},
			With[{yearDateObject=interpreterDate[#year],di=DateInterval[interpreterDate[#year],"Year"]},
				historicalPeriods = EntityList[EntityClass["HistoricalPeriod",{"StartDate"-> LessEqualThan[DateObject[interpreterDate[#year],"Year"]], "EndDate"->GreaterEqualThan[DateObject[yearDateObject,"Year"]]}]];
				p = (*Quiet[*)
					Select[
					EntityValue[Cases[Flatten[EntityValue[historicalPeriods,"PeopleInvolved"]],_Entity],{"Name","BirthDate","DeathDate"}],DateOverlapsQ[di,DateInterval[{Part[#1,2],Part[#1,3]},"Year"]]&];(*,{CalendarConvert::notgran}]*)
				ExportString[
					<|"assistantNotesAboutTheImage" ->"The image is being generated in the background asynchronously and it will be displayed when ready.", "historicalPeriods"-> EntityValue[historicalPeriods,"Name"], "notablePeople"->Flatten[Extract[p,{All,1}]]|>,
				"RawJSON" ,"ConversionFunction"-> (DateString[#,{"ISODateTime", "ISOTimeZone"}]&)]]],
	28,Failure["TimeConstrained",<|"MessageTemplate"->"Request timed out"|>]]&, {"String", "JSON", "JSON"}]


timelineHistoricalPeriodsAndNotablePeopleAsync = APIFunction[{"year"->"String"}, 
	TimeConstrained[
		Module[{historicalPeriods, timeline, p},
			With[{yearDateObject=interpreterDate[#year],di=DateInterval[interpreterDate[#year],"Year"]},
				historicalPeriods = EntityList[EntityClass["HistoricalPeriod",{"StartDate"-> LessEqualThan[DateObject[interpreterDate[#year],"Year"]], "EndDate"->GreaterEqualThan[DateObject[yearDateObject,"Year"]]}]];
				timeline = Rasterize[
					TimelinePlot[{historicalPeriods,{Labeled[DateObject[yearDateObject,"Year"],#year]}},PlotTheme->"Web", PlotLabel->"Timeline of historical periods for the year " <> ToString[DateValue[yearDateObject,"Year"]]],
					ImageResolution->300];
				ExportString[
					<|"image"->CloudExport[timeline,"PNG",Permissions->"Public"][[1]]|>,
				"RawJSON"]]],
	28,Failure["TimeConstrained",<|"MessageTemplate"->"Request timed out"|>]]&, {"String", "JSON", "JSON"}]


(* ::Subsection:: *)
(*test*)


timelineHistoricalPeriodsAndNotablePeopleSync[<|"year"->"1700"|>]


timelineHistoricalPeriodsAndNotablePeopleSync[<|"year"->"abc1800"|>]


str = timelineHistoricalPeriodsAndNotablePeopleAsync[<|"year"->"1800"|>];
parsed = ImportString[str, "JSON"];
CloudImport["image"/. parsed]


(* ::Subsection:: *)
(*deploy*)


coTimelineHistoricalPeriodsAndNotablePeopleSync = CloudDeploy[timelineHistoricalPeriodsAndNotablePeopleSync, "timelineHistoricalPeriodsAndNotablePeopleSync", Permissions->{PermissionsKey[PERMISSIONKEY]->"Execute"}]


coTimelineHistoricalPeriodsAndNotablePeopleAsync = CloudDeploy[timelineHistoricalPeriodsAndNotablePeopleAsync, "timelineHistoricalPeriodsAndNotablePeopleAsync", Permissions->{PermissionsKey[PERMISSIONKEY]->"Execute"}]


(* ::Section::Closed:: *)
(*getWarsByHistoricalCountryAndYear*)


getWarsByHistoricalCountryAndYear = APIFunction[{"historicalCountry"->"String" , "year"->"String"}, 
	TimeConstrained[
		With[{yearDate = interpreterDate[#year], hc = toEntityHistoricalCountryString[#historicalCountry]},
			If[MissingQ[EntityValue[hc,"Name"]],Return[Failure["InvalidParameter",<|"MessageTemplate"->"The historicalCountry input parameter is not valid. `historicalCountry` is not the proper way to call it.", "MessageParameters"-> <|"historicalCountry" -> #historicalCountry |> |>]],Nothing];
			ExportString[Extract[Select[
EntityValue[EntityClass["MilitaryConflict",{"StartDate"->yearDate}],{"CanonicalName","StartDate","EndDate","MainActors"},"PropertyAssociation"],
MemberQ[Flatten[Lookup[#,"MainActors"]],hc]&
],{All, {1,2,3}}],"RawJSON", "ConversionFunction"-> (DateString[#,{"ISODateTime", "ISOTimeZone"}]&), "ConversionRules"->{ 
Missing[_]:>""}]]
	,28,Failure["TimeConstrained",<|"MessageTemplate"->"Request timed out"|>]]&, {"String", "JSON", "JSON"}]


(* ::Subsection:: *)
(*deploy*)


coGetWarsByHistoricalCountryAndYear = CloudDeploy[getWarsByHistoricalCountryAndYear, "getWarsByHistoricalCountryAndYear", Permissions->{PermissionsKey[PERMISSIONKEY]->"Execute"}]


(* ::Section::Closed:: *)
(*SYNC/ASYNC mapHistoricalContinent*)


mapHistoricalContinentSync = APIFunction[{"year"->"String","continent"-> Restricted["String",{"Europe","Asia","Africa","NorthAmerica","SouthAmerica", "Australia"}]},
TimeConstrained[Module[{historicalCountries,imageURL},
With[{yearDateObject=interpreterDate[#year]},
historicalCountries = EntityValue[
GeoEntities[GeoBoundsRegion[Entity["GeographicRegion",#continent]], Dated["HistoricalCountry", yearDateObject]],
"Name"];
ExportString[<|"assistantNotesAboutTheImage" ->"The image is being generated in the background asynchronously and it will be displayed when ready.", 
"historicalCountriesDisplayed"-> historicalCountries,"NoteforAssistant"->"Some labels may be hard to read on the map, especially for smaller countries."|>,"RawJSON"]
]],
30,Failure["TimeConstrained",<|"MessageTemplate"->"Request timed out"|>]]&, {"String", "JSON", "JSON"}]


mapHistoricalContinentAsync = APIFunction[{"year"->"String","continent"-> Restricted["String",{"Europe","Asia","Africa","NorthAmerica","SouthAmerica", "Australia"}]},
TimeConstrained[Module[{historicalCountries,imageURL},
With[{yearDateObject=interpreterDate[#year]},
historicalCountries = EntityValue[
GeoEntities[GeoBoundsRegion[Entity["GeographicRegion",#continent]], Dated["HistoricalCountry", yearDateObject]],
{"Name", Dated["Polygon", yearDateObject]}, "PropertyAssociation"];
imageURL = CloudExport[
Rasterize[
GeoListPlot[
	Partition[Labeled[Lookup[#,Dated["Polygon",yearDateObject]],Lookup[#,"Name"]]&/@historicalCountries,1],
	GeoRange->Entity["GeographicRegion",#continent], 
	GeoBackground->Dated["CountryBorders",yearDateObject],
	GeoLabels->True ,LabelStyle->Tiny, ImageSize->Large, PlotLegends->None, GeoScaleBar->{"Imperial","Metric"}],
ImageResolution->300],
"PNG",Permissions->"Public"][[1]];
ExportString[<|"image"->imageURL, "historicalCountriesDisplayed"-> Lookup[historicalCountries,"Name","None"],"NoteforAssistant"->"Some labels may be hard to read on the map, especially for smaller countries."|>,"RawJSON"]
]],
28,Failure["TimeConstrained",<|"MessageTemplate"->"Request timed out"|>]]&, {"String", "JSON", "JSON"}]


(* ::Subsubsection:: *)
(*test*)


mapHistoricalContinentSync[<|"year"->"1230","continent"->"Europe"|>]


mapHistoricalContinentSync[<|"year"->"3000 BC","continent"->"Europe"|>]


str = mapHistoricalContinentAsync[<|"year"->"2000 BC","continent"->"Europe"|>];
parsed = ImportString[str, "JSON"];
CloudImport["image"/.parsed]


str = mapHistoricalContinentAsync[<|"year"->"1867","continent"->"NorthAmerica"|>];
parsed = ImportString[str, "JSON"];
CloudImport["image"/.parsed]


(* ::Subsection:: *)
(*deploy*)


coMapHistoricalContinentSync = CloudDeploy[mapHistoricalContinentSync, "mapHistoricalContinentSync", Permissions->{PermissionsKey[PERMISSIONKEY]->"Execute"}]


coMapHistoricalContinentAsync = CloudDeploy[mapHistoricalContinentAsync, "mapHistoricalContinentAsync", Permissions->{PermissionsKey[PERMISSIONKEY]->"Execute"}]


(* ::Section:: *)
(*SYNC/ASYNC mapHistoricalCountry*)


mapHistoricalCountrySync = 
APIFunction[{
	"historicalCountry"->"String", 
	"year"->"String", 
	"geoRange"->Restricted["String",RegularExpression["(?i)(automatic|continent|world)"]]->"Automatic"
	},
	TimeConstrained[
	Module[{datedPolygon, hc, yearDate},
		hc = toEntityHistoricalCountryString[#historicalCountry];
		If[MissingQ[EntityValue[hc,"Name"]],Return[Failure["InvalidParameter",<|"MessageTemplate"->"The historicalCountry input parameter is not valid. `historicalCountry` is not the proper way to call it.", "MessageParameters" -> <| "historicalCountry" -> #historicalCountry |> |>]],Nothing];
		If[FailureQ[interpreterDate[#year]], If[IntegerQ[ToExpression[#year]],yearDate =DateObject[{ToExpression[#year]}], Return[Nothing]],yearDate = interpreterDate[#year]];
		If[
			DateOverlapsQ[DateObject[yearDate,"Year"],entityValueStartDate[hc]],
			datedPolygon = EntityValue[hc,Dated["Polygon", entityValueStartDate[hc]]],
			If[DateOverlapsQ[DateObject[yearDate,"Year"],entityValueStartDate[hc]],
				datedPolygon = EntityValue[hc,Dated["Polygon", entityValueEndDate[hc]]],
				datedPolygon = EntityValue[hc,Dated["Polygon", DateObject[yearDate,"Year"]]]
			]
		];
	If[MissingQ[datedPolygon],
		(* dated map is Missing, show what we have *)
		If[EntityValue[hc,"HasPolygon"],
With[{range = GeoBoundsRegion[GeoVariant[hc, "UnionArea"]]}, 
With[{datedHistoricalCountries = GeoEntities[range, Dated["HistoricalCountry", yearDate]]},
Return[ExportString[<| 
"assistantNotesAboutTheImage"-> "the image is being generated asynchronously.", 
"notesforAssistant" -> "I don't have the data exactly for the year that you asked for. Instead, the image will show the historical map of the area of " <> CommonName[hc] <> " in " <> DateString[yearDate,"Year"] <> ". The historical countries in the map are: " <> StringRiffle[CanonicalName[Part[#,1]]& /@ datedHistoricalCountries , ", "]
|>,"RawJSON" ,"ConversionFunction"-> (DateString[#,{"ISODateTime", "ISOTimeZone"}]&)]]
]
]
			,
			Return[Failure["NoMapData", <|"MessageTemplate"->"I could not draw the map. I don't have historical map data for `historicalCountry`.", "MessageParameters" -> <|"historicalCountry"-> CommonName[hc]|>|>]]
		]
	,
	(* show dated polygon *)
	Return[ExportString[<|"Note for Assistant"-> "The image is being generated asynchronously.", "NotesForAssistant" -> "The image will show the historical map of " <> CommonName[hc] <> " in " <> DateString[yearDate,"Year"]|>,"RawJSON" ,"ConversionFunction"-> (DateString[#,{"ISODateTime", "ISOTimeZone"}]&)]]
	]],
	10,Failure["TimeConstrained",<|"MessageTemplate"->"Request timed out"|>]]& , {"String", "JSON", "JSON"}]


mapHistoricalCountryAsync = 
APIFunction[{
"historicalCountry"->"String", 
"year"->"String",
"geoRange"->Restricted["String",RegularExpression["(?i)(automatic|continent|world)"]]->"Continent"
},
TimeConstrained[
Module[{datedPolygon, hc, yearDate,continents, interactiveURL, imageURL},
hc = toEntityHistoricalCountryString[#historicalCountry];
If[MissingQ[EntityValue[hc,"Name"]],
Return[Failure["InvalidParameter",<|"MessageTemplate"->"The historicalCountry input parameter is not valid. `historicalCountry` is not the proper way to call it.", "MessageParameters" -> <| "historicalCountry" -> #historicalCountry |> |>]],
Nothing];
If[FailureQ[interpreterDate[#year]], If[IntegerQ[ToExpression[#year]],yearDate =DateObject[{ToExpression[#year]}], Return[Nothing]],yearDate = interpreterDate[#year]];
If[
DateOverlapsQ[DateObject[yearDate,"Year"],entityValueStartDate[hc]],
datedPolygon = EntityValue[hc,Dated["Polygon", entityValueStartDate[hc]]],
If[DateOverlapsQ[DateObject[yearDate,"Year"],entityValueStartDate[hc]],
datedPolygon = EntityValue[hc,Dated["Polygon", entityValueEndDate[hc]]],
datedPolygon = EntityValue[hc,Dated["Polygon", DateObject[yearDate,"Year"]]]
]
];
continents = DeleteDuplicates[EntityValue[hc["CurrentCountries"],"Continent"]];
If[MissingQ[datedPolygon],
(* dated map is Missing, show what we have *)
If[EntityValue[hc,"HasPolygon"],
With[{range = GeoBoundsRegion[GeoVariant[hc, "UnionArea"]]}, 
With[{datedHistoricalCountries = GeoEntities[range, Dated["HistoricalCountry", yearDate]]},
imageURL = CloudExport[
Rasterize[
GeoListPlot[
Partition[Labeled[Dated[Part[#,1], yearDate], CommonName[Part[#,1]], LabelStyle->Tiny]&/@datedHistoricalCountries,1],
GeoBackground->"CountryBorders",
PlotStyle->{Directive[EdgeForm[{Black,Thin}],Opacity[0.7],_]},
GeoRange->range,
PlotLegends->Placed[Style[CommonName[Part[#,1]], Medium]&/@ datedHistoricalCountries, Right],
ImageSize->Large,
GeoScaleBar->{"Imperial","Metric"}
],
ImageResolution->300],
"PNG", Permissions->"Public"][[1]];
Return[ExportString[<|"image"->imageURL|>,"RawJSON" ]]
]
]
,
Return[Failure["NoMapData", <|"MessageTemplate"->"I could not draw the map. I don't have historical map data for `historicalCountry`.", "MessageParameters" -> <|"historicalCountry"-> CommonName[hc]|>|>]]
]
,
(* show dated polygon *)
imageURL = CloudExport[
Rasterize[
If[ToLowerCase@#geoRange == "automatic",
(* automatic *)
	GeoGraphics[
		{GeoStyling["ReliefMap"],EdgeForm[{Black, Thin}], datedPolygon}, 
		GeoBackground->"Coastlines" , 
		ImageSize-> Large, 
		GeoRange->Automatic, 
		PlotLabel->"Historical map of "<> CommonName[hc] <> " in " <> DateString[yearDate,"Year"]
	]
,
(* continent or world *)
	GeoGraphics[{FaceForm[Red],EdgeForm[{Red, Thick}],datedPolygon}, 
		GeoBackground-> Switch[ToLowerCase@#geoRange, "continent", {"Coastlines", "CountryBorders"}, "world","Coastlines"],
		GeoRange->Switch[ToLowerCase@#geoRange, "continent", continents, "world","World"],
		PlotLabel->"Historical map of "<> CommonName[hc] <> " in " <> DateString[yearDate,"Year"],
		GeoScaleBar->{"Imperial","Metric"},
		ImageSize->Large
	]
],
ImageResolution->300],
"PNG",Permissions->"Public"][[1]];
Return[ExportString[<|"image"->imageURL|>,"RawJSON"]]
]],
	28,Failure["TimeConstrained",<|"MessageTemplate"->"Request timed out"|>]]& , {"String", "JSON", "JSON"}]


(* ::Subsubsection:: *)
(*test*)


mapHistoricalCountrySync[<|"historicalCountry"->"Persian Empire", "year"->"1300"|>]


mapHistoricalCountrySync[<|"historicalCountry"->"France", "year"->"1576"|>]


str = mapHistoricalCountryAsync[<|"historicalCountry"->"Persian Empire", "year"->"1300"|>];
parsed = ImportString[str, "JSON"];
CloudImport["image"/.parsed]


str = mapHistoricalCountryAsync[<|"historicalCountry"->"Persian Empire", "year"->"1300", "geoRange"->"Automatic"|>];
parsed = ImportString[str, "JSON"];
CloudImport["image"/.parsed]


mapHistoricalCountrySync[<|"historicalCountry"->"Persian Empire", "year"->"1300", "geoRange"->"Automatic"|>]


str = mapHistoricalCountryAsync[<|"historicalCountry"->"Maya civilization", "year"->"900"|>];
parsed = ImportString[str, "JSON"];
CloudImport["image"/.parsed]


str = mapHistoricalCountryAsync[<|"historicalCountry"->"Maya civilization", "year"->"900", "geoRange"->"Automatic"|>];
parsed = ImportString[str, "JSON"];
CloudImport["image"/.parsed]


str = mapHistoricalCountryAsync[<|"historicalCountry"->"Maya civilization", "year"->"900", "geoRange"->"continent"|>];
parsed = ImportString[str, "JSON"];
CloudImport["image"/.parsed]


str = mapHistoricalCountryAsync[<|"historicalCountry"->"Maya civilization", "year"->"900", "geoRange"->"world"|>];
parsed = ImportString[str, "JSON"];
CloudImport["image"/.parsed]


str = mapHistoricalCountryAsync[<|"historicalCountry"->"Roman Empire", "year"->"60 AD", "geoRange"->"Automatic"|>];
parsed = ImportString[str, "JSON"];
CloudImport["image"/.parsed]


mapHistoricalCountrySync[<|"historicalCountry"->"GRGR Empire", "year"->"1300"|>]


str = mapHistoricalCountryAsync[<|"historicalCountry"->"GRGR Empire", "year"->"1300"|>];
parsed = ImportString[str, "JSON"];
CloudImport["image"/.parsed]


str = mapHistoricalCountryAsync[<|"historicalCountry"->"Mongol Empire", "year"->"1300"|>];
parsed = ImportString[str, "JSON"];
CloudImport["image"/.parsed]


(* ::Subsection:: *)
(*deploy*)


coMapHistoricalCountrySync = CloudDeploy[mapHistoricalCountrySync, "mapHistoricalCountrySync", Permissions->{PermissionsKey[PERMISSIONKEY]->"Execute"}]


coMapHistoricalCountryAsync = CloudDeploy[mapHistoricalCountryAsync, "mapHistoricalCountryAsync", Permissions->{PermissionsKey[PERMISSIONKEY]->"Execute"}]


(* ::Section::Closed:: *)
(*SYNC/ASYNC timelinePerson*)


timelinePersonSync = APIFunction["person"->"Person",
TimeConstrained[
Module[{events,notableThings},
events=EntityList[EntityClass["HistoricalEvent","PeopleInvolved"->#person]];
notableThings = EntityValue[#person,{"NotableArtworks","NotableAstronomicalDiscoveries", "NotableBooks","NotableChemistryProblems","NotableFacts","NotableInventions","NotablePhysicsProblems"},"NonMissingPropertyAssociation"];
ExportString[
<|
"assistantNotesAboutTheImage" ->"The image is being generated in the background asynchronously and it will be displayed when ready.",
"notableThings"->Map[ToString,notableThings,{2}],
"timelineEvents"->(EntityValue[#,{"Name","StarDate","EndDate","Date"},"NonMissingPropertyAssociation"]&/@events)
|>,
"RawJSON" ,
"ConversionFunction"-> (DateString[#,{"ISODateTime", "ISOTimeZone"}]&),
"ConversionRules"->{ Interval[{min_,max_}]:><|"start"->min,"end"->max|>
}]
],5,Failure["TimeConstrained",<|"MessageTemplate"->"Request timed out"|>]]&, {"String", "JSON", "JSON"}]


timelinePersonAsync = APIFunction["person"->"Person",
TimeConstrained[
Module[{events,periods, notableThings, timeline, imageURL},
events=EntityList[EntityClass["HistoricalEvent","PeopleInvolved"->#person]];
periods =EntityList[EntityClass["HistoricalPeriod","PeopleInvolved"->#person]];
With[{bdate=EntityValue[#person,"BirthDate"],ddate = EntityValue[#person,"DeathDate"]},
timeline = TimelinePlot[{events,periods,{Labeled[DateInterval[{bdate,ddate}],DateString[bdate,{"YearUnsigned"," ", "ADBC"}]<> " - " <> DateString[ddate,{"YearUnsigned"," ", "ADBC"}]]}},PlotTheme->"Web", PlotLayout->"Packed", PlotLabel->CommonName[#person]<> " timeline"];
imageURL = CloudExport[Rasterize[timeline, ImageResolution->300], "PNG", Permissions->"Public"][[1]]];
ExportString[<|"image"->imageURL|>,"RawJSON" ]],
28,Failure["TimeConstrained",<|"MessageTemplate"->"Request timed out"|>]]&, {"String", "JSON", "JSON"}]


(* ::Subsubsection:: *)
(*test*)


timelinePersonSync[<|"person"->"Engels"|>]


timelinePersonSync[<|"person"->"Lenin"|>]


str = timelinePersonAsync[<|"person"->"Lenin"|>];
parsed = ImportString[str, "JSON"];
CloudImport["image"/.parsed]


timelinePersonSync[<|"person"->"Lenin neni"|>]


(* ::Subsection:: *)
(*deploy*)


coTimelinePersonSync = CloudDeploy[timelinePersonSync, "timelinePersonSync", Permissions->{PermissionsKey[PERMISSIONKEY]->"Execute"}]


coTimelinePersonAsync = CloudDeploy[timelinePersonAsync, "timelinePersonAsync", Permissions->{PermissionsKey[PERMISSIONKEY]->"Execute"}]


(* ::Section::Closed:: *)
(*SYNC/ASYNC mapWar*)


mapWarSync = APIFunction[{"militaryConflict"->"String", "geoRange"->Restricted["String",RegularExpression["(?i)(battles|actors|world)"]]->"Actors"},
TimeConstrained[
Module[{subconflicts,battles,subconflictInfo,subconflictInfoClean,geobounds=Automatic,militaryConflictEntity},
militaryConflictEntity = toEntityMilitaryConflictString[#militaryConflict];
If[MissingQ[militaryConflictEntity],
Return[Failure["InvalidParameter",<|"MessageTemplate"->"The militaryConflict input parameter is not valid. `militaryConflict` is not the proper way to call it.", "MessageParameters"-><| "militaryConflict" -> #militaryConflict |>|>]]
,
subconflicts=militaryConflictEntity["Subconflicts"];
If[MissingQ[subconflicts],subconflicts=List[militaryConflictEntity],None];
If[Length[subconflicts]<10,
subconflictInfo = 
EntityValue[subconflicts,
{
EntityProperty["MilitaryConflict","CasualtiesCivilianPrisoners"],
EntityProperty["MilitaryConflict","CasualtiesCiviliansKilled"],
EntityProperty["MilitaryConflict","CasualtiesCiviliansWounded"],
EntityProperty["MilitaryConflict","CasualtiesKilled"],
EntityProperty["MilitaryConflict","CasualtiesMissingInAction"],
EntityProperty["MilitaryConflict","CasualtiesPrisonersOfWar"],
EntityProperty["MilitaryConflict","CasualtiesWounded"],
EntityProperty["MilitaryConflict","CivilianCasualties"],
EntityProperty["MilitaryConflict","Commanders"],
EntityProperty["MilitaryConflict","EndDate"],
EntityProperty["MilitaryConflict","ForcesAircraft"],
EntityProperty["MilitaryConflict","ForcesArtillery"],
EntityProperty["MilitaryConflict","ForcesCavalry"],
EntityProperty["MilitaryConflict","ForcesCivilianForces"],
EntityProperty["MilitaryConflict","ForcesFortifications"],
EntityProperty["MilitaryConflict","ForcesInfantry"],
EntityProperty["MilitaryConflict","ForcesMilitaryForces"],
EntityProperty["MilitaryConflict","ForcesShips"],
EntityProperty["MilitaryConflict","ForcesTanks"],
EntityProperty["MilitaryConflict","ForcesTroops"],
EntityProperty["MilitaryConflict","ForcesVehicles"],
EntityProperty["MilitaryConflict","ForcesWarElephants"],
EntityProperty["MilitaryConflict","LossesAircraftCaptured"],
EntityProperty["MilitaryConflict","LossesAircraftDamaged"],
EntityProperty["MilitaryConflict","LossesAircraftDestroyed"],
EntityProperty["MilitaryConflict","LossesArtilleryCaptured"],
EntityProperty["MilitaryConflict","LossesArtilleryDamaged"],
EntityProperty["MilitaryConflict","LossesArtilleryDestroyed"],
EntityProperty["MilitaryConflict","LossesBuildingsCaptured"],
EntityProperty["MilitaryConflict","LossesBuildingsDamaged"],
EntityProperty["MilitaryConflict","LossesBuildingsDestroyed"],
EntityProperty["MilitaryConflict","LossesShipsCaptured"],
EntityProperty["MilitaryConflict","LossesShipsDamaged"],
EntityProperty["MilitaryConflict","LossesShipsDestroyed"],
EntityProperty["MilitaryConflict","LossesTanksAndVehiclesCaptured"],
EntityProperty["MilitaryConflict","LossesTanksAndVehiclesDamaged"],
EntityProperty["MilitaryConflict","LossesTanksAndVehiclesDestroyed"],
EntityProperty["MilitaryConflict","MainActors"],
EntityProperty["MilitaryConflict","MilitaryCasualties"],
EntityProperty["MilitaryConflict","Name"],
EntityProperty["MilitaryConflict","OutcomeNotes"],
EntityProperty["MilitaryConflict","StartDate"]
},"PropertyAssociation"];
subconflictInfoClean = DeleteCases[subconflictInfo,{Repeated[Missing[_]]},2]
,
subconflictInfo = EntityValue[subconflicts,
{
EntityProperty["MilitaryConflict","EndDate"],
EntityProperty["MilitaryConflict","Name"],
EntityProperty["MilitaryConflict","OutcomeNotes"],
EntityProperty["MilitaryConflict","StartDate"]
},"PropertyAssociation"];
subconflictInfoClean = DeleteCases[subconflictInfo,{Repeated[Missing[_]]},2]
];
With[{retval = <|"assistantNotesAboutTheImage" ->"The image is being generated in the background asynchronously and it will be displayed when ready.", 
"subconflictsWolframLanguage"->ExportString[subconflictInfoClean,"WL"] |>},
ExportString[retval, "RawJSON" ,"Compact"->True]]
]],28,Failure["TimeConstrained",<|"MessageTemplate"->"Request timed out"|>]]&, {"String", "JSON", "JSON"}]


mapWarAsync = APIFunction[{"militaryConflict"->"String", "geoRange"->Restricted["String",RegularExpression["(?i)(battles|actors|world)"]]->"Actors"},
TimeConstrained[
Module[{battlesPerSubconflict, reap, battlesGrouped, legends, mainActors,mainActorsWithDatedPolygon = {},mainActorsWithDefaultPolygon = {},militaryConflictEntity,militaryConflictEntityData, imageURL},
militaryConflictEntity = toEntityMilitaryConflictString[#militaryConflict];
If[MissingQ[militaryConflictEntity],
Return[Failure["InvalidParameter",<|"MessageTemplate"->"The militaryConflict input parameter is not valid. `militaryConflict` is not the proper way to call it.", "MessageParameters"-><| "militaryConflict" -> #militaryConflict |>|>]]
,
militaryConflictEntityData = EntityValue[militaryConflictEntity, { EntityProperty["MilitaryConflict", "Battles"], EntityProperty["MilitaryConflict", "Subconflicts"], EntityProperty["MilitaryConflict", "MainActors"]}, "PropertyAssociation"];
With[{battles = militaryConflictEntityData[EntityProperty["MilitaryConflict","Battles"]], subconflicts = militaryConflictEntityData[EntityProperty["MilitaryConflict","Subconflicts"]]},
If[MissingQ[battles],
(* no battles, there is just a single one *)
battlesGrouped = List[militaryConflictEntity];
legends = EntityValue[militaryConflictEntity,"Name"]
,
(* if there are multiple battles then there are also subconflicts *)
Assert[Length[subconflicts]>0];
battlesPerSubconflict = EntityValue[subconflicts, EntityProperty["MilitaryConflict","Battles"], "EntityAssociation"];
reap = Reap[
KeyValueMap[(If[MissingQ[#2],Sow[#1,"other" ];Nothing,Sow[CommonName@#1, "legend"];#2])&, battlesPerSubconflict],{"other","legend"}];
battlesGrouped = Flatten[Join[Most[reap],Most[Last[reap]]],1];
legends =Flatten[Join[Last[Last[reap]],{"other battles"}]];
];
If[ToLowerCase@#geoRange == "battles", mainActors = {},
mainActors = Cases[Flatten[militaryConflictEntityData[EntityProperty["MilitaryConflict","MainActors"]]],_Entity]];
 imageURL = CloudExport[
Rasterize[ 
Show[
GeoListPlot[
Partition[mainActors,1],
GeoBackground->Switch[ToLowerCase@#geoRange, "battles",{"ReliefMap", "StreetMapLabelsOnly"}, "actors", "Coastlines", "world","Coastlines"],
GeoLabels->True,
PlotStyle->{Directive[EdgeForm[{Black,Thin}],_]},
PlotLegends-> CommonName/@mainActors,
PlotStyle->ColorData[42, "ColorList"]
],
GeoListPlot[battlesGrouped,
GeoBackground->None,
PlotLegends->legends,
PlotLabel-> "Map of " <> CommonName[militaryConflictEntity],
PlotMarkers->If[Length[Flatten[battlesGrouped]]==1,GeoMarker,Point],
GeoScaleBar->{"Imperial","Metric"},
PlotStyle->ColorData[103, "ColorList"]
],
GeoRange->Switch[ToLowerCase@#geoRange, "battles",Automatic, "actors", Automatic, "world","World"],
ImageSize->Large
] , ImageResolution->300],
"PNG",Permissions->"Public"][[1]]  ;
 ExportString[<| "image"->imageURL |>,"RawJSON" ]
]
]],28,Failure["TimeConstrained",<|"MessageTemplate"->"Request timed out"|>]]&, {"String", "JSON", "JSON"}]


(* ::Subsubsection:: *)
(*test*)


mapWarSync[<|"militaryConflict"->"Battle of Waterloo"|>]


mapWarSync[<|"militaryConflict"->"World War I"|>]


mapWarSync[<|"militaryConflict"->"Battle of the Nile"|>]


mapWarSync[<|"militaryConflict"->"Battle of the GRRRR"|>]


str = mapWarAsync[<|"militaryConflict"->"Battle of the Nile"|>];
parsed = ImportString[str, "JSON"];
CloudImport["image"/. parsed]


str = mapWarAsync[<|"militaryConflict"->"Battle of Waterloo", "geoRange"->"battles"|>];
parsed = ImportString[str, "JSON"];
CloudImport["image"/. parsed]


str = mapWarAsync[<|"militaryConflict"->"Ottoman-Habsburg Wars", "geoRange"->"battles"|>];
parsed = ImportString[str, "JSON"];
CloudImport["image"/. parsed]


str = mapWarAsync[<|"militaryConflict"->"Ottoman-Habsburg Wars"|>];
parsed = ImportString[str, "JSON"];
CloudImport["image"/. parsed]


str = mapWarAsync[<|"militaryConflict"->"grgr"|>];
parsed = ImportString[str, "JSON"];
CloudImport["image"/. parsed]


str = mapWarAsync[<|"militaryConflict"->"battle of Mohacs"|>];
parsed = ImportString[str, "JSON"];
CloudImport["image"/. parsed]


str = mapWarAsync[<|"militaryConflict"->"battle of Mohacs", "geoRange"->"Battles"|>];
parsed = ImportString[str, "JSON"];
CloudImport["image"/. parsed]


str = mapWarAsync[<|"militaryConflict"->"Chinese Civil War", "geoRange"->"Battles"|>];
parsed = ImportString[str, "JSON"];
CloudImport["image"/. parsed]


(* ::Subsection:: *)
(*deploy*)


coMapWarSync = CloudDeploy[mapWarSync, "mapWarSync", Permissions->{PermissionsKey[PERMISSIONKEY]->"Execute"}]


coMapWarAsync = CloudDeploy[mapWarAsync, "mapWarAsync", Permissions->{PermissionsKey[PERMISSIONKEY]->"Execute"}]


(* ::Section::Closed:: *)
(*imageSearch*)


imageSearch = APIFunction["searchString"->"String",
TimeConstrained[
Module[{images, collage, imageURL},
images = DeleteCases[WebImageSearch[#searchString,"Images",4, Method->"Google"],Except[_Image]];
If[Length[images]>0,
collage = ImageCollage[ImageResize[#,500]&/@images];
imageURL = CloudExport[collage, "PNG", Permissions->"Public"][[1]];
ExportString[<|"image"->imageURL|>, "RawJSON"],
ExportString[<|"NoteforAssistant"->"The web search did not find any images."|>, "RawJSON"]
]
],10,Failure["TimeConstrained",<|"MessageTemplate"->"Request timed out"|>]
]&, {"String", "JSON", "JSON"}]


(* ::Subsubsection:: *)
(*test*)


imageSearch[<|"searchString"->"Marx"|>]


(* ::Subsection:: *)
(*deploy*)


coImageSearch = CloudDeploy[imageSearch, "imageSearch", Permissions->{PermissionsKey[PERMISSIONKEY]->"Execute"}]


(* ::Section::Closed:: *)
(*SYNC/ASYNC timelineCombined*)


timelineCombinedSync = APIFunction[{
"periods" -> DelimitedSequence[Restricted["String", {"Afsharid Dynasty", "Age of Enlightenment", "Age of Exploration", "Golden age of Perikles", "Alavid Dynasty", 
 "Ancient Greek Period", "Ancient Mesopotamian Period", "Archaic Greek Period", "Asuka period", "Atomic Age", 
 "Azuchi-Momoyama period", "Baroque era", "Aegean Bronze Age", "Asian Bronze Age", "European Bronze Age", 
 "Near Eastern Bronze Age", "Buyid Dynasty", "Byzantine Greek Period", "Chalcolithic", "Chen Dynasty", 
 "Classical Greek Period", "Cold War era", "Early Horizon period", "Early Intermediate period", 
 "Early Middle Ages", "Early Modern Europe", "Eastern Han Dynasty", "Eastern Wei Dynasty", 
 "Spring and Autumn Period", "Warring States Period", "Edo period", "Edwardian era", "Elizabethan era", 
 "European Migration period", "European Renaissance", "Georgian era", "Ghaznavid Dynasty", "Gilded Age", "Goryeo", 
 "Great Depression", "Green Revolution", "Harappan period", "Heian period", "Heisei period", 
 "Hellenistic Greek Period", "High Middle ages", "Napoleon's Hundred Days", "Indian Vedic period", 
 "Industrial Revolution", "Information Age", "Initial period", "Internet Age", "Interwar period", "Iron Age", 
 "Islamic Golden Age", "Jacobean era", "Eastern Jin Dynasty", "Sixteen Kingdoms", "J\:014dmon period", 
 "Joseon Dynasty", "July Monarchy", "Jurchen Jin Dynasty", "Kamakura period", "Khwarezmian Dynasty", 
 "Kofun period", "K\[OAcute]ka period", "Late Horizon period", "Late Intermediate period", "Late Middle ages", 
 "Later Han Dynasty", "Later Jin Dynasty", "Later Liang Dynasty", "Later Tang Dynasty", "Later Zhou Dynasty", 
 "Liang Dynasty", "Liao Dynasty", "Liu Song Dynasty", "Lower Paleolithic", "Machine Age", "Meiji period", 
 "Mesoamerican Classic era", "Mesoamerican Paleo-Indian", "Mesoamerican Postclassic era", 
 "Mesoamerican Pre\[Hyphen]Classic period", "Mesolithic", "Middle Horizon period", "Middle Kingdom", 
 "Middle Paleolithic", "Ming Dynasty", "Modern Greek Period", "Muromachi period", "Napoleonic era", "Nara period", 
 "Neoclassical period", "Neolithic Period", "New Kingdom", "Northern Wei Dynasty", "Northern Qi Dynasty", 
 "Northern Zhou Dynasty", "Old Kingdom", "Ottoman Greek Period", "Paleolithic", "Pax Romana", 
 "Protestant Reformation", "Qajar Dynasty", "Qin Dynasty", "Qing Dynasty", "Civil War Reconstruction", 
 "Regency England", "Reiwa Period", "Rococo era", "Roman Greek Period", "Romantic era", "Safavid Dynasty", 
 "Saffarid Dynasty", "Samanid Dynasty", "Scientific Revolution", "Second Industrial Revolution", "Shang Dynasty", 
 "Sh\:014dwa period", "Northern Song Dynasty", "Southern Song Dynasty", "Southern and Northern Dynasties", 
 "Southern Qi Dynasty", "Space Age", "Stone Age", "Sui Dynasty", "Tahirid Dynasty", "Taish\:014d period", 
 "Tang Dynasty", "Kingdom of Shu", "Kingdom of Wei", "Kingdom of Wu", "Three Sovereigns and the Five Emperors", 
 "Timurid Dynasty", "Upper Paleolithic", "Victorian era", "Viking Age", "Western Han Dynasty", 
 "Western Jin Dynasty", "Western Wei Dynasty", "Western Xia Dynasty", "Xia Dynasty", "Xin Dynasty", "Yayoi period", 
 "Yuan Dynasty", "Zand Dynasty", "Western Zhou Dynasty", "Ziyarid Dynasty"}],";"]->{},
"historicalCountries" -> DelimitedSequence["String", ";"]->{},
"militaryConflicts"->DelimitedSequence["String",";"]->{},
"persons"->DelimitedSequence["String", ";"]->{}},
TimeConstrained[
Module[{periods, incorrectPeriods, 
historicalCountries, incorrectHistoricalCountries,  
militaryConflicts, incorrectMilitaryConflicts, 
persons, incorrectPersons},
(* periods *)
periods = AssociationMap[Interpreter["HistoricalPeriod"][#]&,#periods];
incorrectPeriods = Select[periods, FailureQ] //Keys;
(* historical countries *)
historicalCountries = AssociationMap[toEntityHistoricalCountryString[#]&, #historicalCountries];
incorrectHistoricalCountries = Select[historicalCountries, MissingQ[EntityValue[#,"Name"]]&] // Keys;
(* militaryConfllicts *)
militaryConflicts = AssociationMap[toEntityMilitaryConflictString, #militaryConflicts];
incorrectMilitaryConflicts = Select[militaryConflicts, MissingQ] // Keys;
(* persons *)
persons = AssociationMap[Interpreter["Person"][#]&,#persons];
incorrectPersons = Select[persons, FailureQ] // Keys;
If[Length[Join[incorrectPeriods, incorrectHistoricalCountries, incorrectMilitaryConflicts, incorrectPersons]] == 0,
ExportString[
<| "assistantNotesAboutTheImage"-> "The image is being generated in the background asynchronously and it will be displayed when ready."|>,"RawJSON"],
Failure["IncorrectParameters", <|
	"MessageTemplate" -> "Some input parameters are invalid. incorrectPeriods: `incorrectPeriods`; incorrectHistoricalCountries: `incorrectHistoricalCountries`; incorrectMilitaryConflicts: `incorrectMilitaryConflicts`; incorrectPersons: `incorrectPersons`", 
	"MessageParameters"-> <|
		"incorrectPeriods"->StringRiffle[incorrectPeriods, ", "],
		"incorrectHistoricalCountries"-> StringRiffle[incorrectHistoricalCountries, ", "],
		"incorrectMilitaryConflicts"->StringRiffle[incorrectMilitaryConflicts, ", "],
		"incorrectPersons"->StringRiffle[incorrectPersons, ", "]
		|>
	|>]
]
],15,Failure["TimeConstrained",<|"MessageTemplate"->"Request timed out"|>]]&, {"String", "JSON", "JSON"}]


timelineCombinedAsync = APIFunction[{
"periods" -> DelimitedSequence[Restricted["String", {"Afsharid Dynasty", "Age of Enlightenment", "Age of Exploration", "Golden age of Perikles", "Alavid Dynasty", 
 "Ancient Greek Period", "Ancient Mesopotamian Period", "Archaic Greek Period", "Asuka period", "Atomic Age", 
 "Azuchi-Momoyama period", "Baroque era", "Aegean Bronze Age", "Asian Bronze Age", "European Bronze Age", 
 "Near Eastern Bronze Age", "Buyid Dynasty", "Byzantine Greek Period", "Chalcolithic", "Chen Dynasty", 
 "Classical Greek Period", "Cold War era", "Early Horizon period", "Early Intermediate period", 
 "Early Middle Ages", "Early Modern Europe", "Eastern Han Dynasty", "Eastern Wei Dynasty", 
 "Spring and Autumn Period", "Warring States Period", "Edo period", "Edwardian era", "Elizabethan era", 
 "European Migration period", "European Renaissance", "Georgian era", "Ghaznavid Dynasty", "Gilded Age", "Goryeo", 
 "Great Depression", "Green Revolution", "Harappan period", "Heian period", "Heisei period", 
 "Hellenistic Greek Period", "High Middle ages", "Napoleon's Hundred Days", "Indian Vedic period", 
 "Industrial Revolution", "Information Age", "Initial period", "Internet Age", "Interwar period", "Iron Age", 
 "Islamic Golden Age", "Jacobean era", "Eastern Jin Dynasty", "Sixteen Kingdoms", "J\:014dmon period", 
 "Joseon Dynasty", "July Monarchy", "Jurchen Jin Dynasty", "Kamakura period", "Khwarezmian Dynasty", 
 "Kofun period", "K\[OAcute]ka period", "Late Horizon period", "Late Intermediate period", "Late Middle ages", 
 "Later Han Dynasty", "Later Jin Dynasty", "Later Liang Dynasty", "Later Tang Dynasty", "Later Zhou Dynasty", 
 "Liang Dynasty", "Liao Dynasty", "Liu Song Dynasty", "Lower Paleolithic", "Machine Age", "Meiji period", 
 "Mesoamerican Classic era", "Mesoamerican Paleo-Indian", "Mesoamerican Postclassic era", 
 "Mesoamerican Pre\[Hyphen]Classic period", "Mesolithic", "Middle Horizon period", "Middle Kingdom", 
 "Middle Paleolithic", "Ming Dynasty", "Modern Greek Period", "Muromachi period", "Napoleonic era", "Nara period", 
 "Neoclassical period", "Neolithic Period", "New Kingdom", "Northern Wei Dynasty", "Northern Qi Dynasty", 
 "Northern Zhou Dynasty", "Old Kingdom", "Ottoman Greek Period", "Paleolithic", "Pax Romana", 
 "Protestant Reformation", "Qajar Dynasty", "Qin Dynasty", "Qing Dynasty", "Civil War Reconstruction", 
 "Regency England", "Reiwa Period", "Rococo era", "Roman Greek Period", "Romantic era", "Safavid Dynasty", 
 "Saffarid Dynasty", "Samanid Dynasty", "Scientific Revolution", "Second Industrial Revolution", "Shang Dynasty", 
 "Sh\:014dwa period", "Northern Song Dynasty", "Southern Song Dynasty", "Southern and Northern Dynasties", 
 "Southern Qi Dynasty", "Space Age", "Stone Age", "Sui Dynasty", "Tahirid Dynasty", "Taish\:014d period", 
 "Tang Dynasty", "Kingdom of Shu", "Kingdom of Wei", "Kingdom of Wu", "Three Sovereigns and the Five Emperors", 
 "Timurid Dynasty", "Upper Paleolithic", "Victorian era", "Viking Age", "Western Han Dynasty", 
 "Western Jin Dynasty", "Western Wei Dynasty", "Western Xia Dynasty", "Xia Dynasty", "Xin Dynasty", "Yayoi period", 
 "Yuan Dynasty", "Zand Dynasty", "Western Zhou Dynasty", "Ziyarid Dynasty"}],";"]->{},
"historicalCountries" -> DelimitedSequence["String", ";"]->{},
"militaryConflicts"->DelimitedSequence["String",";"]->{},
"persons"->DelimitedSequence["String", ";"]->{}},
TimeConstrained[
Module[{periods, periodsForTimeline,
historicalCountries, historicalCountriesForTimeline, 
militaryConflictsForTimeline,
persons,  personsForTimeline, 
imageURL},
(* periods *)
periods = AssociationMap[Interpreter["HistoricalPeriod"][#]&,#periods];
periodsForTimeline = Select[periods, Not[FailureQ[#]]&];
(* historical countries *)
historicalCountries = AssociationMap[toEntityHistoricalCountryString[#]&, #historicalCountries];
historicalCountriesForTimeline = Labeled[DateInterval[{EntityValue[#,"StartDate"],If[MissingQ[EntityValue[#,"EndDate"]],Now, EntityValue[#,"EndDate"]]}],EntityValue[#,"Name"]]& /@ 
Select[historicalCountries, Not[MissingQ[EntityValue[#,"Name"]]]&];
(* militaryConfllicts *)
militaryConflictsForTimeline = DeleteMissing[toEntityMilitaryConflictString /@ #militaryConflicts];
(* persons *)
persons = AssociationMap[Interpreter["Person"][#]&,#persons];
personsForTimeline = Labeled[DateInterval[{EntityValue[#,"BirthDate"],If[MissingQ[EntityValue[#,"DeathDate"]],Now, EntityValue[#,"DeathDate"]]}],EntityValue[#,"Name"]]&/@Values[Select[persons, Not[FailureQ[#]]&]];
(* timelinePlot *)
imageURL = CloudExport[
Rasterize[
TimelinePlot[{periodsForTimeline, historicalCountriesForTimeline, militaryConflictsForTimeline, personsForTimeline},
PlotTheme->"Web",PlotLayout->"Stacked", Filling-> Axis, PlotLegends->{"Periods","Historical Countries", "Military conflicts", "Persons"}, ImageSize->Large],
 ImageResolution->300], 
"PNG", Permissions->"Public"][[1]];
ExportString[<|"image"->imageURL|>,"RawJSON"]
],28,Failure["TimeConstrained",<|"MessageTemplate"->"Request timed out"|>]]&, {"String","JSON","JSON"}]


(* ::Subsection:: *)
(*test*)


timelineCombinedSync[<|"persons"->"Lenin;Lenin neni", "periods"->"space age; gr period", "historicalCountries"->"Roman EMpire; GR period", "militaryConflicts"->"Battle of Mohacs; Battle of GR"|>]


timelineCombinedSync[<|"persons"->"Lenin;Lenin neni", "periods"->"Space Age", "historicalCountries"->"Roman EMpire; GR period", "militaryConflicts"->"Battle of Mohacs; Battle of GR"|>]


timelineCombinedSync[<|"persons"->"Lenin;Lenin neni"|>]


str = timelineCombinedAsync[<|"persons"->"Lenin;Lenin neni", "periods"->"space age; gr period", "historicalCountries"->"Roman Empire; GR period", "militaryConflicts"->"Battle of Mohacs; Battle of GR"|>];
parsed = ImportString[str, "JSON"];
CloudImport["image"/.parsed]


(* ::Subsection:: *)
(*deploy*)


coTimelineCombinedSync = CloudDeploy[timelineCombinedSync, "timelineCombinedSync", Permissions->{PermissionsKey[PERMISSIONKEY]->"Execute"}]


coTimelineCombinedAsync = CloudDeploy[timelineCombinedAsync, "timelineCombinedAsync", Permissions->{PermissionsKey[PERMISSIONKEY]->"Execute"}]


(* ::Section::Closed:: *)
(*SYNC/ASYNC mapCombined*)


mapCombinedSync = APIFunction[{
"historicalCountries"->DelimitedSequence["String",";"]->{},
"yearForHistoricalCountries"->"String"->"",
"militaryConflicts"->DelimitedSequence["String",";"]->{},
"countries"->DelimitedSequence["String",";"]->{},
"cities"->DelimitedSequence["String",";"]->{},
"periods"->DelimitedSequence[Restricted["String", {"Afsharid Dynasty", "Age of Enlightenment", "Age of Exploration", "Golden age of Perikles", "Alavid Dynasty", 
 "Ancient Greek Period", "Ancient Mesopotamian Period", "Archaic Greek Period", "Asuka period", "Atomic Age", 
 "Azuchi-Momoyama period", "Baroque era", "Aegean Bronze Age", "Asian Bronze Age", "European Bronze Age", 
 "Near Eastern Bronze Age", "Buyid Dynasty", "Byzantine Greek Period", "Chalcolithic", "Chen Dynasty", 
 "Classical Greek Period", "Cold War era", "Early Horizon period", "Early Intermediate period", 
 "Early Middle Ages", "Early Modern Europe", "Eastern Han Dynasty", "Eastern Wei Dynasty", 
 "Spring and Autumn Period", "Warring States Period", "Edo period", "Edwardian era", "Elizabethan era", 
 "European Migration period", "European Renaissance", "Georgian era", "Ghaznavid Dynasty", "Gilded Age", "Goryeo", 
 "Great Depression", "Green Revolution", "Harappan period", "Heian period", "Heisei period", 
 "Hellenistic Greek Period", "High Middle ages", "Napoleon's Hundred Days", "Indian Vedic period", 
 "Industrial Revolution", "Information Age", "Initial period", "Internet Age", "Interwar period", "Iron Age", 
 "Islamic Golden Age", "Jacobean era", "Eastern Jin Dynasty", "Sixteen Kingdoms", "J\:014dmon period", 
 "Joseon Dynasty", "July Monarchy", "Jurchen Jin Dynasty", "Kamakura period", "Khwarezmian Dynasty", 
 "Kofun period", "K\[OAcute]ka period", "Late Horizon period", "Late Intermediate period", "Late Middle ages", 
 "Later Han Dynasty", "Later Jin Dynasty", "Later Liang Dynasty", "Later Tang Dynasty", "Later Zhou Dynasty", 
 "Liang Dynasty", "Liao Dynasty", "Liu Song Dynasty", "Lower Paleolithic", "Machine Age", "Meiji period", 
 "Mesoamerican Classic era", "Mesoamerican Paleo-Indian", "Mesoamerican Postclassic era", 
 "Mesoamerican Pre\[Hyphen]Classic period", "Mesolithic", "Middle Horizon period", "Middle Kingdom", 
 "Middle Paleolithic", "Ming Dynasty", "Modern Greek Period", "Muromachi period", "Napoleonic era", "Nara period", 
 "Neoclassical period", "Neolithic Period", "New Kingdom", "Northern Wei Dynasty", "Northern Qi Dynasty", 
 "Northern Zhou Dynasty", "Old Kingdom", "Ottoman Greek Period", "Paleolithic", "Pax Romana", 
 "Protestant Reformation", "Qajar Dynasty", "Qin Dynasty", "Qing Dynasty", "Civil War Reconstruction", 
 "Regency England", "Reiwa Period", "Rococo era", "Roman Greek Period", "Romantic era", "Safavid Dynasty", 
 "Saffarid Dynasty", "Samanid Dynasty", "Scientific Revolution", "Second Industrial Revolution", "Shang Dynasty", 
 "Sh\:014dwa period", "Northern Song Dynasty", "Southern Song Dynasty", "Southern and Northern Dynasties", 
 "Southern Qi Dynasty", "Space Age", "Stone Age", "Sui Dynasty", "Tahirid Dynasty", "Taish\:014d period", 
 "Tang Dynasty", "Kingdom of Shu", "Kingdom of Wei", "Kingdom of Wu", "Three Sovereigns and the Five Emperors", 
 "Timurid Dynasty", "Upper Paleolithic", "Victorian era", "Viking Age", "Western Han Dynasty", 
 "Western Jin Dynasty", "Western Wei Dynasty", "Western Xia Dynasty", "Xia Dynasty", "Xin Dynasty", "Yayoi period", 
 "Yuan Dynasty", "Zand Dynasty", "Western Zhou Dynasty", "Ziyarid Dynasty"}],";"]->{}},
TimeConstrained[
Module[{historicalCountries, incorrectHistoricalCountries, 
militaryConflicts, incorrectMilitaryConflicts,  
countries, incorrectCountries, cities, incorrectCities,
periods, incorrectPeriods},
(* historical countries *)
historicalCountries = AssociationMap[toEntityHistoricalCountryString[#]&, #historicalCountries];
incorrectHistoricalCountries = Select[historicalCountries, MissingQ[EntityValue[#,"Name"]]&] // Keys;
If[
(* required parameter missing *)
And[Length[#historicalCountries]>0,StringMatchQ[#yearForHistoricalCountries,""]],
Failure["MissingParameter",<|"MessageTemplate"->"When you are providing historicalCountries you also have to provide a value for the yearForHistoricalCountries." |>]
,
(* parameters OK *)
(* military conflicts *)
militaryConflicts = AssociationMap[toEntityMilitaryConflictString, #militaryConflicts ];
incorrectMilitaryConflicts = Select[militaryConflicts, MissingQ] // Keys;
(* countries *)
countries = AssociationMap[Interpreter["Country"][#]&, #countries];
incorrectCountries = Select[countries, FailureQ]//Keys;
(* cities *)
cities = AssociationMap[Interpreter["City"][#]&,#cities];
incorrectCities = Select[cities, FailureQ] // Keys;
(* periods  *)
periods = AssociationMap[Interpreter["HistoricalPeriod"][#]&,#periods];
incorrectPeriods = Select[periods, FailureQ] //Keys;
If[Length[Join[incorrectHistoricalCountries, incorrectMilitaryConflicts, incorrectCountries, incorrectCities, incorrectPeriods]] == 0,
	ExportString[<|"assistantNotesAboutTheImage" -> "The image is being generated in the background asynchronously and it will be displayed when ready."|>, "RawJSON"],
	Failure["InvalidParameters", <|
	"MessageTemplate" -> "Some input parameters are invalid. incorrectHistoricalCountries: `incorrectHistoricalCountries`; incorrectMilitaryConflicts: `incorrectMilitaryConflicts`; incorrectCountries: `incorrectCountries`; incorrectCities: `incorrectCities`; incorrectPeriods: `incorrectPeriods`", 
	"MessageParameters" -> <| 
		"incorrectHistoricalCountries"-> incorrectHistoricalCountries, 
		"incorrectMilitaryConflicts"->incorrectMilitaryConflicts, 
		"incorrectCountries"->incorrectCountries, 
		"incorrectCities"->incorrectCities, 
		"incorrectPeriods"->incorrectPeriods
	|>|>]
]
]
],
15,Failure["TimeConstrained",<|"MessageTemplate"->"Request timed out"|>]]&, {"String", "JSON", "JSON"}]


mapCombinedAsync = APIFunction[{
"historicalCountries"->DelimitedSequence["String",";"]->{},
"yearForHistoricalCountries"->"String"->"",
"militaryConflicts"->DelimitedSequence["String",";"]->{},
"countries"->DelimitedSequence["String",";"]->{},
"cities"->DelimitedSequence["String",";"]->{},
"periods"->DelimitedSequence[Restricted["String", {"Afsharid Dynasty", "Age of Enlightenment", "Age of Exploration", "Golden age of Perikles", "Alavid Dynasty", 
 "Ancient Greek Period", "Ancient Mesopotamian Period", "Archaic Greek Period", "Asuka period", "Atomic Age", 
 "Azuchi-Momoyama period", "Baroque era", "Aegean Bronze Age", "Asian Bronze Age", "European Bronze Age", 
 "Near Eastern Bronze Age", "Buyid Dynasty", "Byzantine Greek Period", "Chalcolithic", "Chen Dynasty", 
 "Classical Greek Period", "Cold War era", "Early Horizon period", "Early Intermediate period", 
 "Early Middle Ages", "Early Modern Europe", "Eastern Han Dynasty", "Eastern Wei Dynasty", 
 "Spring and Autumn Period", "Warring States Period", "Edo period", "Edwardian era", "Elizabethan era", 
 "European Migration period", "European Renaissance", "Georgian era", "Ghaznavid Dynasty", "Gilded Age", "Goryeo", 
 "Great Depression", "Green Revolution", "Harappan period", "Heian period", "Heisei period", 
 "Hellenistic Greek Period", "High Middle ages", "Napoleon's Hundred Days", "Indian Vedic period", 
 "Industrial Revolution", "Information Age", "Initial period", "Internet Age", "Interwar period", "Iron Age", 
 "Islamic Golden Age", "Jacobean era", "Eastern Jin Dynasty", "Sixteen Kingdoms", "J\:014dmon period", 
 "Joseon Dynasty", "July Monarchy", "Jurchen Jin Dynasty", "Kamakura period", "Khwarezmian Dynasty", 
 "Kofun period", "K\[OAcute]ka period", "Late Horizon period", "Late Intermediate period", "Late Middle ages", 
 "Later Han Dynasty", "Later Jin Dynasty", "Later Liang Dynasty", "Later Tang Dynasty", "Later Zhou Dynasty", 
 "Liang Dynasty", "Liao Dynasty", "Liu Song Dynasty", "Lower Paleolithic", "Machine Age", "Meiji period", 
 "Mesoamerican Classic era", "Mesoamerican Paleo-Indian", "Mesoamerican Postclassic era", 
 "Mesoamerican Pre\[Hyphen]Classic period", "Mesolithic", "Middle Horizon period", "Middle Kingdom", 
 "Middle Paleolithic", "Ming Dynasty", "Modern Greek Period", "Muromachi period", "Napoleonic era", "Nara period", 
 "Neoclassical period", "Neolithic Period", "New Kingdom", "Northern Wei Dynasty", "Northern Qi Dynasty", 
 "Northern Zhou Dynasty", "Old Kingdom", "Ottoman Greek Period", "Paleolithic", "Pax Romana", 
 "Protestant Reformation", "Qajar Dynasty", "Qin Dynasty", "Qing Dynasty", "Civil War Reconstruction", 
 "Regency England", "Reiwa Period", "Rococo era", "Roman Greek Period", "Romantic era", "Safavid Dynasty", 
 "Saffarid Dynasty", "Samanid Dynasty", "Scientific Revolution", "Second Industrial Revolution", "Shang Dynasty", 
 "Sh\:014dwa period", "Northern Song Dynasty", "Southern Song Dynasty", "Southern and Northern Dynasties", 
 "Southern Qi Dynasty", "Space Age", "Stone Age", "Sui Dynasty", "Tahirid Dynasty", "Taish\:014d period", 
 "Tang Dynasty", "Kingdom of Shu", "Kingdom of Wei", "Kingdom of Wu", "Three Sovereigns and the Five Emperors", 
 "Timurid Dynasty", "Upper Paleolithic", "Victorian era", "Viking Age", "Western Han Dynasty", 
 "Western Jin Dynasty", "Western Wei Dynasty", "Western Xia Dynasty", "Xia Dynasty", "Xin Dynasty", "Yayoi period", 
 "Yuan Dynasty", "Zand Dynasty", "Western Zhou Dynasty", "Ziyarid Dynasty"}],";"]->{}},
TimeConstrained[
Module[{historicalCountries,  historicalCountriesShownInDifferentYearWhereThereIsMapData, hitoricalCountriesForMap,
yearDate, 
militaryConflicts,  militaryConflictsFiltered, militaryConflictsForMap,
countries, cities,  placesForMap,
periods, periodsForMap, periodsForLegend,
battles,    
imageURL},
(* historical countries *)
historicalCountries = AssociationMap[toEntityHistoricalCountryString[#]&, #historicalCountries];
If[
(* required parameter missing *)
And[Length[#historicalCountries]>0,StringMatchQ[#yearForHistoricalCountries,""]],
Failure["MissingParameter",<|"MessageTemplate"->"When you are providing historicalCountries you also have to provide a value for the yearForHistoricalCountries." |>]
,
(* parameters OK *)
If[FailureQ[interpreterDate[#yearForHistoricalCountries]], If[IntegerQ[ToExpression[#yearForHistoricalCountries]],yearDate =DateObject[{ToExpression[#yearForHistoricalCountries]}],
Return[Nothing]],yearDate = interpreterDate[#yearForHistoricalCountries]];
historicalCountriesShownInDifferentYearWhereThereIsMapData = {};
hitoricalCountriesForMap = 
With[{datedPolygon =EntityValue[#,Dated["Polygon", yearDate]], name = EntityValue[#, "Name"]}, 
If[MissingQ[datedPolygon],
AppendTo[historicalCountriesShownInDifferentYearWhereThereIsMapData , EntityValue[#,"Name"]]; Labeled[EntityValue[#, "Polygon"], name],
Labeled[datedPolygon, name]
]]& /@
Values[Select[historicalCountries, Not[MissingQ[EntityValue[#,"Name"]]]&]];
(* military conflicts *)
militaryConflicts = AssociationMap[toEntityMilitaryConflictString, #militaryConflicts ];
battles = Flatten[
DeleteMissing[EntityValue[#,"Battles"]]&/@Values[DeleteMissing[militaryConflicts]]];
militaryConflictsFiltered = Select[Join[Values[DeleteMissing[militaryConflicts]], battles], Not[MissingQ[EntityValue[#,"Position"]]]&];
militaryConflictsForMap = Labeled[GeoMarker[#], EntityValue[#,"Name"]]&/@ militaryConflictsFiltered;
(* countries *)
countries = AssociationMap[Interpreter["Country"][#]&, #countries];
(* cities *)
cities = AssociationMap[Interpreter["City"][#]&,#cities];
placesForMap = Join[
Labeled[#,#]&/@ Values[Select[countries, Not[FailureQ[#]]&]], 
Labeled[#,#]&/@Values[Select[cities, Not[FailureQ[#]]&]]
];
(* periods  *)
periods = AssociationMap[Interpreter["HistoricalPeriod"][#]&,#periods];
periodsForMap = EntityValue[#, "CountriesInvolved"]&/@Values[Select[periods, Not[FailureQ[#]]&]];
periodsForLegend = EntityValue[#,"Name"]&/@Values[Select[periods, Not[FailureQ[#]]&]];
(* geoListPlot *)
imageURL = CloudExport[
Rasterize[
GeoListPlot[Join[{hitoricalCountriesForMap, militaryConflictsForMap, placesForMap},periodsForMap],
LabelStyle->Tiny, ImageSize->Large, GeoBackground->"CountryBorders", 
PlotLegends-> Join[{"Historical Coutries", "Military Conflicts","Places"}, periodsForLegend], 
PlotStyle-> ColorData[64, "ColorList"],
GeoScaleBar->{"Imperial","Metric"}
],ImageResolution->300],
"PNG", Permissions->"Public"][[1]];
ExportString[<|"image"->imageURL|>,"RawJSON"]
]],28,Failure["TimeConstrained",<|"MessageTemplate"->"Request timed out"|>]]&, {"String", "JSON", "JSON"}]


(* ::Subsection:: *)
(*test*)


mapCombinedSync[<|"historicalCountries"->"Roman Epire; GR",
"yearForHistoricalCountries"->"1456",
"militaryConflicts"-> "Battle of Waterloo;Battle of GR",
"countries"->"Hungary;Romania;GrGRGR",
"cities"->"Faszhaza;Budapest",
"periods"->"space age;Gr age"|>]


str = mapCombinedAsync[<|"historicalCountries"->"Roman Epire; GR",
"yearForHistoricalCountries"->"1456",
"militaryConflicts"-> "Battle of Waterloo;Battle of GR",
"countries"->"Hungary;Romania;GrGRGR",
"cities"->"Faszhaza;Budapest",
"periods"->"Space Age"|>];
parsed = ImportString[str, "JSON"];
CloudImport["image"/.parsed]


str = mapCombinedAsync[<|"historicalCountries"->"Roman Epire",
"yearForHistoricalCountries"->"200 AD",
"militaryConflicts"-> "Battle of Waterloo",
"countries"->"Hungary;Romania",
"cities"->"Budapest",
"periods"->"Space Age"|>];
parsed = ImportString[str, "JSON"];
CloudImport["image"/.parsed]


(* ::Subsection:: *)
(*deploy*)


coMapCombinedSync = CloudDeploy[mapCombinedSync, "mapCombinedSync", Permissions->{PermissionsKey[PERMISSIONKEY]->"Execute"}]


coMapCombinedAsync = CloudDeploy[mapCombinedAsync, "mapCombinedAsync", Permissions->{PermissionsKey[PERMISSIONKEY]->"Execute"}]
