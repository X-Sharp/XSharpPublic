//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

[XSharp.Internal.TypesChanged];
CLASS DataField
	PROTECT oFieldSpec		AS FieldSpec
	PROTECT oHyperLabel	    AS HyperLabel

CONSTRUCTOR()
    
CONSTRUCTOR( oHLName AS STRING, oFS := NULL AS FieldSpec)
    SELF(HyperLabel{ oHLName }, oFS)

CONSTRUCTOR( oHLName AS HyperLabel, oFS := NULL AS FieldSpec) 
	oHyperLabel := oHLName
    oFieldSpec := oFS
	RETURN 

PROPERTY FieldSpec AS FieldSpec     GET oFieldSpec
PROPERTY HyperLabel AS HyperLabel   GET oHyperLabel
PROPERTY Name AS STRING             GET oHyperLabel:Name
PROPERTY NameSym AS SYMBOL          GET oHyperLabel:NameSym

METHOD AsString( ) AS STRING STRICT
	RETURN oHyperLabel:AsString( )

END CLASS

