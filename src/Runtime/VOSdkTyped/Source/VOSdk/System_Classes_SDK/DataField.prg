//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


/// <include file="System.xml" path="doc/DataField/*" />
[XSharp.Internal.TypesChanged];
CLASS DataField
	PROTECT oFieldSpec		AS FieldSpec
	PROTECT oHyperLabel	    AS HyperLabel


/// <include file="System.xml" path="doc/DataField.ctor/*" />
CONSTRUCTOR() STRICT


/// <include file="System.xml" path="doc/DataField.ctor/*" />
CONSTRUCTOR( oHLName AS STRING, oFS := NULL AS FieldSpec)
    SELF(HyperLabel{ oHLName }, oFS)


/// <include file="System.xml" path="doc/DataField.ctor/*" />
CONSTRUCTOR( oHLName AS HyperLabel, oFS := NULL AS FieldSpec)
	oHyperLabel := oHLName
    oFieldSpec := oFS
	RETURN


/// <include file="System.xml" path="doc/DataField.FieldSpec/*" />
PROPERTY FieldSpec AS FieldSpec     GET oFieldSpec
/// <include file="System.xml" path="doc/DataField.HyperLabel/*" />
PROPERTY HyperLabel AS HyperLabel   GET oHyperLabel
/// <include file="System.xml" path="doc/DataField.Name/*" />
PROPERTY Name AS STRING             GET oHyperLabel:Name
/// <include file="System.xml" path="doc/DataField.NameSym/*" />
PROPERTY NameSym AS SYMBOL          GET oHyperLabel:NameSym


/// <include file="System.xml" path="doc/DataField.AsString/*" />
METHOD AsString( ) AS STRING STRICT
	RETURN oHyperLabel:AsString( )


END CLASS


