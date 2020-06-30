CLASS DataField
	PROTECT oFieldSpec		AS FieldSpec
	PROTECT oHyperLabel	    AS HyperLabel


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

