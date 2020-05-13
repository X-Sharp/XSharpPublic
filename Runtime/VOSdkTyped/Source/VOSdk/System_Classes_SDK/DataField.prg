CLASS DataField
	PROTECT oFieldSpec		AS FieldSpec
	PROTECT oHyperLabel	AS HyperLabel

METHOD AsString( ) AS STRING STRICT
	RETURN oHyperLabel:AsString( )

ACCESS FieldSpec AS FieldSpec
	RETURN oFieldSpec

ACCESS HyperLabel AS HyperLabel
	RETURN oHyperLabel

CONSTRUCTOR( oHLName AS STRING, oFS := NULL AS FieldSpec)
    SELF(HyperLabel{ oHLName }, oFS)

CONSTRUCTOR( oHLName AS HyperLabel, oFS := NULL AS FieldSpec) 
	oHyperLabel := oHLName
    oFieldSpec := oFS
	RETURN 

ACCESS Name AS STRING
	RETURN oHyperLabel:Name

ACCESS NameSym AS SYMBOL
	RETURN oHyperLabel:NameSym


END CLASS

