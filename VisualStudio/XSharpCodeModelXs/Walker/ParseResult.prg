USING System
USING System.Collections.Generic
USING System.Collections.Immutable
 
BEGIN NAMESPACE XSharpModel
CLASS ParseResult
	PROPERTY Types			AS IList<EntityObject> AUTO
	PROPERTY Entities		AS IList<EntityObject> AUTO
	PROPERTY SpecialLines   AS IList<LineObject> AUTO
	PROPERTY Locals         AS IList<EntityObject> AUTO
	PROPERTY SourceLength   AS INT AUTO
	PROPERTY LineCount      AS INT AUTO

	CONSTRUCTOR (oParser AS Parser)
		SELF:Types			:= oParser:Types:ToImmutableArray()
		SELF:Entities		:= oParser:Entities:ToImmutableArray()
		SELF:SpecialLines   := oParser:SpecialLines:ToImmutableArray()
		SELF:SourceLength   := oParser:SourceLength
		SELF:LineCount      := oParser:LineCount
		SELF:Locals			:= oParser:Locals

END CLASS
END NAMESPACE
