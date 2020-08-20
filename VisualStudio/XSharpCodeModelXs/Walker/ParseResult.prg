USING System
USING System.Collections.Generic
USING System.Linq
 
BEGIN NAMESPACE XSharpModel
CLASS ParseResult
	PROPERTY Types			AS IList<EntityObject> AUTO
	PROPERTY Entities		AS IList<EntityObject> AUTO
	PROPERTY SpecialLines   AS IList<LineObject> AUTO
	PROPERTY Locals         AS IList<EntityObject> AUTO
	PROPERTY SourceLength   AS INT AUTO
	PROPERTY LineCount      AS INT AUTO
	PROPERTY NameSpaces		AS IList<NameSpaceObject> AUTO

	CONSTRUCTOR (oParser AS Parser)
		SELF:Types			:= oParser:Types:ToArray()
		SELF:Entities		:= oParser:Entities:ToArray()
		SELF:SpecialLines   := oParser:SpecialLines:ToArray()
		SELF:SourceLength   := oParser:SourceLength
		SELF:LineCount      := oParser:LineCount
		SELF:Locals			:= oParser:Locals
		//
		SELF:NameSpaces		:= oParser:NameSpaces:ToArray()

END CLASS
END NAMESPACE
