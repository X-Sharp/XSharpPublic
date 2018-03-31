using System
using System.Collections.Generic
using System.Collections.Immutable
 
begin namespace XSharpModel
class ParseResult
	property Types			as IList<EntityObject> auto
	property Entities		as IList<EntityObject> auto
	property SpecialLines   as IList<LineObject> auto
	property SourceLength   as int auto
	property LineCount      as int auto

	constructor (oParser as Parser)
		self:Types			:= oParser:Types:ToImmutableArray()
		self:Entities		:= oParser:Entities:ToImmutableArray()
		self:SpecialLines   := oParser:SpecialLines:ToImmutableArray()
		self:SourceLength   := oParser:SourceLength
		self:LineCount      := oParser:LineCount
end class
end namespace
