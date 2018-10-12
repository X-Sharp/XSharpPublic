#using System.Collections.Generic
#using System.Collections

begin namespace Xide
    
enum     EntityType as Int32 // todo need to add delegate, operator
        member _None
        member _Constructor
        member _Destructor
        member _Method
        member _Access
        member _Assign
        member _Class
        member _Function
        member _Procedure
        member _Enum
        member _VOStruct
        member _Global
        member _Structure
        member _Interface
        member _Delegate
        member _Event
        member @@_Field
        member _Union
        member _Operator
        member _Local
        member _Property
        
        member _Define
        member _Resource
        member _TextBlock
end     enum
    
    [Flags];
enum         EntityModifiers as Int32
        member _None := 0
        member _Protected := 1
        member _Private := 2
        member _Internal := 4
        member _Virtual := 8
        member _Abstract := 16
        member _Sealed := 32
        member _Static := 64
        member _Partial := 128
        member _New := 256
end     enum
    
enum     AccessLevel
        member @@Public := 0
        member @@Protected := 1
        member @@Hidden := 2
        member @@Internal := 4
end     enum
    
class     EntityParamsObject
        export cName as string
        export cType as string
        export lReference as logic
        constructor(_cName as string , _cType as string)
            super()
            self:cName := _cName
            self:cType := _cType
            return
        access IntelText as string
            local cRet as string
            cRet := self:cName
            if .not. String.IsNullOrWhiteSpace(self:cType)
                cRet += iif(self:lReference , " REF " , " AS ") + self:cType
            end if
            return cRet
        method Clone() as EntityParamsObject
            return (EntityParamsObject)self:MemberwiseClone()
end             class
    
class     EntityObject
        export eType as EntityType
        export cName,cInherit,cRetType,cImplements as string
        export eModifiers as EntityModifiers
        export eAccessLevel as AccessLevel
        export cShortClassName as string
        export cTypedClassName as string
        export cClassNamespace as string
        export aParams as List<EntityParamsObject>
        export nLine , nCol as int
        export aNameSpaces as List<string> // prosoxh, kai se functions etc
        export lStatic as logic
        export lPartial as logic
        export cClassType as string
        export lExtension as logic
        
        constructor()
            super()
            self:cInherit := ""
            self:cImplements := ""
            self:cRetType := ""
            self:cClassType := ""
            return
            
        virtual method Clone() as EntityObject
            local oEntity as EntityObject
            oEntity := (EntityObject)self:MemberwiseClone()
            if oEntity:aParams != null
                oEntity:aParams := List<EntityParamsObject>{}
                foreach oParam as EntityParamsObject in self:aParams
                    oEntity:aParams:Add(oParam:Clone())
                next
            end if
            if oEntity:aNameSpaces != null
                oEntity:aNameSpaces := List<string>{}
                foreach cNameSpace as string in oEntity:aNameSpaces
                    oEntity:aNameSpaces:Add(cNameSpace)
                next
            end if
            return oEntity
            
        property HasParams as logic get aParams != null .and. aParams:Count != 0
        
        method NamespacesEqual(_aNameSpaces as List<string>) as logic
            local n as int
            if self:aNameSpaces == null .or. self:aNameSpaces:Count != _aNameSpaces:Count
                return false
            end if
            for n := 0 upto self:aNameSpaces:Count - 1
                if self:aNameSpaces[n] != _aNameSpaces[n]
                    return false
                end if
            next
            return true
        method SetNamespaces(_aNameSpaces as List<string>) as void
            local n as int
            if self:NamespacesEqual(_aNameSpaces)
                return
            end if 
            if self:aNameSpaces == null
                self:aNameSpaces := List<string>{_aNameSpaces:Count}
            end if
            for n := 0 upto _aNameSpaces:Count - 1
                self:aNameSpaces:Add(_aNameSpaces[n])
            next
            return
            
        access FullClassName as string
            local cRet as string
            cRet := self:cShortClassName
            if .not. String.IsNullOrEmpty(self:cClassNamespace)
                cRet := self:cClassNamespace + "." + cRet
            end if
            return cRet
        access FullName as string
            local cRet as string
            switch self:eType 
            case EntityType._Class 
            case EntityType._Interface 
            case EntityType._Structure
                cRet := self:FullClassName
            case EntityType._Access 
            case EntityType._Assign 
            case EntityType._Property 
            case EntityType._Method 
            case EntityType._Field 
            case EntityType._Event
                cRet := self:FullClassName + "." + self:cName
            case EntityType._Function 
            case EntityType._Procedure 
            case EntityType._Global
                cRet := self:cName
            case EntityType._Enum
                cRet := self:FullClassName
            case EntityType._Delegate
                cRet := self:FullClassName
            case EntityType._VOStruct 
            case EntityType._Union
                cRet := self:FullClassName
            otherwise
                cRet := ""
            end switch
            return cRet
            
        access IsType as logic
            switch self:eType 
            case EntityType._Class 
            case EntityType._Structure 
            case EntityType._VOStruct 
            case EntityType._Union 
            case EntityType._Interface 
            case EntityType._Delegate 
            case EntityType._Enum
                return true
            otherwise
                return false
            end switch
            
            
        access IsClassOrMember as logic
            switch self:eType 
            case EntityType._Class 
            case EntityType._Method 
            case EntityType._Access 
            case EntityType._Assign 
            case EntityType._Property 
            case EntityType._Constructor 
            case EntityType._Destructor
                return true
            otherwise
                return false
            end switch
            
        access NonClass as logic
            switch self:eType 
                case EntityType._Function 
            case EntityType._Procedure 
                case EntityType._Global
                    return true
                otherwise
                    return false
                end switch
            
        access IsCode as logic
            switch self:eType 
            case EntityType._Function 
            case EntityType._Procedure 
            case EntityType._Access 
            case EntityType._Assign 
            case EntityType._Constructor 
            case EntityType._Destructor 
            case EntityType._Method 
            case EntityType._Operator 
            case EntityType._Property
                return true
             otherwise
                return false
             end switch
        access IsVO as logic
            switch self:eType
            case EntityType._Function 
            case EntityType._Procedure 
            case EntityType._VOStruct 
            case EntityType._Union 
            case EntityType._Global
                return true
            otherwise
                return false
            end switch
            
         access IsFuncProc as logic
            switch self:eType
            case EntityType._Function 
            case EntityType._Procedure
                return true
            otherwise
                return false
            end switch
        access IsFuncProcGlobal as logic
            switch self:eType 
            case EntityType._Function 
            case EntityType._Procedure 
            case EntityType._Global
                return true
            otherwise
                return false
            end switch
            
        access StringEntityType as string
            return self:eType:ToString():Substring(1):ToUpper()
            
            method AddParam(cParam as string) as void
            if self:aParams == null
                self:aParams := List<EntityParamsObject>{}
            end if
            self:aParams:Add(EntityParamsObject{cParam , "USUAL"})
            return
        method AddParam(cParam as string , cType as string) as void
            if self:aParams == null
                self:aParams := List<EntityParamsObject>{}
            end if
            self:aParams:Add(EntityParamsObject{cParam , cType})
            return
        method SetParamType(cType as string) as void
            local oParam as EntityParamsObject
            if self:aParams == null .or. self:aParams:Count == 0 .or. String.IsNullOrEmpty(cType)
                return
            end if
            oParam := self:aParams[self:aParams:Count - 1]
            if cType:Contains("&")
                cType := cType:Replace("&" , "")
                oParam:lReference := true
            end if
            oParam:cType := cType
            return
            
            virtual method ToString() as string
            local cRet as string
            //		LOCAL n AS INT
            if self:eType == EntityType._Property .or. self:eType == EntityType._Access .or. self:eType == EntityType._Assign .or. ;
                    self:eType == EntityType._Constructor .or. self:eType == EntityType._Destructor .or. self:eType == EntityType._Event .or. ;
                    self:eType == EntityType._Field .or. self:eType == EntityType._Method
                cRet := "    "
            else
                cRet := ""
            end if
            cRet += self:eType:ToString() + " "
            cRet += self:cName
            if .not. String.IsNullOrEmpty(self:cInherit)
                cRet += " (" + self:cInherit + ") "
            end if
            return cRet
            
end class
    
end namespace
