// Class1.prg
#using System.IO

BEGIN NAMESPACE Fab_VO_Entities

   ENUM FabVODefinitions
        APPBODY  := 0x010F
        APPDESC := 0x000F
        APPEXPNAME := 0x0126
        APPNAME := 0x0002
        CH := 0x0125
        CHFILE := 0x0124
        DLL := (0x0060 )
        DLLFILE := 0x0102
        DLLFLG := 0x000B
        REC_END := 0xffff
        ENTBODY := 0x010D
        ENTNAME := 0x0040
        EXENAME := 0x000D
        EXTMOD := 0x0101
        EXTMODNAME := 0x0022
        EXTRES := (0x0103 )
        EXTRESNAME := 0x0061
        GROUPNAME := 0x0108
        REC_HEADER := 1
        LIBFLG := 0x000A
        MODBODY := 0x010E
        MODEXPNAME := 0x0127
        MODNAME := 0x0020
        SPATH := 0x0004
        UDC := 0x0005
        UDCFILE := (0x0100 )
        ENT_CREATETIME := 5
        ENT_LASTBUILD := 4
        ENT_NAME := 1
        ENT_PROTO := 3
        ENT_PTR := 2
        ENT_SIZE := 5
        ENTPROTO := 0x004e
        ENTSOURCE := 0x0041        
   END ENUM
   
    CLASS FabRecHeader
        PUBLIC uiType AS WORD
        PUBLIC ulLength AS DWORD
        
        STATIC PUBLIC Size := 6 AS long
        
    END CLASS
    
    CLASS FabEntInfo
        PUBLIC Name         AS  STRING
        PUBLIC Pos          AS  LONG
        PUBLIC Proto        AS  STRING
        PUBLIC LastBuild    as  DWORD
        PUBLIC CreateTime   as  DWORD
        PUBLIC MemStream    as  MemoryStream
        PUBLIC Size         as  long
        
        CONSTRUCTOR( oMS AS MemoryStream )
            SELF:Name := ""
            SELF:Pos := 0
            SELF:Proto := ""
            SELF:LastBuild := 0
            SELF:CreateTime := 0
            SELF:MemStream := oMS
            Self:Size := 0
        RETURN
        
    END CLASS
           
END NAMESPACE // Fab_VO_Entities
   