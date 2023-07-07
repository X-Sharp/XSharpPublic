
BEGIN NAMESPACE XSharp.Settings

/// <summary>
/// The KnownTypes class.
/// </summary>
STATIC CLASS KnownTypes

    PUBLIC CONST SystemExtension    := "System.Runtime.CompilerServices.ExtensionAttribute" AS STRING
    PUBLIC CONST VulcanClipper      := "Vulcan.Internal.ClipperCallingConventionAttribute" AS STRING
    PUBLIC CONST VulcanClassLibrary := "Vulcan.Internal.VulcanClassLibraryAttribute" AS STRING
    PUBLIC CONST VulcanImplicitNS   := "Vulcan.VulcanImplicitNamespaceAttribute" AS STRING
    PUBLIC CONST VulcanDefaultParam := "Vulcan.Internal.DefaultParameterValueAttribute" as STRING
    PUBLIC CONST XSharpClipper      := "XSharp.Internal.ClipperCallingConventionAttribute" AS STRING
    PUBLIC CONST XSharpClassLibrary := "XSharp.Internal.ClassLibraryAttribute" AS STRING
    PUBLIC CONST XSharpImplicitNS   := "XSharp.ImplicitNamespaceAttribute" AS STRING
    PUBLIC CONST XSharpDefaultParam := "XSharp.Internal.DefaultParameterValueAttribute" AS STRING


    PUBLIC CONST ArrayType          := "__Array" AS STRING
    PUBLIC CONST BinaryType         := "__Binary" AS STRING
    PUBLIC CONST CodeBlockType      := "CodeBlock" AS STRING
    PUBLIC CONST DateType           := "__Date" AS STRING
    PUBLIC CONST PszType            := "__Psz" AS STRING
    PUBLIC CONST SymbolType         := "__Symbol" AS STRING

    PUBLIC CONST VulcanArray        := "Vulcan.__Array" AS STRING
    PUBLIC CONST VulcanCodeblock    := "Vulcan._CodeBlock" AS STRING
    PUBLIC CONST VulcanDate         := "Vulcan.__VODate" AS STRING
    PUBLIC CONST VulcanFloat        := "Vulcan.__VOFloat" AS STRING
    PUBLIC CONST VulcanPSZ          := "Vulcan.__Psz" AS STRING
    PUBLIC CONST VulcanSymbol       := "Vulcan.__Symbol" AS STRING
    PUBLIC CONST VulcanUsual        := "Vulcan.__Usual" AS STRING
    PUBLIC CONST VulcanUsualType    := "Vulcan.UsualType" AS STRING
    PUBLIC CONST VulcanWinBool      := "Vulcan.__WinBool" AS STRING


    PUBLIC CONST XSharpArray        := "XSharp.__Array" AS STRING
    PUBLIC CONST XSharpArrayBase    := "XSharp.__ArrayBase" AS STRING
    PUBLIC CONST XSharpBinary       := "XSharp.__Binary" AS STRING
    PUBLIC CONST XSharpCodeblock    := "XSharp._CodeBlock" AS STRING
    PUBLIC CONST XSharpCurrency     := "XSharp.__Currency" AS STRING
    PUBLIC CONST XSharpDate         := "XSharp.__Date" AS STRING
    PUBLIC CONST XSharpFoxArray     := "XSharp.__FoxArray" AS STRING
    PUBLIC CONST XSharpFloat        := "XSharp.__Float" AS STRING
    PUBLIC CONST XSharpPSZ          := "XSharp.__Psz" AS STRING
    PUBLIC CONST XSharpSymbol       := "XSharp.__Symbol" AS STRING
    PUBLIC CONST XSharpUsual        := "XSharp.__Usual" AS STRING
    PUBLIC CONST XSharpUsualType    := "XSharp.__UsualType" AS STRING
    PUBLIC CONST XSharpVODate       := "XSharp.__VODate" AS STRING
    PUBLIC CONST XSharpVOFloat      := "XSharp.__VOFloat" AS STRING
    PUBLIC CONST XSharpWinBool      := "XSharp.__WinBool" AS STRING
    PUBLIC CONST XSharpWinDate      := "XSharp.__WinDate" AS STRING
    
    PUBLIC CONST SystemArray        := "System.Array"  AS STRING
    PUBLIC CONST SystemBoolean      := "System.Boolean"  AS STRING
    PUBLIC CONST SystemByte         := "System.Byte"     AS STRING
    PUBLIC CONST SystemChar         := "System.Char"     AS STRING
    PUBLIC CONST SystemDateTime     := "System.DateTime" AS STRING
    PUBLIC CONST SystemDecimal      := "System.Decimal"  AS STRING
    PUBLIC CONST SystemDouble       := "System.Double"   AS STRING
    PUBLIC CONST SystemInt16        := "System.Int16"    AS STRING
    PUBLIC CONST SystemInt32        := "System.Int32"    AS STRING
    PUBLIC CONST SystemInt64        := "System.Int64"    AS STRING
    PUBLIC CONST SystemObject       := "System.Object"   AS STRING
    PUBLIC CONST SystemSByte        := "System.SByte"    AS STRING
    PUBLIC CONST SystemSingle       := "System.Single"   AS STRING
    PUBLIC CONST SystemString       := "System.String"   AS STRING
    PUBLIC CONST SystemUInt16       := "System.UInt16"   AS STRING
    PUBLIC CONST SystemUInt32       := "System.UInt32"   AS STRING
    PUBLIC CONST SystemUInt64       := "System.UInt64"   AS STRING
    PUBLIC CONST SystemVoid         := "System.Void"     AS STRING
    PUBLIC CONST SystemVoidPtr      := "System.Void*"    AS STRING
    PUBLIC CONST SystemIntPtr       := "System.IntPtr"   AS STRING


    public const NullDate           := "NULL_DATE"      AS STRING
    public const NullObject         := "NULL_OBJECT"    AS STRING
    public const NullPsz            := "NULL_PSZ"       AS STRING
    public const NullPtr            := "NULL_PTR"       AS STRING
    public const NullString         := "NULL_STRING"    AS STRING
    public const NullSymbol         := "NULL_SYMBOL"    AS STRING


END CLASS
END NAMESPACE // XSharpModel.Support
