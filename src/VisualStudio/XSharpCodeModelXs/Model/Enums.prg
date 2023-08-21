//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING LanguageService.CodeAnalysis.XSharp.SyntaxParser

BEGIN NAMESPACE XSharpModel
   INTERNAL ENUM ImageListKind AS Int32
      // These values match the StandardGlyphGroup in Microsoft.VisualStudio.Language.Intellisense
      // there are 6 variations of each item, based on the visibility (see the overlay enum below)
      // StandardGlyphGroup continues after ValueType with several other icons
      MEMBER @@Class:=0
      MEMBER @@Const:=6
      MEMBER @@Delegate:=12
      MEMBER @@Enum:=18
      MEMBER @@EnumValue:=24
      MEMBER @@Event:=30
      MEMBER @@Exception:=36
      MEMBER @@Field:=42
      MEMBER @@Interface:=48
      MEMBER @@Macro:=54
      MEMBER @@Map:=60
      MEMBER @@MapItem:=66
      MEMBER @@Method:=72
      MEMBER @@Overload:=78
      MEMBER @@Module:=84
      MEMBER @@Namespace:=90
      MEMBER @@Operator:=96
      MEMBER @@Property:=102
      MEMBER @@Structure:=108
      MEMBER @@Template:=114
      MEMBER @@Typedef:=120
      MEMBER @@Type:=126
      MEMBER @@Union:=132
      MEMBER @@Local:=138
      MEMBER @@ValueType:=144

      // These items also have overlays
      MEMBER Intrinsic := 150
      MEMBER JSharpMethod := 156
      MEMBER JSharpField := 162
      MEMBER JSharpClass := 168
      MEMBER JSharpNamespace := 174
      MEMBER JSharpInterface := 180
      MEMBER GroupError := 186
      MEMBER ExtensionMethod := 220

      // These items have no overlays
      MEMBER BscFile := 191
      MEMBER Assembly := 192
      MEMBER Library := 193
      MEMBER VBProject := 194
      MEMBER CoolProject := 196
      MEMBER CppProject := 199
      MEMBER DialogId := 200
      MEMBER OpenFolder := 201
      MEMBER ClosedFolder := 202
      MEMBER Arrow := 203
      MEMBER CSharpFile := 204
      MEMBER CSharpExpansion := 205
      MEMBER Keyword := 206
      MEMBER Information := 207
      MEMBER Reference := 208
      MEMBER Recursion := 209
      MEMBER XmlItem := 210
      MEMBER JSharpProject := 211
      MEMBER JSharpDocument := 212
      MEMBER ForwardType := 213
      MEMBER CallersGraph := 214
      MEMBER CallGraph := 215
      MEMBER Warning := 216
      MEMBER MaybeReference := 217
      MEMBER MaybeCaller := 218
      MEMBER MaybeCall := 219

      MEMBER XmlAttribute := 226
      MEMBER XmlChild := 227
      MEMBER XmlDescendant := 228
      MEMBER XmlNamespace := 229
      MEMBER XmlAttributeQuestion := 230
      MEMBER XmlAttributeCheck := 231
      MEMBER XmlChildQuestion := 232
      MEMBER XmlChildCheck := 233
      MEMBER XmlDescendantQuestion := 234
      MEMBER XmlDescendantCheck := 235
      MEMBER CompletionWarning := 236
      MEMBER GroupUnknown := 237

   END ENUM

   INTERNAL ENUM ImageListOverlay AS Int32
      MEMBER @@Internal:=1
      MEMBER @@ProtectedInternal:=2
      MEMBER @@Protected:=3
      MEMBER @@Private:=4
      MEMBER @@ImageListOverlayArrow:=5
      MEMBER @@Public:=0
   END ENUM

   [Flags];
   Enum AccessorKind
        MEMBER @@Get        := 1 << 0
        MEMBER @@Set        := 1 << 1
        MEMBER @@Add        := 1 << 2
        MEMBER @@Remove     := 1 << 3
   END ENUM
   ENUM Kind AS Int32
      MEMBER @@Namespace   :=  0
      MEMBER @@Class       :=  1
      MEMBER @@Structure   :=  2
      MEMBER @@Constructor :=  3
      MEMBER @@Destructor  :=  4
      MEMBER @@Method      :=  5
      MEMBER @@Access      :=  6
      MEMBER @@Assign      :=  7
      MEMBER @@Property    :=  8
      MEMBER @@Function    :=  9
      MEMBER @@Procedure   :=  10
      MEMBER @@Field       :=  11
      MEMBER @@Local       :=  12
      MEMBER @@Parameter   :=  13
      MEMBER @@Event       :=  14
      MEMBER @@Operator    :=  15
      MEMBER @@Interface   :=  16
      MEMBER @@Delegate    :=  17
      MEMBER @@Enum        :=  18
      MEMBER @@EnumMember  :=  19
      MEMBER @@Keyword     :=  20
      MEMBER @@Union       :=  21
      MEMBER @@Using       :=  22
      MEMBER @@VODefine    :=  23
      MEMBER @@VODLL       :=  24
      MEMBER @@VOStruct    :=  25
      MEMBER @@VOGlobal    :=  26
      MEMBER @@Unknown     :=  27
      MEMBER @@Ignore      :=  28
      MEMBER @@MemVar      :=  29
      MEMBER @@DbField     :=  30
      MEMBER @@LocalFunc   :=  31
      MEMBER @@LocalProc   :=  32
      MEMBER @@Attribute   :=  33
      MEMBER @@Define      :=  34
      MEMBER @@Undefine    :=  35
      MEMBER @@Command     :=  36
      MEMBER @@XCommand    :=  37
      MEMBER @@Translate   :=  38
      MEMBER @@XTranslate  :=  39
      MEMBER @@Undeclared  :=  40
      MEMBER @@TypeParameter  :=  41
      MEMBER @@Include     := 42
   END ENUM

   [Flags];
   ENUM Modifiers AS LONG
      MEMBER @@None              := 0
      MEMBER @@Private           := 1 << 0
      MEMBER @@Hidden            := 1 << 0   // alias for Private
      MEMBER @@ProtectedInternal := 1 << 1
      MEMBER @@Internal          := 1 << 2
      MEMBER @@Protected         := 1 << 3
      MEMBER @@Public            := 1 << 4
      MEMBER @@Export            := 1 << 4   // alias for Public
      MEMBER @@VisibilityMask    := (1 << 5) -1
      MEMBER @@Abstract          := 1 << 5
      MEMBER @@New               := 1 << 6
      MEMBER @@Partial           := 1 << 7
      MEMBER @@Sealed            := 1 << 8
      MEMBER @@Static            := 1 << 9
      MEMBER @@Unsafe            := 1 << 10
      MEMBER @@Virtual           := 1 << 11
      MEMBER @@Override          := 1 << 11  // alias for Virtual
      MEMBER @@External          := 1 << 12
      MEMBER @@Const             := 1 << 13
      MEMBER @@InitOnly          := 1 << 14
      MEMBER @@Instance          := 1 << 15
      MEMBER @@Volatile          := 1 << 16
      MEMBER @@Async             := 1 << 17
      //XPP Modifiers below
      MEMBER @@Deferred          := 1 << 20            // Mapped to ABSTRACT
      MEMBER @@Final             := 1 << 21            // Mapped to SEALED
      MEMBER @@Freeze            := 1 << 22            // Not supported by the compiler
      MEMBER @@Introduce         := 1 << 23            // Mapped to NEW
      MEMBER @@Sync              := 1 << 24            // Implemented in the method body
      MEMBER @@Class             := 1 << 25            // Mapped to STATIC

   END ENUM

   ENUM XFileType AS LONG
      MEMBER Unknown            := -1
      MEMBER SourceCode         := 0
      MEMBER PreprocessorOutput := 1
      MEMBER Header             := 2
      MEMBER VOForm             := 3
      MEMBER VOMenu             := 4
      MEMBER VODBServer         := 5
      MEMBER VOIndex            := 6
      MEMBER VOOrder            := 7
      MEMBER VOFieldSpec        := 8
      MEMBER NativeResource     := 9
      MEMBER ManagedResource    := 10
      MEMBER XAML               := 11
      MEMBER Settings           := 12
      MEMBER License            := 13
      MEMBER Resource           := 14
      MEMBER Template           := 15       // tpl and inf
      MEMBER TextTemplate       := 16     // tt
      MEMBER Config             := 17     // config for app.config and packages.config
      MEMBER Other              := 99
   END ENUM

   ENUM ParamType
      MEMBER @@As       := XSharpLexer.AS
      MEMBER @@Ref      := XSharpLexer.REF
      MEMBER @@Out      := XSharpLexer.OUT
      MEMBER @@Params   := XSharpLexer.PARAMS
      MEMBER @@In       := XSharpLexer.IN
   END ENUM

   ENUM LocalType
      MEMBER @@As       := XSharpLexer.AS
      MEMBER @@Is       := XSharpLexer.IS
   END ENUM

   ENUM CallingConvention
      // Convention=(CLIPPER | STRICT | PASCAL | ASPEN | WINCALL | CALLBACK | FASTCALL | THISCALL)
      MEMBER None          := 0
      MEMBER @@Clipper     :=  XSharpLexer.CLIPPER
      MEMBER @@Strict      :=  XSharpLexer.STRICT
      MEMBER @@Pascal      :=  XSharpLexer.PASCAL
      MEMBER @@Aspen       :=  XSharpLexer.ASPEN
      MEMBER @@Wincall     :=  XSharpLexer.WINCALL
      MEMBER @@Callback    :=  XSharpLexer.CALLBACK
      MEMBER @@Fastcall    :=  XSharpLexer.FASTCALL
      MEMBER @@Thiscall    :=  XSharpLexer.THISCALL
   END ENUM

    ENUM ImpliedKind
        MEMBER None         := 0     //
        MEMBER Assignment   := 1     // VAR name := Expression         Need to detect the type of Expression. Save everything after := to the Expression
        MEMBER TypeCheck    := 2     // IF x IS type VAR name          This will assign the typename of the ImpliedVar. No need to store the Expression
        MEMBER InCollection := 3     // FOREACH VAR x IN collection    Need to detect type of collection: everything after IN is saved in the Expression
        MEMBER LoopCounter  := 4     // FOR VAR  name := start TO end  Need to detect the type of start or end. Save tokens start .. end in the Expression
        MEMBER OutParam     := 5     // Int32.TryParse("123", OUT VAR name)  // need to detect the type of the method and get the parametertype. Whole line until closing paren in the Expression
        MEMBER Using        := 6     // (BEGIN) USING VAR name := Expression   variation of VAR Name := Expression
    END ENUM

END NAMESPACE
Function EscapeKeyword(sName as STRING) AS STRING
    RETURN sName
