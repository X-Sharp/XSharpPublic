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
   END ENUM                   
   
   [Flags];
   ENUM Modifiers AS LONG
      MEMBER @@None:=0
      MEMBER @@Private:= 0x01
      MEMBER @@Hidden:=  0x01
      MEMBER @@ProtectedInternal:=0x02
      MEMBER @@Internal:=0x04
      MEMBER @@Protected:=0x08
      MEMBER @@Public:=0x10
      MEMBER @@Export:=0x10
      MEMBER @@VisibilityMask := 0xFF
      MEMBER @@Abstract:=0x100
      MEMBER @@New:=0x200
      MEMBER @@Partial:=0x400
      MEMBER @@Sealed:=0x800
      MEMBER @@Static:=0x1000
      MEMBER @@Unsafe:=0x2000
      MEMBER @@Virtual:=0x4000
      MEMBER @@Override:=0x4000
      MEMBER @@External:=0x8000
      MEMBER @@Const:=0x10000
      MEMBER @@InitOnly:=0x20000
      MEMBER @@Instance:=0x40000
   END ENUM
   
   ENUM XFileType AS LONG
      MEMBER Unknown:=-1
      MEMBER SourceCode:=0
      MEMBER PreprocessorOutput:=1
      MEMBER Header:=2
      MEMBER VOForm:=3
      MEMBER VOMenu:=4
      MEMBER VODBServer:=5
      MEMBER VOIndex:=6
      MEMBER VOOrder:=7
      MEMBER VOFieldSpec:=8
      MEMBER NativeResource:=9
      MEMBER ManagedResource:=10
      MEMBER XAML:=11
      MEMBER Settings:=12
      MEMBER License:=13
      MEMBER Resource:= 14
      MEMBER Template:= 15       // tpl and inf 
   END ENUM
   
   ENUM ParamType AS BYTE
      MEMBER @@As		:= 0
      MEMBER @@Ref    := 1
      MEMBER @@Out	:= 2
      MEMBER @@Params := 3
      MEMBER @@In  	:= 4
   END ENUM
   
   ENUM CallingConvention
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
   
   
END NAMESPACE

