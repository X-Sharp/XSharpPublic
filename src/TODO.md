# TODO Items in X# Source Code

> Generated from source code. Excludes `src/Roslyn`.

**Total: 419 TODO items across 24 areas.**

## Summary

| Area | Count |
|------|-------|
| `src/Common/FoxProCmd.xh` | 2 |
| `src/Compiler/src` | 59 |
| `src/CompilerTests/Applications` | 2 |
| `src/CompilerTests/Automated` | 5 |
| `src/Runtime/MacroCompiler` | 34 |
| `src/Runtime/VOSDK` | 10 |
| `src/Runtime/VOSdkTyped` | 120 |
| `src/Runtime/XSharp.Core` | 2 |
| `src/Runtime/XSharp.RT` | 4 |
| `src/Runtime/XSharp.Rdd` | 14 |
| `src/Runtime/XSharp.SQLRdd` | 3 |
| `src/Runtime/XSharp.VFP` | 11 |
| `src/Runtime/XSharp.VFP.UI` | 46 |
| `src/Tools/Mono.Cecil` | 3 |
| `src/Tools/SHFB` | 7 |
| `src/Tools/VFPXPorter` | 9 |
| `src/Tools/VOXporter` | 5 |
| `src/VisualStudio/AppDesigner` | 4 |
| `src/VisualStudio/CodeGenerator` | 4 |
| `src/VisualStudio/LanguageService` | 13 |
| `src/VisualStudio/ProjectBase` | 25 |
| `src/VisualStudio/ProjectPackage` | 7 |
| `src/VisualStudio/XSharpCodeModelXs` | 6 |
| `src/VisualStudio/XSharpVoEditors` | 24 |

## Details

### src/Common/FoxProCmd.xh (2)

- `src/Common/FoxProCmd.xh:50` — // todo
- `src/Common/FoxProCmd.xh:414` — // Todo FoxPro clauses

### src/Compiler/src (59)

- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Antlr4.Runtime/Atn/ATNDeserializer.cs:950` — // TODO: not yet implemented
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Antlr4.Runtime/Atn/LexerATNSimulator.cs:542` — // TODO: if the entry rule is invoked recursively, some
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Antlr4.Runtime/Atn/ParserATNSimulator.cs:559` — // TODO: don't we always stop? only lexer would keep going
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Antlr4.Runtime/Atn/ParserATNSimulator.cs:560` — // TODO: v3 dfa don't do this.
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Antlr4.Runtime/Atn/ParserATNSimulator.cs:800` — /// TODO: greedy + those
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Antlr4.Runtime/Atn/ParserATNSimulator.cs:1508` — // TODO: make sure it distinguishes empty stack states
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Antlr4.Runtime/Atn/PredicateTransition.cs:10` — /// TODO: this is old comment:
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Antlr4.Runtime/Atn/PredicateTransition.cs:14` — /// TODO: this is old comment:
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Antlr4.Runtime/Atn/SetTransition.cs:18` — // TODO (sam): should we really allow null here?
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Antlr4.Runtime/Lexer.cs:556` — // TODO: Do we lose character or line position information?
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Antlr4.Runtime/Parser.cs:970` — // TODO: useful in parser?
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Antlr4.Runtime/Tree/Xpath/XPath.cs:88` — // TODO: check for invalid token/rule names, bad syntax
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Binder/Binder_Expressions.cs:70` — left = new BoundPointerIndirectionOperator(node.Left, left, refersToLocation: false, pointedAtType, hasErrors) // TODO nvk (refersToLocation)
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Binder/Binder_Invocation.cs:96` — // ToDo
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Binder/Semantics/Conversions.cs:644` — // TODO (nvk): Check numeric range before accepting conversion!
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Parser/XSharpCompilerGeneratedCode.cs:125` — null, // TODO nvk: parameterList
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Parser/XSharpCompilerGeneratedCode.cs:155` — null, // TODO nvk: parameterList
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Parser/XSharpCompilerGeneratedCode.cs:197` — null, // TODO nvk: parameterList
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Parser/XSharpCompilerGeneratedCode.cs:247` — null, // TODO nvk: parameterList
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Parser/XSharpLexerCode.cs:596` — // Todo: The VFP and Xbase++ dialect do not know
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Parser/XSharpTreeTransformationCore.cs:1657` — parameterList: null, // TODO nvk: parameterList
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Parser/XSharpTreeTransformationCore.cs:1717` — parameterList: null, // TODO nvk: parameterList
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Parser/XSharpTreeTransformationCore.cs:2478` — attributeLists: default, // TODO nvk
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Parser/XSharpTreeTransformationCore.cs:2480` — returnType: null, // TODO nvk
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Parser/XSharpTreeTransformationCore.cs:2490` — attributeLists: default, // TODO nvk
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Parser/XSharpTreeTransformationCore.cs:2492` — returnType: null, // TODO nvk
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Parser/XSharpTreeTransformationCore.cs:3053` — parameterList: null, // TODO nvk
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Parser/XSharpTreeTransformationCore.cs:3106` — parameterList: null, // TODO nvk
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Parser/XSharpTreeTransformationCore.cs:3122` — parameterList: null, // TODO nvk
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Parser/XSharpTreeTransformationCore.cs:3221` — parameterList: null, // TODO nvk
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Parser/XSharpTreeTransformationCore.cs:3237` — parameterList: null, // TODO nvk
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Parser/XSharpTreeTransformationCore.cs:4006` — parameterList: null, // TODO nvk
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Parser/XSharpTreeTransformationCore.cs:4544` — explicitInterfaceSpecifier: null, // TODO nvk
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Parser/XSharpTreeTransformationCore.cs:4546` — checkedKeyword: null, // TODO nvk
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Parser/XSharpTreeTransformationCore.cs:4575` — explicitInterfaceSpecifier: null, // TODO nvk
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Parser/XSharpTreeTransformationCore.cs:4578` — checkedKeyword: null, // TODO nvk
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Parser/XSharpTreeTransformationCore.cs:4927` — null, // TODO: (grammar) name: attr arg syntax?
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Parser/XSharpTreeTransformationCore.cs:5593` — // TODO nvk (calling convention is silently ignored for now)
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Parser/XSharpTreeTransformationCore.cs:5782` — // todo: declare and process attributes
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Parser/XSharpTreeTransformationCore.cs:7698` — attributeLists: default, //TODO nvk
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Parser/XSharpTreeTransformationCore.cs:7700` — returnType: null, // TODO nvk
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Parser/XSharpTreeTransformationCore.cs:9997` — attributeLists: default, // TODO nvk
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Parser/XSharpTreeTransformationCore.cs:9999` — returnType: null, // TODO nvk
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Parser/XSharpTreeTransformationFox.cs:945` — parameterList: null, //TODO nvk
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Parser/XSharpTreeTransformationFox.cs:1224` — // todo: declare and process attributes
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Parser/XSharpTreeTransformationRT.cs:1674` — null, // TODO nvk
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Parser/XSharpTreeTransformationRT.cs:4385` — attributeLists: default, // TODO nvk
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Parser/XSharpTreeTransformationRT.cs:4387` — returnType: null, // TODO nvk
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Parser/XSharpTreeTransformationRT.cs:4416` — attributeLists: default, // TODO nvk
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Parser/XSharpTreeTransformationRT.cs:4418` — returnType: null, // TODO nvk
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Parser/XSharpTreeTransformationVO.cs:70` — parameterList: null, //TODO nvk
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Parser/XSharpTreeTransformationVO.cs:179` — parameterList: null, //TODO nvk
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Parser/XSharpTreeTransformationXPP.cs:270` — parameterList: null, //TODO nvk
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Parser/XSharpTreeTransformationXPP.cs:1073` — // todo place in unique namespace
- `src/Compiler/src/Compiler/XSharpCodeAnalysis/Symbols/AnonymousTypemanager.cs:65` — // TODO nvk: refKind, scopde of AnonymousTypeField() below
- `src/Compiler/src/Scripting/XSharpScripting/XSharpMacroCompiler.cs:66` — // TODO: report diagnostics
- `src/Compiler/src/Scripting/XSharpScripting/XSharpMacroCompiler.cs:83` — optimizationLevel: OptimizationLevel.Debug, // TODO
- `src/Compiler/src/Scripting/XSharpScripting/XSharpMacroCompiler.cs:84` — checkOverflow: false,                       // TODO
- `src/Compiler/src/Scripting/XSharpScripting/XSharpMacroCompiler.cs:85` — allowUnsafe: true,                          // TODO

### src/CompilerTests/Applications (2)

- `src/CompilerTests/Applications/RuntimeTests/Prg/RuntimeTests.prg:65` — // TODO Must fail: "C135"
- `src/CompilerTests/Applications/RuntimeTests/Prg/RuntimeTests.prg:119` — // todo: set the correct dialect by calling

### src/CompilerTests/Automated (5)

- `src/CompilerTests/Automated/CompilerTests.prg:1020` — // todo Na fygoun auta
- `src/CompilerTests/Automated/CompilerTests.prg:2737` — // todo katalathos, bgazei lathh to peverify
- `src/CompilerTests/Automated/CompilerTests.prg:3568` — SELF:cSwitchesCF := "" // todo exei problhma o compiler
- `src/CompilerTests/Automated/CompilerTests.prg:4313` — // TODO Dirtiest Hack Ever (the FOR loop)
- `src/CompilerTests/Automated/CompilerTests.prg:4844` — CATCH // todo auto mphke giati kapoies fores o compiler kanei report errors me corrupted filename

### src/Runtime/MacroCompiler (34)

- `src/Runtime/MacroCompiler/Binder/Binder.Argument.cs:93` — // Todo: Handle IN parameters
- `src/Runtime/MacroCompiler/Binder/Binder.BinaryOp.cs:220` — // TODO (nvk): We don't support C#12 virtual static interfaces yet.
- `src/Runtime/MacroCompiler/Binder/Binder.Call.cs:239` — // todo: Check for ref arguments and handle writeback from arguments array
- `src/Runtime/MacroCompiler/Binder/Binder.Call.cs:254` — // todo: Check for ref arguments and handle writeback from arguments array
- `src/Runtime/MacroCompiler/Binder/Binder.Call.cs:269` — // todo: Check for ref arguments and handle writeback from arguments array
- `src/Runtime/MacroCompiler/Binder/Binder.Conversion.cs:124` — return conv; // TODO nvk: this should raise a warning!
- `src/Runtime/MacroCompiler/Binder/Binder.Literal.cs:340` — // todo
- `src/Runtime/MacroCompiler/Binder/Binder.Type.cs:51` — return Compilation.Get(NativeType.Object); // TODO nvk: special dynamic type?
- `src/Runtime/MacroCompiler/Binder/Conversion.cs:253` — //case NativeType.Decimal: return 14; // TODO nvk: easy-out ops
- `src/Runtime/MacroCompiler/Binder/Conversion.cs:274` — //case NativeType.Decimal: return 27; // TODO nvk: easy-out ops
- `src/Runtime/MacroCompiler/Binder/Node.Bound.Stmt.cs:153` — // TODO: Handle STATIC
- `src/Runtime/MacroCompiler/Binder/Node.Bound.Stmt.cs:173` — // TODO: Handle IS
- `src/Runtime/MacroCompiler/Binder/Node.Bound.Stmt.cs:177` — // TODO: Handle DIM, array sub peroperly (according to full compiler)
- `src/Runtime/MacroCompiler/Binder/Node.Bound.Stmt.cs:678` — // TODO: do not allow multiple unfiltered catch blocks of the same type
- `src/Runtime/MacroCompiler/Binder/Node.Bound.Stmt.cs:694` — // TODO: support filtered exceptions
- `src/Runtime/MacroCompiler/Binder/Node.Bound.Stmt.cs:799` — // TODO: UNSAFE
- `src/Runtime/MacroCompiler/Binder/Node.Bound.Stmt.cs:802` — // TODO: CHECKED
- `src/Runtime/MacroCompiler/Binder/Node.Bound.Stmt.cs:805` — // TODO: UNCHECKED
- `src/Runtime/MacroCompiler/Binder/Node.Bound.Stmt.cs:881` — // TODO: support FIXED when pointer support is added
- `src/Runtime/MacroCompiler/Binder/Node.Bound.cs:157` — // TODO (nvk): If delegates are supprted this needs to be revised!
- `src/Runtime/MacroCompiler/Binder/Node.Bound.cs:642` — // Todo:
- `src/Runtime/MacroCompiler/Binder/OverloadResult.cs:114` — // TODO (nvk): I'm not sure why this.Symbol could be equal to other.Symbol, but it can happen
- `src/Runtime/MacroCompiler/CodeGen/CodeGen.Emit.cs:297` — ilg.Emit(OpCodes.Newobj, typeof(decimal).GetConstructor(new[] { typeof(int) })); // TODO use Compilation.GetMember
- `src/Runtime/MacroCompiler/CodeGen/Node.Emit.cs:231` — // TODO (nvk): to be implemented
- `src/Runtime/MacroCompiler/CodeGen/Node.Emit.cs:235` — // TODO (nvk): to be implemented
- `src/Runtime/MacroCompiler/CodeGen/Node.Emit.cs:239` — // TODO (nvk): to be implemented
- `src/Runtime/MacroCompiler/CodeGen/Operator.Binary.Emit.cs:19` — EmitBinaryOperator(ilg, this, OpType.TypeSymbol()); // TODO nkok: handle checked/unchecked
- `src/Runtime/MacroCompiler/CodeGen/Operator.Unary.Emit.cs:19` — EmitUnaryOperator(ilg, this, OpType.TypeSymbol()); // TODO nkok: handle checked/unchecked
- `src/Runtime/MacroCompiler/Preprocessor/XSharpPPTokenExtensions.cs:279` — // TODO nvk
- `src/Runtime/MacroCompiler/Preprocessor/XSharpPPTokenExtensions.cs:304` — case XSharpLexer.ASSIGN_QQMARK: //Todo: Complete
- `src/Runtime/MacroCompiler/Syntax/Parser.Expr.cs:216` — // TODO nvk: PTR LPAREN Type=datatype COMMA Expr=expression RPAREN		#voCastPtrExpression	// PTR( typeName, expr )
- `src/Runtime/MacroCompiler/Syntax/Parser.Expr.cs:317` — // TODO nvk: Parse generic arguments
- `src/Runtime/MacroCompiler/Syntax/Parser.Expr.cs:327` — // TODO nvk: parse PTR, array specifiers
- `src/Runtime/MacroCompiler/Syntax/Parser.Expr.cs:350` — // TODO nvk: Parse property initializers { name := expr }

### src/Runtime/VOSDK (10)

- `src/Runtime/VOSDK/Source/VOSDK/GUI_Classes_SDK/Accelerator.prg:141` — RegisterAxit(SELF) // TODO: Conditional call to RegisterAxit() should be replaced with call to GC.SuppressFinalize() for opposite condition
- `src/Runtime/VOSDK/Source/VOSDK/GUI_Classes_SDK/Application Window.prg:261` — // TODO
- `src/Runtime/VOSDK/Source/VOSDK/GUI_Classes_SDK/Bitmap.prg:70` — RegisterAxit(SELF) // TODO: Conditional call to RegisterAxit() should be replaced with call to GC.SuppressFinalize() for opposite condition
- `src/Runtime/VOSDK/Source/VOSDK/GUI_Classes_SDK/IPCTopic.prg:161` — RegisterAxit(SELF) // TODO: Conditional call to RegisterAxit() should be replaced with call to GC.SuppressFinalize() for opposite condition
- `src/Runtime/VOSDK/Source/VOSDK/GUI_Classes_SDK/Icon.prg:84` — RegisterAxit(SELF) // TODO: Conditional call to RegisterAxit() should be replaced with call to GC.SuppressFinalize() for opposite condition
- `src/Runtime/VOSDK/Source/VOSDK/GUI_Classes_SDK/ShellWindow.prg:557` — // TODO
- `src/Runtime/VOSDK/Source/VOSDK/GUI_Classes_SDK/ToolBar.prg:1210` — RegisterAxit(SELF) // TODO: Conditional call to RegisterAxit() should be replaced with call to GC.SuppressFinalize() for opposite condition
- `src/Runtime/VOSDK/Source/VOSDK/Internet_SDK/CMailAbstract.prg:158` — RegisterAxit(SELF) // TODO: Conditional call to RegisterAxit() should be replaced with call to GC.SuppressFinalize() for opposite condition
- `src/Runtime/VOSDK/Source/VOSDK/RDD_Classes_SDK/DBOrderSpec.prg:975` — // TODO: Make OrdDestroy() delete NTX like CDX does.
- `src/Runtime/VOSDK/Source/VOSDK/Report_Classes_SDK/Methods.prg:544` — TRY // TODO: Temp workaround for Access Violation thrown by WrmTerminate()

### src/Runtime/VOSdkTyped (120)

- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/AnimationControl.prg:8` — //Todo: Implement ANimationControl
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/Control.prg:119` — //Todo __EnsureVisibity
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/Control.prg:281` — // Todo __EnsureVisibity
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/Control.prg:543` — //Todo Default
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/Control.prg:582` — //Todo DisableTheme
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/DataBrowser.prg:1598` — // Todo ChangeBackground
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/DataBrowser.prg:1830` — // Todo Check if the border looks as expected
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/DataBrowser.prg:1935` — // Todo: Enable vertical split, using Frozen Columns ?
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/DataBrowser.prg:2327` — //Todo
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/DataBrowser.prg:2935` — // Todo ?
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/DataListView.prg:142` — // Todo: Implement __SearchForVirtualItems
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/DateTimePicker.prg:144` — // Todo DateTimePicker.ParentNotify
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/ListBox.prg:267` — //Todo EnableItemDrag
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/ListBox.prg:425` — //Todo SelectedFile
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/ListBox.prg:532` — //Todo ListFiles
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/ListView.prg:119` — //Todo __CreateDragImageList
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/ListView.prg:414` — //Todo	 DragImageList
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/ListView.prg:419` — //Todo	 DragImageList
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/ListView.prg:768` — //tODO ItemsPerPage
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/ListView.prg:898` — //Todo SetBackgroundImage
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/ListView.prg:1065` — //Todo	 SetSelectedColumn
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/ListView.prg:1151` — //Todo ViewBoundingBox
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/ListView.prg:1168` — //Todo ViewOrigin
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/ListView.prg:1403` — //Todo DropTarget
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/ListView.prg:1408` — //Todo DropTarget
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/ListView.prg:1510` — //todo OverlayImageIndex
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/MMContainer.prg:21` — //Todo Implement
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/MMContainer.prg:29` — //Todo Implement
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/MMContainer.prg:62` — //Todo Implement
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/MMContainer.prg:77` — //Todo Implement
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/MMContainer.prg:114` — //Todo Implement
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/MMContainer.prg:183` — //Todo Implement
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/MMContainer.prg:225` — //Todo Implement
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/RichTextEdit.prg:96` — //Todo Font
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/RichTextEdit.prg:139` — //Todo Font
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/RichTextEdit.prg:326` — //Todo Richedit Print
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/ScrollBar.prg:88` — //Todo Notify Owner
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/Slider.prg:57` — // Todo SelectionSlider.ClearSelection
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/Slider.prg:72` — // Todo SelectionRange
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/Slider.prg:86` — // Todo SelectionRange
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/Slider.prg:118` — // Todo Slider.ClearTicks
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/Slider.prg:124` — //Todo GetTickPos
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/Slider.prg:137` — // Todo ThumbBoundingBox
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/Slider.prg:151` — // Todo ThumbLength
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/Slider.prg:157` — // Todo ThumbLength
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/Slider.prg:199` — // Todo TickCount
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/Spinner.prg:11` — //Todo Implement Spinner
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/SplitView.prg:233` — // Todo SplitView.ChangeBackGround
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/StatusBar.prg:146` — //Todo: BorderWidths
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/StatusBar.prg:533` — //Todo: ODDrawItem
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/TabControl.prg:65` — // Todo: Fix this
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/TextControl.prg:204` — // Todo: Implement EnableAutoComplete
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/TreeView.prg:31` — // Todo Implement __CreateDragImageList
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/TreeView.prg:47` — // Todo Check  __Expand
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/TreeView.prg:331` — // Todo GetDropHighlight
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/TreeView.prg:770` — // Todo
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/TreeView.prg:840` — //Todo Set Bold when needed
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/TreeView.prg:843` — //Todo Set Disabled when needed
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/TreeView.prg:854` — //Todo Set Focused when needed
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/TreeView.prg:999` — //Todo tree view comparison code
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Controls/WindowScrollBar.prg:6` — //Todo: WIndowScrollBar classes
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Drawing/Bitmap.prg:43` — //Todo ?
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Drawing/Bitmap.prg:48` — //Todo handle various options
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Drawing/FixedIcon.prg:33` — // Todo Lable Image
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Drawing/FormattedTextObject.prg:19` — // Todo FormattedTextObject:Draw()
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Drawing/ImageList.prg:37` — //Todo AddMask
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Drawing/ImageList.prg:56` — //Todo CreateOverlayImage
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Drawing/RectangleObject.prg:27` — //ToDo Draw
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Events/ControlEvents.prg:182` — //Todo RichEditProtectEvent
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Events/ControlEvents.prg:215` — //Todo RichEditSelectionEvent
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Events/ControlEvents.prg:315` — //Todo SysLinkSelectEvent
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Events/OleDragEvent.prg:44` — //Todo
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Menu/Menu.prg:99` — // todo Menu Bitmaps
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Menu/Menu.prg:346` — //Todo MakeMenuRtol
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Menu/Menu.prg:445` — //Todo ShowAsPopup
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Menu/Menu.prg:679` — //		//Todo SystemMenu class
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Menu/ToolBar.prg:12` — // Todo Implement CoolBar Toolbar
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Menu/ToolBar.prg:293` — // Todo: Toolbar.AddBand
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Menu/ToolBar.prg:375` — // Todo: Toolbar.AddSubToolBarBand
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Printing/PrintingDevice.prg:51` — //Todo
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Support/Pair.prg:95` — //Todo ConvertToScreen
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Support/Temp.prg:16` — // Todo:
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/WinForm/ChildWinForm.prg:92` — //Todo ActivateWinForm
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/WinForm/WinFormVOWindowHost.prg:144` — //Todo AdjustVOWindow
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Windows/Application Window.prg:70` — //Todo Dispatch ?
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Windows/Application Window.prg:162` — //Todo EnableOleDropTarget
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Windows/Application.prg:45` — //Todo __SetHelpWind
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Windows/Application.prg:49` — //Todo __SetHelpWind
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Windows/Application.prg:57` — //Todo __SetHelpWind
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Windows/DataWindow.prg:1097` — // todo: Implement ClipperKeys
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Windows/DataWindow.prg:1199` — //Todo ?
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Windows/DataWindow.prg:1476` — //  Todo Draw
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Windows/DataWindow.prg:1876` — //Todo	LineTo
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Windows/DataWindow.prg:1930` — //Todo	 MoveTo
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Windows/DataWindow.prg:2138` — //Todo	 PaintBoundingBox
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Windows/DataWindow.prg:2517` — // Todo	 TextPrint
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Windows/DialogWindow.prg:154` — // todo: Implement ClipperKeys
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Windows/Window.prg:534` — // Todo __CreateSelfBitmap
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Windows/Window.prg:590` — // Todo __EnableHelpCursor
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Windows/Window.prg:676` — // Todo ? __GetDC
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Windows/Window.prg:933` — // Todo __ProcessHelp
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Windows/Window.prg:1280` — //Todo ComboBoxExNotify
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Windows/Window.prg:1802` — // Todo EnableThemeDialogTexture
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Windows/Window.prg:1809` — // Todo EnableToolTips
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Windows/Window.prg:2151` — //Todo
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Windows/Window.prg:2199` — // Todo
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Windows/Window.prg:2296` — // Todo
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Windows/Window.prg:2324` — // Todo MouseButtonUp
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Windows/Window.prg:2340` — // Todo MouseDrag
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Windows/Window.prg:2355` — // Todo MouseTrapOff
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Windows/Window.prg:2362` — // Todo MouseTrapOn
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Windows/Window.prg:2372` — // Todo
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Windows/Window.prg:2440` — // Todo PaintBackground
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Windows/Window.prg:2477` — //Todo PaintBoundingBox
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Windows/Window.prg:2556` — // Todo  Print
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Windows/Window.prg:2680` — // Todo Scroll
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Windows/Window.prg:2742` — // TOdo SetBackgroundBrush
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Windows/Window.prg:2945` — // TOdo SysLinkSelect
- `src/Runtime/VOSdkTyped/Source/VOSdk/GUI_Classes_SDK/Windows/Window.prg:2979` — // Todo TextPrint
- `src/Runtime/VOSdkTyped/Source/VOSdk/RDD_Classes_SDK/DBOrderSpec.prg:861` — // TODO: Make OrdDestroy() delete NTX like CDX does.

### src/Runtime/XSharp.Core (2)

- `src/Runtime/XSharp.Core/RDD/DbcSupport.prg:771` — // todo: close all DBC files.
- `src/Runtime/XSharp.Core/RDD/Workarea.prg:1098` — // todo check basic implementation of Workarea.Info

### src/Runtime/XSharp.RT (4)

- `src/Runtime/XSharp.RT/Functions/ExtensionMethods.prg:12` — // todo: Move class to the XSharp namespace and rename to LinqExtensionMethods
- `src/Runtime/XSharp.RT/Functions/OOP.prg:92` — // TOdo Optimize FindClass
- `src/Runtime/XSharp.RT/RDD/Dbfunctions.prg:343` — // Todo: FoxPro allows to lock the header
- `src/Runtime/XSharp.RT/Types/FixedMemory.prg:170` — // TODO: Throw an exception or log the result when FixedMemory.Free fails

### src/Runtime/XSharp.Rdd (14)

- `src/Runtime/XSharp.Rdd/Dbf/DBF.prg:2041` — oResult := NULL     // Todo Add DBI_TRIGGER
- `src/Runtime/XSharp.Rdd/Dbf/DBF.prg:2042` — CASE DbInfo.DBI_DECRYPT         // Todo Add DBI_DECRYPT
- `src/Runtime/XSharp.Rdd/Dbf/DBF.prg:2043` — CASE DbInfo.DBI_ENCRYPT         // Todo Add DBI_ENCRYPT
- `src/Runtime/XSharp.Rdd/Dbf/DBF.prg:2154` — // Todo Add DBRI_RAW..
- `src/Runtime/XSharp.Rdd/Dbf/DBF.prg:2157` — // Todo add DBRI_ENCRYPTED
- `src/Runtime/XSharp.Rdd/DbfCdx/Pages/CdxBranchPage.prg:221` — // TODO Use ArrayCopy for whole block for better performance
- `src/Runtime/XSharp.Rdd/DbfCdx/Pages/CdxBranchPage.prg:248` — // TODO Use MemCopy or ArrayCopy in stead for better performance
- `src/Runtime/XSharp.Rdd/DbfCdx/Pages/CdxLeafPage.prg:518` — // Todo: optimize. We are now expanding the leaves which could be overkill.
- `src/Runtime/XSharp.Rdd/DbfCdx/Pages/CdxLeafPage.prg:728` — // Todo: optimize. We are now expanding the leaves which could be overkill.
- `src/Runtime/XSharp.Rdd/DbfCdx/Pages/CdxLeafPage.prg:763` — // Todo: optimize. We are now expanding the leaves which could be overkill.
- `src/Runtime/XSharp.Rdd/DbfCdx/Pages/CdxLeafPage.prg:779` — // Todo: optimize. We are now expanding and compressing the leaves which could be overkill.
- `src/Runtime/XSharp.Rdd/DbfCdx/Tag/CdxTagCreate.prg:471` — // todo Find all pages of the tag and delete them
- `src/Runtime/XSharp.Rdd/DbfCdx/Tag/CdxTagUpdate.prg:513` — // Todo: Optimization: do not add a page when there is room on the next page
- `src/Runtime/XSharp.Rdd/FlexFile/FlexArea.prg:333` — // Todo: add deleted block to FlexFile deleted blocks list

### src/Runtime/XSharp.SQLRdd (3)

- `src/Runtime/XSharp.SQLRdd/Classes/Connection.prg:285` — // Todo: Check for # of open users and close the connection when no users are left and then throw an exception
- `src/Runtime/XSharp.SQLRdd/RDD/SQLRDD-Orders.prg:139` — // todo: Rebuild Orders in tablemode, query mode uses DBFVFP driver for indices
- `src/Runtime/XSharp.SQLRdd/Support/SqlDbTableCommandBuilder.prg:153` — // todo: Add condition from conditional index

### src/Runtime/XSharp.VFP (11)

- `src/Runtime/XSharp.VFP/ArrayFunctions.prg:346` — // TODO(irwin): serach in loaded assemblies if not found directly
- `src/Runtime/XSharp.VFP/ArrayFunctions.prg:467` — // TODO(irwin): Elements 13, 15 require low-level Win32 VerQueryValue API logic
- `src/Runtime/XSharp.VFP/Database/DbFunctions.prg:376` — // Todo Evaluate FOR clause
- `src/Runtime/XSharp.VFP/DateFunctions.prg:485` — // TODO(irwin): consider using DTOC()
- `src/Runtime/XSharp.VFP/FileFunctions.prg:276` — // todo: when cFileName2 is not empty, then the relative path must be calculated
- `src/Runtime/XSharp.VFP/GetWord.prg:94` — /// ToDo: flagged version, flags set on SetDict? Measure setup time!
- `src/Runtime/XSharp.VFP/GetWord.prg:298` — /// todo: Check new dict+array vs. :clear
- `src/Runtime/XSharp.VFP/LanguageCoreWrappers.prg:103` — // TODO(irwin): revisar de aquí en adelante
- `src/Runtime/XSharp.VFP/LanguageCoreWrappers.prg:339` — // TODO(irwin): functions pending to implement
- `src/Runtime/XSharp.VFP/RuntimeCoreWrappers.prg:69` — // TODO(irwin): functions to check
- `src/Runtime/XSharp.VFP/WorkAreaWrappers.prg:64` — // TODO(irwin): functions pending to implement

### src/Runtime/XSharp.VFP.UI (46)

- `src/Runtime/XSharp.VFP.UI/Column.prg:27` — // Todo
- `src/Runtime/XSharp.VFP.UI/Container.prg:18` — // TODO Check IDynamicProperties -> XSharp.RT
- `src/Runtime/XSharp.VFP.UI/Container.prg:54` — // Todo No Transparency on Panel
- `src/Runtime/XSharp.VFP.UI/Container.prg:62` — // Todo : BorderColor Currently ... do nothing
- `src/Runtime/XSharp.VFP.UI/Container.prg:99` — // TODO !! Warning we are Case-Insensitive here
- `src/Runtime/XSharp.VFP.UI/Cursor.prg:106` — // Todo
- `src/Runtime/XSharp.VFP.UI/Cursor.prg:141` — // Todo : Don't forget to remove the Notify EventHandler...
- `src/Runtime/XSharp.VFP.UI/DataEnvironment.prg:25` — // Todo
- `src/Runtime/XSharp.VFP.UI/DataEnvironment.prg:27` — // Todo
- `src/Runtime/XSharp.VFP.UI/EditBox.prg:26` — // TODO: Implement AddLineFeeds
- `src/Runtime/XSharp.VFP.UI/EditBox.prg:32` — // TODO: Implement AllowTabs
- `src/Runtime/XSharp.VFP.UI/Form.prg:27` — // Todo
- `src/Runtime/XSharp.VFP.UI/Form.prg:29` — // Todo
- `src/Runtime/XSharp.VFP.UI/Form.prg:31` — // Todo
- `src/Runtime/XSharp.VFP.UI/Form.prg:33` — // Todo
- `src/Runtime/XSharp.VFP.UI/Form.prg:40` — // Todo  0 = Modeless; 1 = Modal
- `src/Runtime/XSharp.VFP.UI/Form.prg:42` — // Todo
- `src/Runtime/XSharp.VFP.UI/Form.prg:44` — // Todo
- `src/Runtime/XSharp.VFP.UI/Form.prg:46` — // Todo
- `src/Runtime/XSharp.VFP.UI/Form.prg:48` — // Todo
- `src/Runtime/XSharp.VFP.UI/Form.prg:69` — // Todo Add/Remove the Close item to the System Menu
- `src/Runtime/XSharp.VFP.UI/Generated/VFPContainer.xh:46` — // TODO : Watchout of the cOLEClass !!
- `src/Runtime/XSharp.VFP.UI/Generated/VFPList.xh:25` — // Todo: ItemIDData property
- `src/Runtime/XSharp.VFP.UI/Generated/VFPList.xh:34` — // Todo: ListItem property
- `src/Runtime/XSharp.VFP.UI/Grid.prg:67` — // Todo
- `src/Runtime/XSharp.VFP.UI/Grid.prg:130` — // Todo
- `src/Runtime/XSharp.VFP.UI/Grid.prg:132` — // Todo
- `src/Runtime/XSharp.VFP.UI/Grid.prg:135` — // Todo
- `src/Runtime/XSharp.VFP.UI/Grid.prg:195` — // Todo
- `src/Runtime/XSharp.VFP.UI/Grid.prg:226` — // Todo
- `src/Runtime/XSharp.VFP.UI/Headers/ControlProperties.xh:30` — // Todo
- `src/Runtime/XSharp.VFP.UI/Headers/ControlProperties.xh:34` — // Todo
- `src/Runtime/XSharp.VFP.UI/Headers/ControlProperties.xh:71` — // Todo
- `src/Runtime/XSharp.VFP.UI/Headers/ControlProperties.xh:77` — // Todo
- `src/Runtime/XSharp.VFP.UI/Headers/FontProperties.xh:8` — // Todo
- `src/Runtime/XSharp.VFP.UI/Headers/FontProperties.xh:61` — // Todo
- `src/Runtime/XSharp.VFP.UI/Headers/FontProperties.xh:78` — // Todo
- `src/Runtime/XSharp.VFP.UI/Headers/FontProperties.xh:95` — // Todo
- `src/Runtime/XSharp.VFP.UI/Headers/FontProperties.xh:112` — // Todo
- `src/Runtime/XSharp.VFP.UI/Headers/FontProperties.xh:133` — // Todo
- `src/Runtime/XSharp.VFP.UI/MaskProvider.prg:104` — // TODO : Maybe we should read the SET xxx definitions here ?
- `src/Runtime/XSharp.VFP.UI/Spinner.prg:39` — //Todo: See how we can map this to the NumericUpdown
- `src/Runtime/XSharp.VFP.UI/TextBox.prg:58` — // Todo
- `src/Runtime/XSharp.VFP.UI/TextBox.prg:131` — // TODO : Don't forget that if NODEFAULT has been calledd previously we should mark the Event has handled and return
- `src/Runtime/XSharp.VFP.UI/TextBox.prg:212` — // TODO
- `src/Runtime/XSharp.VFP.UI/Timer.prg:58` — // TODO: Implement Reset

### src/Tools/Mono.Cecil (3)

- `src/Tools/Mono.Cecil/Mono.Cecil/Import.cs:342` — if (name.FullName != reference.FullName) // TODO compare field by field
- `src/Tools/Mono.Cecil/Mono.Cecil/MetadataResolver.cs:390` — // TODO: dimensions
- `src/Tools/Mono.Cecil/Mono.Cecil/MetadataResolver.cs:437` — //TODO: check scope

### src/Tools/SHFB (7)

- `src/Tools/SHFB/XSharpPlugin/XSharpDocChanger.cs:758` — output = regex.Replace(output, TitleFunction, 1); //TODO: perhaps make this lowercase?
- `src/Tools/SHFB/XSharpPlugin/XSharpDocChanger.cs:779` — output = Regex.Replace(output, "(?i)" + hhkFunctionClassPageOld, TitleFunctionClassPageNew); //Todo: change output to lowercase? info: The i flag is for case insensitivity
- `src/Tools/SHFB/XSharpPlugin/XSharpDocsPlugIn.cs:19` — /// TODO: Set your plug-in's unique ID and description in the export attribute below.
- `src/Tools/SHFB/XSharpPlugin/XSharpDocsPlugIn.cs:90` — // TODO: Add and invoke a configuration dialog if you need one.  You will also need to set the
- `src/Tools/SHFB/XSharpPlugin/XSharpDocsPlugIn.cs:112` — // TODO: Add your initialization code here such as reading the configuration data
- `src/Tools/SHFB/XSharpPlugin/XSharpDocsPlugIn.cs:202` — // TODO: If the plug-in hasn't got any disposable resources, this finalizer can be removed
- `src/Tools/SHFB/XSharpPlugin/XSharpDocsPlugIn.cs:217` — // TODO: Dispose of any resources here if necessary

### src/Tools/VFPXPorter (9)

- `src/Tools/VFPXPorter/Source/VFPXPorterLib/CodeConverter.prg:110` — // TODO : Try to do this only once...will be three times faster...
- `src/Tools/VFPXPorter/Source/VFPXPorterLib/CodeConverter.prg:155` — // TODO : Try to do this only once...will be three times faster...
- `src/Tools/VFPXPorter/Source/VFPXPorterLib/CodeConverter.prg:190` — // TODO : Try to do this only once...will be three times faster...
- `src/Tools/VFPXPorter/Source/VFPXPorterLib/SCXVCXItem.prg:110` — // TODO : Maybe rewrite this method, because currently having a replacement (with "<@") cannot be used with "^xx99" rules
- `src/Tools/VFPXPorter/Source/VFPXPorterLib/VSProject.prg:80` — // TODO : set the Framework version as a Setting ??
- `src/Tools/VFPXPorter/Source/VFPXPorterLib/VSProject.prg:153` — // Todo Should we generate a relative path ??
- `src/Tools/VFPXPorter/Source/VFPXPorterLib/XPorterCtrlForm.prg:1087` — // TODO : It would be better to put that in the template, no ?
- `src/Tools/VFPXPorter/Source/VFPXPorterLib/XPorterCtrlForm.prg:1109` — // Todo : If we have more than one grid, prefix the Column Setting with the Grid Name ??
- `src/Tools/VFPXPorter/Source/VFPXPorterLib/XPorterProject.prg:369` — // Todo Use a Extension Method, in order to centralize

### src/Tools/VOXporter (5)

- `src/Tools/VOXporter/Source/Fab_VO_Entities/AEF File.prg:79` — // TODO ExportModule
- `src/Tools/VOXporter/Source/Fab_VO_Entities/MEF File.prg:255` — // TODO Write Export
- `src/Tools/VOXporter/Source/VOXPorter/VOParser.prg:413` — ENUM EntityType AS Int32 // todo need to add delegate, operator
- `src/Tools/VOXporter/Source/VOXPorter/VOParser.prg:1592` — // TODO based on compiler option
- `src/Tools/VOXporter/Source/VOXPorter/VOParser.prg:1603` — // TODO based on compiler option

### src/VisualStudio/AppDesigner (4)

- `src/VisualStudio/AppDesigner/PropertyPages/XDebugPropertyPage.cs:54` — //todo enable / disable controls based on contents
- `src/VisualStudio/AppDesigner/PropertyPages/XDebugPropertyPagePanel.Designer.cs:28` — // todo enable / disable options based on dialect
- `src/VisualStudio/AppDesigner/PropertyPages/XDialectPropertyPage.cs:64` — //todo enable / disable controls based on contents
- `src/VisualStudio/AppDesigner/PropertyPages/XDialectPropertyPagePanel.Designer.cs:28` — // todo enable / disable options based on dialect

### src/VisualStudio/CodeGenerator (4)

- `src/VisualStudio/CodeGenerator/XIDE_BaseBuffer.prg:790` — // TODO based on compiler option
- `src/VisualStudio/CodeGenerator/XIDE_BaseBuffer.prg:801` — // TODO based on compiler option
- `src/VisualStudio/CodeGenerator/XIDE_BaseBuffer.prg:1360` — // TODO: erxetai pote edw???
- `src/VisualStudio/CodeGenerator/XIDE_EntityObject.prg:6` — enum     EntityType as Int32 // todo need to add delegate, operator

### src/VisualStudio/LanguageService (13)

- `src/VisualStudio/LanguageService/Classifier/XSharpClassifier.cs:971` — // Todo:
- `src/VisualStudio/LanguageService/Commands/CommentCommand.cs:195` — // Todo: add check to see if we have a block marked on a single line
- `src/VisualStudio/LanguageService/LanguageService/XSharpLanguageService.cs:348` — // TODO: This should go through the project/analysis and see if we can
- `src/VisualStudio/LanguageService/Library/HierarchyListener.cs:159` — // TODO: Find out if this event is needed.
- `src/VisualStudio/LanguageService/Library/HierarchyListener.cs:202` — // TODO: Find out what this event is about.
- `src/VisualStudio/LanguageService/Library/LibraryNode.cs:606` — // TODO: Use the flags and list type to actually filter the result.
- `src/VisualStudio/LanguageService/Library/LibraryNode.cs:669` — // TODO: make use of the text option.
- `src/VisualStudio/LanguageService/Library/LibraryNode.cs:682` — // TODO: Make use of the tooltip type.
- `src/VisualStudio/LanguageService/LightBulb/ImplementInterfaceAction.cs:121` — // Todo : Check these
- `src/VisualStudio/LanguageService/Lookup/XSharpLookup.cs:356` — //todo what else ?
- `src/VisualStudio/LanguageService/Lookup/XSharpLookup.cs:447` — // TODO: Resolve type lookup Out Parameters
- `src/VisualStudio/LanguageService/MatchingTokens/IdentifierMatching.cs:61` — // Todo: the classifier is now marking open and close keywords with (invisible) classification
- `src/VisualStudio/LanguageService/MatchingTokens/KeywordMatching.cs:123` — // Todo: the classifier is now marking open and close keywords with (invisible) classification

### src/VisualStudio/ProjectBase (25)

- `src/VisualStudio/ProjectBase/Automation/OANavigableProjectItems.cs:133` — // TODO:  Add OAProjectItems.Kind getter implementation
- `src/VisualStudio/ProjectBase/Automation/OANullProperty.cs:47` — //todo: EnvDTE.Property.Collection
- `src/VisualStudio/ProjectBase/Automation/OANullProperty.cs:64` — //todo: let_Value
- `src/VisualStudio/ProjectBase/Automation/OAProjectItems.cs:177` — // TODO: VSADDITEMOP_LINKTOFILE
- `src/VisualStudio/ProjectBase/Automation/OAProperty.cs:52` — //todo: EnvDTE.Property.Collection
- `src/VisualStudio/ProjectBase/Automation/VSProject/OAProjectReference.cs:89` — // TODO: Write the code that finds out the type of the output of the source project.
- `src/VisualStudio/ProjectBase/FolderNode.cs:338` — //TODO - this should not digest all exceptions.
- `src/VisualStudio/ProjectBase/FolderNode.cs:377` — //TODO - this should not digest all exceptions.
- `src/VisualStudio/ProjectBase/Output.cs:82` — path = "file:///" + path; // TODO: does not work with '#' char, see e.g. bug 641942
- `src/VisualStudio/ProjectBase/ProjectConfig.cs:135` — // TODO rationalize this code with callers and ProjectNode.OnHandleConfigurationRelatedGlobalProperties, ProjectNode.TellMSBuildCurrentSolutionConfiguration, etc
- `src/VisualStudio/ProjectBase/ProjectConfig.cs:1317` — supported[0] = 0; // TODO:
- `src/VisualStudio/ProjectBase/ProjectNode.cs:485` — // TODO cache an instance for perf; but be sure not to be stale (correctness)
- `src/VisualStudio/ProjectBase/ProjectNode.cs:3416` — // TODO: Handle source control issues.
- `src/VisualStudio/ProjectBase/ProjectNode.cs:3858` — // TODO: If source control is enabled check out the project file.
- `src/VisualStudio/ProjectBase/ProjectNode.cs:4090` — // TODO : Revisit the VSADDFILEFLAGS here. Can it be a nested project?
- `src/VisualStudio/ProjectBase/ProjectNode.cs:4354` — // TODO: possibly report in the error list that the the item is already contained in the project file similar to Language projects.
- `src/VisualStudio/ProjectBase/ProjectNode.cs:5133` — // TODO: Refactor this code when we support user files
- `src/VisualStudio/ProjectBase/ProjectNode.cs:5140` — // TODO: Refactor this code when we support user files
- `src/VisualStudio/ProjectBase/ProjectNode.cs:5193` — // TODO: Refactor this code when we support user files
- `src/VisualStudio/ProjectBase/ProjectNode.cs:5343` — // TODO: Refactor this code when we support user files
- `src/VisualStudio/ProjectBase/ProjectNode.cs:5447` — // TODO: turn file watcher back on.
- `src/VisualStudio/ProjectBase/ProjectNode.cs:6865` — // TODO: when adding support for User files, we need to update this method
- `src/VisualStudio/ProjectBase/ProjectNode.cs:6922` — // TODO: when adding support for User files, we need to update this method
- `src/VisualStudio/ProjectBase/SettingsPage.cs:492` — this.panel.Visible = true; // TODO: pass SW_SHOW* flags through
- `src/VisualStudio/ProjectBase/SingleFileGenerator.cs:272` — //Todo - Create a file and add it to the Project

### src/VisualStudio/ProjectPackage (7)

- `src/VisualStudio/ProjectPackage/Editors/XSharpVOEditorPane.cs:697` — // TODO:  Add Editor.SaveCompleted implementation
- `src/VisualStudio/ProjectPackage/Editors/XSharpVOEditorPane.cs:1159` — // TODO:  Add VOFormEditorPane.RenameDocData implementation
- `src/VisualStudio/ProjectPackage/FileCodeModel/FileCodeModelBase.cs:36` — // TODO : Move to XSharp.Constants
- `src/VisualStudio/ProjectPackage/Nodes/XSharpFileNode.cs:448` — // Todo: Link imported node with parent node
- `src/VisualStudio/ProjectPackage/XSharpDocumentWatcher.cs:18` — // todo: we can probably delete this code.
- `src/VisualStudio/ProjectPackage/XSharpSingleFileGenerator.cs:45` — // TODO: this should create a task list item
- `src/VisualStudio/ProjectPackage/XSharpSingleFileGenerator.cs:449` — //Todo - Create a file and add it to the Project

### src/VisualStudio/XSharpCodeModelXs (6)

- `src/VisualStudio/XSharpCodeModelXs/MetaData/XAssembly.prg:117` — // Todo: Use the Intellisense database instead of the Namespaces and Types collection.
- `src/VisualStudio/XSharpCodeModelXs/Model/XTypeExtensions.prg:90` — // todo: need to handle type parameters !
- `src/VisualStudio/XSharpCodeModelXs/Model/XTypeMemberExtensions.prg:129` — // todo: need to handle type parameters !
- `src/VisualStudio/XSharpCodeModelXs/Parser/XsParser.prg:904` — // Todo handle Add Object clause inside Class.
- `src/VisualStudio/XSharpCodeModelXs/Parser/XsParser.prg:2959` — // Todo: Get Expression for OUT variable
- `src/VisualStudio/XSharpCodeModelXs/ProjectSystem/XProject.prg:1179` — //todo Collect interfaces from IMPLEMENTS clauses

### src/VisualStudio/XSharpVoEditors (24)

- `src/VisualStudio/XSharpVoEditors/Designers/PropGrid.prg:69` — LOCAL aTemp AS List<STRING> // todo BIG, BAD, UGLY HACK!
- `src/VisualStudio/XSharpVoEditors/Designers/Properties.prg:267` — IF rSize > (REAL4)500 // TODO big bad ugly hack
- `src/VisualStudio/XSharpVoEditors/Designers/Template.prg:542` — DO WHILE cKey:Length > 3 // TODO looks like the VO WED works this way, for all types
- `src/VisualStudio/XSharpVoEditors/Designers/VODBServerEditor.prg:919` — SELF:oGrid:FindForm():Text := "DBServer Editor Properties" // TODO: need to use resource for that
- `src/VisualStudio/XSharpVoEditors/Designers/VOFieldSpecEditor.prg:89` — SELF:oGrid:FindForm():Text := "FieldSpec Editor Properties" // TODO: need to use resource for that
- `src/VisualStudio/XSharpVoEditors/Designers/VOMenuEditor.prg:62` — SELF:oGrid:FindForm():Text := "Menu Editor Properties" // TODO: need to use resource for that
- `src/VisualStudio/XSharpVoEditors/Designers/VOWindowEditor.prg:135` — SELF:oGrid:FindForm():Text := "Window Editor Properties" // TODO: need to use resource for that
- `src/VisualStudio/XSharpVoEditors/Designers/VOWindowEditor.prg:253` — //	PRIVATE METHOD TimerTicked(o AS OBJECT,e AS EventArgs) AS VOID // TODO: should be allowed?
- `src/VisualStudio/XSharpVoEditors/Designers/VOWindowEditor.prg:394` — cLine := "ABCDabcdefghiklmnopqrstuvwxyz" // TODO
- `src/VisualStudio/XSharpVoEditors/Designers/VOWindowEditor.prg:450` — aResource:Add("FONT " + oProp:CodeValue:Substring(3)) // TODO ugly hack
- `src/VisualStudio/XSharpVoEditors/Designers/VOWindowEditor.prg:547` — IF String.IsNullOrWhiteSpace(oPageOptions:cName) .OR. oPageOptions:cCaption == "Page" // TODO Should check if page window exists
- `src/VisualStudio/XSharpVoEditors/Designers/VOWindowEditor.prg:905` — cLine := "ABCDabcdefghiklmnopqrstuvwxyz" // TODO
- `src/VisualStudio/XSharpVoEditors/Designers/VOWindowEditor.prg:961` — aResource:Add("FONT " + oProp:CodeValue:Substring(3)) // TODO ugly hack
- `src/VisualStudio/XSharpVoEditors/Designers/VOWindowEditor.prg:1054` — IF String.IsNullOrWhiteSpace(oPageOptions:cName) .OR. oPageOptions:cCaption == "Page" // TODO Should check if page window exists
- `src/VisualStudio/XSharpVoEditors/Designers/VOWindowEditor.prg:2503` — //					IF SELF:oWindowDesign:lSelected;oPoint := Point{0 , 0};ENDIF // TODO Big, ugly, dirty hack!
- `src/VisualStudio/XSharpVoEditors/Designers/VOWindowEditor.prg:2505` — IF SELF:oWindowDesign:lSelected;oPoint := Point{5 , 5};ENDIF // TODO Big, ugly, dirty hack!
- `src/VisualStudio/XSharpVoEditors/Designers/VOWindowEditor.prg:4461` — // TODO : in the VO WED, the Tab Key property has an empty option
- `src/VisualStudio/XSharpVoEditors/Designers/VOWindowEditor.prg:4520` — IF SELF:GetProperty(cProp) == NULL // TODO code duplication
- `src/VisualStudio/XSharpVoEditors/Designers/VOWindowEditor.prg:4522` — // TODO : in the VO WED, the Tab Key property has an empty option
- `src/VisualStudio/XSharpVoEditors/Designers/VOWindowEditorIO.prg:606` — // TODO: Hack, need to find a better, general way. Below have the default values (no change) so ApplyProperty() is not being invoked
- `src/VisualStudio/XSharpVoEditors/Designers/VOWindowEditorIO.prg:611` — ELSEIF oProp:aEnumValues:Count != 0 // TODO
- `src/VisualStudio/XSharpVoEditors/Designers/VOWindowEditorIO.prg:617` — // TODO: Hack, need to find a better, general way. Below have the default values (no change) so ApplyProperty() is not being invoked
- `src/VisualStudio/XSharpVoEditors/Designers/VOWindowEditorIO.prg:711` — cName += Encoding.Default:GetString(aBytes , n - 1 , 1) // TODO: This really needs improvement
- `src/VisualStudio/XSharpVoEditors/Designers/VOWindowEditorIO.prg:753` — cTab += Encoding.Default:GetString(<BYTE>{nByte} , 0 , 1) // TODO: This really, really, really needs improvement

