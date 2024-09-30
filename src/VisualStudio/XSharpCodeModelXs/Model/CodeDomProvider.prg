using System.CodeDom
using System.CodeDom.Compiler
using XSharp.Settings
using System.IO


#pragma warning disable 618
begin namespace XSharpModel
/// <summary>
/// This class is a wrapper around the CodeDomProvider class that is used by the XSharpCodeModel
/// </summary>
public class XSharpCodeDomProvider INHERIT CodeDomProvider
    private _provider as CodeDomProvider
    public override property FileExtension as String => "prg"
    public override property LanguageOptions AS LanguageOptions GET _provider:LanguageOptions

    public constructor
        var type := XSettings.CodeDomProviderClass
        if type == null
            Throw ArgumentNullException{"CodeDomProviderClass"}
        endif
        var obj := Activator.CreateInstance(type)
        if  obj is CodeDomProvider var provider
            _provider := provider
        else
            Throw ArgumentException{"CodeDomProviderClass, incorrect type", type:FullName}
        endif
    end constructor


    PUBLIC OVERRIDE METHOD CompileAssemblyFromDom(options AS CompilerParameters, compilationUnits AS CodeCompileUnit[]) AS CompilerResults
        return _provider:CompileAssemblyFromDom(options, compilationUnits)
    END METHOD

    PUBLIC OVERRIDE METHOD CompileAssemblyFromFile(options AS CompilerParameters, fileNames AS STRING[]) AS CompilerResults
        return _provider:CompileAssemblyFromFile(options, fileNames)
    END METHOD
    PUBLIC OVERRIDE METHOD CompileAssemblyFromSource(options AS CompilerParameters, sources AS STRING[]) AS CompilerResults
        return _provider:CompileAssemblyFromSource(options, sources)
    END METHOD

    [Obsolete("Callers should not use the ICodeGenerator interface and should instead use the methods directly on the CodeDomProvider class.")];
    public override method CreateCompiler() as ICodeCompiler
        return _provider:CreateCompiler()
    end method

    PUBLIC override METHOD CreateEscapedIdentifier(@@value AS STRING) AS STRING
        return _provider:CreateEscapedIdentifier(@@value)
    END METHOD

    [Obsolete("Callers should not use the ICodeGenerator interface and should instead use the methods directly on the CodeDomProvider class.")];
    public override method CreateGenerator() as ICodeGenerator
        return _provider:CreateGenerator()
    end method


    PUBLIC override METHOD CreateGenerator(fileName AS STRING) AS ICodeGenerator
        return _provider:CreateGenerator(fileName)
    END METHOD

    PUBLIC override METHOD CreateGenerator(output AS System.IO.TextWriter) AS ICodeGenerator
        return _provider:CreateGenerator(output)
    END METHOD

    [Obsolete];
    PUBLIC override METHOD CreateParser() AS ICodeParser
        return _provider:CreateParser()
    END METHOD

    PUBLIC override METHOD CreateValidIdentifier(@@value AS STRING) AS STRING
        return _provider:CreateValidIdentifier(@@value)
    END METHOD

    PUBLIC override METHOD GenerateCodeFromCompileUnit(compileUnit AS CodeCompileUnit, writer AS TextWriter, options AS CodeGeneratorOptions) AS VOID
        _provider:GenerateCodeFromCompileUnit(compileUnit, writer, options)
    END METHOD

    PUBLIC override METHOD GenerateCodeFromExpression(expression AS CodeExpression, writer AS TextWriter, options AS CodeGeneratorOptions) AS VOID
        _provider:GenerateCodeFromExpression(expression, writer, options)
    END METHOD

    PUBLIC override METHOD GenerateCodeFromMember(@@member AS CodeTypeMember, writer AS TextWriter, options AS CodeGeneratorOptions) AS VOID
        _provider:GenerateCodeFromMember(@@member, writer, options)
    END METHOD

    PUBLIC OVERRIDE METHOD GenerateCodeFromNamespace(codeNamespace AS CodeNamespace, writer AS TextWriter, options AS CodeGeneratorOptions) AS VOID
        _provider:GenerateCodeFromNamespace(codeNamespace, writer, options)
    END METHOD

    PUBLIC OVERRIDE METHOD GenerateCodeFromStatement(statement AS CodeStatement, writer AS TextWriter, options AS CodeGeneratorOptions) AS VOID
        _provider:GenerateCodeFromStatement(statement, writer, options)
    END METHOD

    PUBLIC OVERRIDE METHOD GenerateCodeFromType(codeType AS CodeTypeDeclaration, writer AS TextWriter, options AS CodeGeneratorOptions) AS VOID
        _provider:GenerateCodeFromType(codeType, writer, options)
    END METHOD
    public override method GetConverter (type as System.Type) as System.ComponentModel.TypeConverter
        return _provider:GetConverter(type)
    end method
    PUBLIC OVERRIDE METHOD GetTypeOutput(type AS CodeTypeReference) AS STRING
        return _provider:GetTypeOutput(type)
    END METHOD

    PUBLIC OVERRIDE METHOD IsValidIdentifier(@@value AS STRING) AS LOGIC
        return _provider:IsValidIdentifier(@@value)
    END METHOD

    PUBLIC OVERRIDE METHOD Parse(codeStream AS TextReader) AS System.CodeDom.CodeCompileUnit
        return _provider:Parse(codeStream)
    END METHOD
    PUBLIC OVERRIDE METHOD Supports(generatorSupport AS GeneratorSupport) AS LOGIC
        return _provider:Supports(generatorSupport)
    END METHOD

end class
end namespace
