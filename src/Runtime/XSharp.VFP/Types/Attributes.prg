USING System
USING System.Collections.Generic
USING System.Text

BEGIN NAMESPACE XSharp.VFP

    /// <include file="XSharp.VFP.Docs.xml" path="doc/FoxEngine/*" />
    PUBLIC ENUM FoxEngine
        RuntimeCore := 0
        LanguageCore := 1
        WorkArea := 2
        DataSession := 3
        SQL := 4
        UI := 5
        Macro := 6
        NameResolution := 7
        Interop := 8
    END ENUM

    /// <include file="XSharp.VFP.Docs.xml" path="doc/FoxCriticality/*" />
    PUBLIC ENUM FoxCriticality
        High := 3
        Medium := 2
        Low := 1
    END ENUM

    /// <include file="XSharp.VFP.Docs.xml" path="doc/FoxFunctionStatus/*" />
    PUBLIC ENUM FoxFunctionStatus
        Full := 0 // Fully supported (same as VFP)
        Partial := 1 // Partially supported (edge cases missed, flags or optional params)
        Changed := 2 // Supported with changes (eg: requires .NET type or behavior differs by design)
        Stub := 3 // Compiles but not functionally implemented
        Obsolete := 4 // Obsolete (exists for retrocompatibility but it's usage is not recommended)
        NotSupported := 5 // It won't be supported in X# (eg: Addls(), Bar(), COMArray(), etc)
    END ENUM

    /// <include file="XSharp.VFP.Docs.xml" path="doc/FoxFunctionCategory/*" />
    PUBLIC ENUM FoxFunctionCategory
        Array := 0
        Bitwise := 1
        ClassAndObject := 2
        CursorAndTable := 3
        Database := 4
        DateAndTime := 5
        EnvironmentAndSystem := 6
        FileAndIO := 7
        Financial := 8
        MathAndNumeric := 9
        StringAndCharacter := 10
        SQL := 11
        UIAndWindow := 12
        General := 13
    END ENUM

    /// <include file="XSharp.VFP.Docs.xml" path="doc/FoxProFunctionAttribute/*" />
    [AttributeUsage(AttributeTargets.Method, AllowMultiple := FALSE, Inherited := FALSE)];
    PUBLIC CLASS FoxProFunctionAttribute INHERIT Attribute

        PROPERTY Name AS STRING AUTO GET PRIVATE SET
        PROPERTY Category AS FoxFunctionCategory AUTO GET PRIVATE SET
        PROPERTY Engine AS FoxEngine AUTO GET PRIVATE SET
        PROPERTY Status AS FoxFunctionStatus AUTO GET PRIVATE SET
        PROPERTY Criticality AS FoxCriticality AUTO GET PRIVATE SET
        PROPERTY Notes AS STRING AUTO GET PRIVATE SET
        PROPERTY IntroducedVersion AS STRING AUTO GET PRIVATE SET
        PROPERTY DeprecatedVersion AS STRING AUTO GET PRIVATE SET

        CONSTRUCTOR(;
            cName AS STRING, ;
            eCategory AS FoxFunctionCategory, ;
            eEngine AS FoxEngine, ;
            eStatus AS FoxFunctionStatus, ;
            eCriticality AS FoxCriticality, ;
            cNotes := "" AS STRING, ;
            cIntroduced := "" AS STRING, ;
            cDeprecated := "" AS STRING)

            SUPER()

            IF String.IsNullOrWhiteSpace(cName)
                THROW ArgumentException{"Function name cannot be empty"}
            ENDIF

            SELF:Name := cName
            SELF:Category := eCategory
            SELF:Engine := eEngine
            SELF:Status := eStatus
            SELF:Criticality := eCriticality
            SELF:Notes := cNotes
            SELF:IntroducedVersion := cIntroduced
            SELF:DeprecatedVersion := cDeprecated
        RETURN
    END CLASS
END NAMESPACE
