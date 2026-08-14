﻿//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

#pragma options("az", on)


using XSharp.Internal
using System.Reflection
using System.Reflection.Emit
using System.Collections.Generic
using System.Collections.Concurrent
using System.Linq
using System.Text
using System.Runtime.CompilerServices
using System.Diagnostics

// Everything that FindBestOverLoad() and CompareMethods() need to know about one candidate.
// GetParameters() returns a new array on every call and IsDefined() and the default values of the
// parameters are reflection lookups as well, so this is collected once per candidate instead of
// inside the loop that compares every candidate with every other candidate
internal sealed class __OverLoadInfo
    // Method, Member and Parameters are all keywords, hence the abbreviated names
    internal Mb          as MethodBase
    internal DeclType    as System.Type
    internal Pars        as ParameterInfo[]
    internal IsClipper   as logic
    private  _nonDefault as long        // -1 as long as it has not been calculated

    internal constructor(m as MethodBase)
        self:Mb          := m
        self:DeclType    := m:DeclaringType
        self:Pars        := m:GetParameters()
        self:IsClipper   := m:IsDefined(OOPHelpers.clipperCallType, false)
        self:_nonDefault := -1
        return

    // The number of parameters that have no default value. Calculated when it is first asked for,
    // because the most common cases are decided on the number of parameters alone
    internal property NonDefault as long
        get
            if self:_nonDefault < 0
                self:_nonDefault := OOPHelpers.CountNonDefaultParameters(self:Pars)
            endif
            return self:_nonDefault
        end get
    end property
end class

internal static class OOPHelpers
    static internal aXsAssemblies  as HashSet<Assembly>
    static internal EnableOptimizations as logic
    static internal cacheClassesAll as ConcurrentDictionary<string,Type>
    static internal cacheClassesOurAssemblies as ConcurrentDictionary<string,Type>
    static internal fieldPropCache    as ConcurrentDictionary<System.Type, ConcurrentDictionary<string, MemberInfo> >
    static internal overloadCache     as ConcurrentDictionary<System.Type, ConcurrentDictionary<string, IList<MethodInfo>> >
    // The assemblies with a ClassLibraryAttribute and the clipper functions inside these assemblies
    // are cached. Both caches are invalidated when a new assembly is loaded into the AppDomain.
    // asmGeneration is used to detect that an assembly was loaded while we were filling a cache
    static internal ourAssemblies     as Assembly[]
    static internal clipperFuncCache  as ConcurrentDictionary<string, MethodInfo[]>
    static internal asmGeneration     as long
    // Conversion operators are members of a type, so they never change. Both the operators that were
    // found and the fact that a type has no operator for a certain target type are worth caching
    static internal operatorCache     as ConcurrentDictionary<System.Type, ConcurrentDictionary<System.Type, MethodInfo> >
    // typeof() is a runtime call, so the types and attributes that are needed inside loops are resolved once
    static internal usualType         as System.Type
    static internal usualArrayType    as System.Type
    static internal objectType        as System.Type
    static internal stringType        as System.Type
    static internal arrayType         as System.Type
    static internal codeblockType     as System.Type
    static internal floatType         as System.Type
    static internal dateType          as System.Type
    static internal symbolType        as System.Type
    static internal ptrType           as System.Type
    static internal paramArrayType    as System.Type
    static internal clipperCallType   as System.Type
    static internal classLibraryType  as System.Type
    static internal implicitNsType    as System.Type
    static internal rtAssembly        as Assembly
    // The namespaces that FindClass() prefixes to the classname, in the order in which they are tried
    static internal searchNameSpaces  as string[]
    // The runtime assemblies in the order of their dependencies. Used to decide which overload wins
    // when the same method is defined in more than one of them. The index per assembly is cached
    // because GetName() allocates a new AssemblyName on every call
    static internal ourAssemblyNames  as string[]
    static internal asmIndexCache     as ConcurrentDictionary<Assembly, long>
    static constructor()
        cacheClassesAll             := ConcurrentDictionary<string,Type>{StringComparer.OrdinalIgnoreCase}
        cacheClassesOurAssemblies   := ConcurrentDictionary<string,Type>{StringComparer.OrdinalIgnoreCase}
        fieldPropCache              := ConcurrentDictionary<System.Type, ConcurrentDictionary<string, MemberInfo> >{}
        overloadCache               := ConcurrentDictionary<System.Type, ConcurrentDictionary<string, IList<MethodInfo>> >{}
        aXsAssemblies               := HashSet<Assembly>{}
        clipperFuncCache            := ConcurrentDictionary<string, MethodInfo[]>{StringComparer.OrdinalIgnoreCase}
        operatorCache               := ConcurrentDictionary<System.Type, ConcurrentDictionary<System.Type, MethodInfo> >{}
        dynamicMethodCache          := ConcurrentDictionary<MethodInfo, Func<object, object[], object> >{}
        ourAssemblies               := null
        asmGeneration               := 0
        asmIndexCache               := ConcurrentDictionary<Assembly, long>{}
        usualType                   := typeof(usual)
        usualArrayType              := typeof(usual[])
        objectType                  := typeof(object)
        stringType                  := typeof(System.String)
        arrayType                   := typeof(XSharp.__Array)
        codeblockType               := typeof(XSharp.Codeblock)
        floatType                   := typeof(float)
        dateType                    := typeof(date)
        symbolType                  := typeof(symbol)
        ptrType                     := typeof(ptr)
        paramArrayType              := typeof(ParamArrayAttribute)
        clipperCallType             := typeof(ClipperCallingConventionAttribute)
        classLibraryType            := typeof(ClassLibraryAttribute)
        implicitNsType              := typeof(ImplicitNamespaceAttribute)
        rtAssembly                  := typeof(__Usual):Assembly     // XSharp.RT
        searchNameSpaces            := <string>{"","System.","XSharp.", "XSharp.Internal."}
        ourAssemblyNames            := <string>{"xsharp.core", "xsharp.rt", "xsharp.vo", "xsharp.vfp", "xsharp.xpp", "xsharp.harbour"}
        AppDomain.CurrentDomain:AssemblyLoad += OnAssemblyLoad
        return

    // A newly loaded assembly may contain classes and clipper functions that we have not seen before,
    // so the caches that depend on the list of loaded assemblies have to be thrown away
    static method OnAssemblyLoad(sender as object, args as AssemblyLoadEventArgs) as void
        System.Threading.Interlocked.Increment(ref asmGeneration)
        ourAssemblies := null
        clipperFuncCache:Clear()
        // The classes that we did find are still valid, but the new assembly may contain a class
        // that we have been looking for before without success
        RemoveNegativeEntries(cacheClassesAll)
        RemoveNegativeEntries(cacheClassesOurAssemblies)
        return

    static method RemoveNegativeEntries(cache as ConcurrentDictionary<string, Type>) as void
        var keys := List<string>{}
        foreach var pair in cache
            if pair:Value == null
                // do not delete inside the foreach, this may throw an exception
                keys:Add(pair:Key)
            endif
        next
        foreach var strKey in keys
            cache:TryRemove(strKey, out var _)
        next
        return

    static method FindOurAssemblies as IEnumerable<Assembly>
        var result := ourAssemblies
        if result == null
            var gen  := asmGeneration
            var cla  := typeof( ClassLibraryAttribute )
            var list := List<Assembly>{}
            foreach asm as Assembly in AppDomain.CurrentDomain:GetAssemblies()
                if asm:IsDefined(cla, false)
                    list:Add(asm)
                endif
            next
            result := list:ToArray()
            if gen == asmGeneration
                // no assembly was loaded while we were building the list, so we can cache it
                ourAssemblies := result
            endif
        endif
        return result

    static method MethodMatches(m as MethodInfo, cName as STRING) AS LOGIC
        if m:IsSpecialName
            RETURN FALSE
        endif
        return String.Equals(m:Name, cName, StringComparison.OrdinalIgnoreCase)

    static method FindClipperFunctions(cFunction as string) as MethodInfo[]
        if String.IsNullOrEmpty(cFunction)
            return MethodInfo[]{0}
        endif
        // Looking up a function means walking all our assemblies and doing several reflection
        // calls per assembly, so the result is cached. _HasClipFunc() and _CallClipFunc() are
        // often called right after each other for the same name
        var cache := clipperFuncCache
        if cache:TryGetValue(cFunction, out var cached)
            return cached
        endif
        var gen := asmGeneration
        var cla := typeof( ClassLibraryAttribute )
        local aMethods as List<MethodInfo>
        aMethods := List<MethodInfo>{}
        foreach asm as Assembly in OOPHelpers.FindOurAssemblies()
            local atr := (ClassLibraryAttribute) (asm:GetCustomAttributes(cla,false):First()) as ClassLibraryAttribute
            local oType as System.Type
            oType := asm:GetType(atr:GlobalClassName,false, true)
            if oType != null_object
                local oMI as MethodInfo
                local bf as BindingFlags
                bf := BindingFlags.Static | BindingFlags.IgnoreCase | BindingFlags.Public | BindingFlags.DeclaredOnly
                try
                    oMI := oType:GetMethod(cFunction,bf)
                    if oMI != null_object
                        aMethods:Add( (MethodInfo) oMI)
                    endif
                catch as AmbiguousMatchException
                    local aMI as MethodInfo[]
                    var list := OOPHelpers.GetCachedOverLoads(oType, cFunction)
                    if list != null
                        aMethods:AddRange(list)
                    else
                        list := OOPHelpers.FindOverloads(oType, cFunction, false)
                        aMI := oType:GetMethods(bf)
                        foreach oM as MethodInfo in aMI
                            if MethodMatches(oM, cFunction)
                                if ! list:Contains(oM)
                                    list:Add( oM )
                                endif
                            endif
                        next
                        if list:Count > 0
                            aMethods:AddRange(list)
                        endif
                    endif
                end try
            endif
        next
        var result := aMethods:ToArray()
        if gen == asmGeneration
            // no assembly was loaded while we were looking, so the result is still valid
            cache:TryAdd(cFunction, result)
        endif
        return result


    static method FindClass(cName as string) as System.Type
        return OOPHelpers.FindClass(cName, true)

    static method FindClass(cName as string, lOurAssembliesOnly as logic) as System.Type
        local ret := null as System.Type
        local cache as ConcurrentDictionary<string,Type>
        local aAssemblies as IEnumerable<Assembly>

        if String.IsNullOrWhiteSpace(cName)
            // otherwise asm:GetType() will throw an exception with empty name
            return ret
        end if

        if lOurAssembliesOnly
            cache := cacheClassesOurAssemblies
        else
            cache := cacheClassesAll
        end if
        // A NULL in the cache means that we have looked for this class before and did not find it.
        // That is worth remembering: the search below walks all assemblies and may even have to
        // enumerate all their types. The negative entries are removed when an assembly is loaded
        if cache:TryGetValue(cName, out ret)
            return ret
        end if
        if lOurAssembliesOnly
            aAssemblies := OOPHelpers.FindOurAssemblies()
        else
            aAssemblies := AppDomain.CurrentDomain:GetAssemblies()
        end if
        var gen := asmGeneration
        var ns  := searchNameSpaces

        foreach asm as Assembly in aAssemblies
            FOREACH var n in ns
                var cFullName := n + cName
                ret := asm:GetType( cFullName, false, true )
                if ret != null
                    exit
                endif
            NEXT
            if ret != null
                exit
            endif
            // The class could be prefixed with a Namespace.
            // If there is a class library attribute and we prefixed all classes with a namespace then
            // this is visible in the ClassLibraryAttribute
            // We don't know if the current assembly is compiler with /INS, but we assume it is when they
            // use the 'old fashioned' CreateInstance().
            var att := classLibraryType
            if asm:IsDefined(  att, false )
                // there should be only one but it does not hurt to be cautious
                foreach var attribute in asm:GetCustomAttributes(att,false)
                    var cla := (ClassLibraryAttribute) attribute
                    if !String.IsNullOrEmpty(cla:DefaultNameSpace)
                        var cFullName := cla:DefaultNameSpace +"."+cName
                        ret := asm:GetType( cFullName, false, true )
                        if ret != null
                            exit
                        endif
                    endif
                next
            endif
            if ret != null
                exit
            end if
            // If there is an Implicit Namespace Attribute
            att := implicitNsType
            if asm:IsDefined(  att, false )
                foreach var attribute in asm:GetCustomAttributes(att,false)
                    var ins := (ImplicitNamespaceAttribute) attribute
                    if !String.IsNullOrEmpty(ins:Namespace)
                        var cFullName := ins:Namespace+"."+cName
                        ret := asm:GetType( cFullName, false, true )
                        if ret != null
                            exit
                        endif
                    endif
                next
            endif
            if ret != null
                exit
            endif
        next
        if ret == null
            // try to find classes in a namespace. This forces all types of the assembly to be loaded,
            // so it is the most expensive part of this method
            var cla := classLibraryType
            foreach asm as Assembly in aAssemblies
                if asm:IsDefined(  cla, false )
                    var types := asm:GetTypes()
                    foreach type as System.Type in types
                        if String.Equals(type:Name, cName, StringComparison.OrdinalIgnoreCase)
                            ret := type
                            exit
                        endif
                    next
                    if ret != null
                        exit
                    endif
                endif
            next
        endif

        if gen == asmGeneration
            // No assembly was loaded while we were searching. Also cache a NULL result, so the next
            // lookup for this name does not have to walk all assemblies again
            cache:TryAdd(cName , ret)
        end if

        return ret

    static method FindMethod(t as System.Type, cName as string, lSelf as logic, lInstance := true as logic ) as MethodInfo
     local oMI := null as MethodInfo

        if t == null .or. String.IsNullOrEmpty(cName)
            return null
        end if

        try
            var bf := BindingFlags.IgnoreCase | BindingFlags.Public
            if lSelf
                bf |= BindingFlags.NonPublic
            else
                bf |= BindingFlags.Public
            endif
            if lInstance
                bf |= BindingFlags.Instance
            else
                bf |= BindingFlags.Static
            endif
            oMI := t:GetMethod(cName, bf)
        catch as System.Reflection.AmbiguousMatchException
            oMI := null
        end try

        return oMI

    static method CompareMethods(t as System.Type, m1 as __OverLoadInfo, m2 as __OverLoadInfo, uArgs as usual[]) as long
        if (m1:DeclType != m2:DeclType)
            if m1:DeclType == t .and. m1:Mb:IsHideBySig
                return 1
            endif
            if m2:DeclType == t .and. m2:Mb:IsHideBySig
                return 2
            endif
        endif
        var p1 := m1:Pars
        var p2 := m2:Pars
        var n1 := m1:NonDefault
        var n2 := m2:NonDefault
        if n1 != n2
            if n1 == uArgs:Length
                return 1
            elseif n2 == uArgs:Length
                return 2
            else
                return 0
            endif
        endif
        // when we get here then the parameter counts are the same
        for var nPar := 0 to p1:Length-1
            if nPar > uArgs:Length-1
                exit
            endif
            var par1 := p1[nPar]
            var par2 := p2[nPar]
            var parType1 := par1:ParameterType
            var parType2 := par2:ParameterType
            var arg  := uArgs[nPar]
            if  parType1 != parType2
                if parType1:IsAssignableFrom(arg:SystemType)
                    return 1
                endif
                if parType2:IsAssignableFrom(arg:SystemType)
                    return 2
                endif
                if parType1 = usualType
                    return 1
                endif
                if parType2 = usualType
                    return 2
                endif
            endif
        next
        var type1 := m1:DeclType
        var type2 := m2:DeclType
        if (type1 != type2)
            if type1:IsAssignableFrom(type2)
                return 2
            elseif type2:IsAssignableFrom(type1)
                return 1
            else
                var asm1 := type1:Assembly
                var asm2 := type2:Assembly
                return ResolveByAssembly(asm1, asm2)
            endif
        endif


        return 0
    static method IndexOfOurAssembly(asm as Assembly) as long
        if asmIndexCache:TryGetValue(asm, out var result)
            return result
        endif
        var name := asm:GetName():Name
        var idx  := -1
        for var i := 0 upto ourAssemblyNames:Length-1
            if String.Equals(ourAssemblyNames[i], name, StringComparison.OrdinalIgnoreCase)
                idx := i
                exit
            endif
        next
        asmIndexCache:TryAdd(asm, idx)
        return idx

    static method ResolveByAssembly(asm1 as Assembly, asm2 as Assembly) as long
        // this is called from the O(n*n) loop in FindBestOverLoad(), so it must not allocate
        var idx1 := IndexOfOurAssembly(asm1)
        var idx2 := IndexOfOurAssembly(asm2)
        if idx1 >= 0 .and. idx2 >= 0
            if idx1 > idx2
                return 1
            elseif idx2 > idx1
                return 2
            endif
        endif
        return 0

    /// <include file="XSharp.RT.Docs.xml" path="doc/OOPHelpers.ConvertFromNull/*" />
    static method ConvertFromNull(type as System.Type) as usual
        if type == stringType
            return __Usual{__UsualType.String, false}
        elseif type == arrayType
            return __Usual{__UsualType.Array, false}
        elseif type == codeblockType
            return __Usual{__UsualType.Codeblock, false}
        elseif type:IsValueType
            return NIL
        endif
        return NULL_OBJECT

    // Does GetDefaultValue() return a value other than NULL for this parameter ? This is a lot cheaper
    // than calling GetDefaultValue() itself, because the value does not have to be built and converted
    static method HasDefaultValue(oPar as ParameterInfo) as logic
        if oPar:HasDefaultValue
            return oPar:DefaultValue != null
        endif
        local oDefAttrib as DefaultParameterValueAttribute
        oDefAttrib := (DefaultParameterValueAttribute) oPar:GetCustomAttribute(typeof(DefaultParameterValueAttribute))
        if oDefAttrib == null
            return false
        endif
        switch oDefAttrib:Flag
        case 1  // NIL, and that results in a NULL value, just like a missing attribute
            return false
        case 2  // DATE
        case 3  // SYMBOL
        case 4  // NULL_PSZ
        case 5  // NULL_PTR
        case 6  // Decimal
            return true
        otherwise
            return oDefAttrib:Value != null
        end switch

    /// <include file="XSharp.RT.Docs.xml" path="doc/OOPHelpers.CountNonDefaultParameters/*" />
    static method CountNonDefaultParameters(pars as IList<ParameterInfo>) as long
        // We only need to know if a parameter has a default value, so we do not call GetDefaultValue()
        // here: that would also convert the value to the type of the parameter and that is not needed
        for var i := 0 upto pars:Count -1
            if OOPHelpers.HasDefaultValue(pars[i])
                return i
            endif
        next
        return pars:Count

    static method FindBestOverLoad<T>(t as System.Type, overloads as IList<T>, cFunction as string, uArgs as usual[]) as T where T is MethodBase
        if overloads:Count <= 1
            return overloads:FirstOrDefault()
        endif
        // More than one. Collect the reflection info for every candidate once, in the same order as
        // the overloads, so we can map a candidate back to the overload that it belongs to
        var infos := List<__OverLoadInfo>{overloads:Count}
        foreach var m in overloads
            infos:Add(__OverLoadInfo{m})
        next
        // first look for methods with the same ! of parametes
        var found := List<__OverLoadInfo>{}
        foreach var info in infos
            if info:IsClipper
                found:Add(info)
            elseif info:Pars:Length == uArgs:Length
                found:Add(info)
            elseif info:Pars:Length > 0
                // check to see if there are default parameters for the method
                if uArgs:Length >= info:NonDefault
                    found:Add(info)
                endif
            endif
        next
        if found:Count == 1
            return overloads[infos:IndexOf(found[0])]
        endif
        // then compare the candidates with each other. A HashSet, because this loop adds and removes
        // candidates repeatedly and Contains() and Remove() on a List are linear scans
        var winners := HashSet<__OverLoadInfo>{}
        foreach var m1 in found
            foreach var m2 in found
                if (m2 != m1)
                    var result := OOPHelpers.CompareMethods(t, m1, m2, uArgs)
                    if result == 1
                        winners:Add(m1)
                        winners:Remove(m2)
                    elseif result == 2
                        winners:Add(m2)
                        winners:Remove(m1)
                    endif
                endif
            next
        next
        if winners:Count == 1
            return overloads[infos:IndexOf(winners:First())]
        endif
        local cClass as string
        cClass := overloads:First():DeclaringType:Name
        var oError := Error.VOError( EG_AMBIGUOUSMETHOD, cFunction, "MethodName", 1, <object>{cClass+":"+overloads:First():Name})

        local sb as StringBuilder
        sb := StringBuilder{}
        sb:AppendLine(oError:Message)
        sb:AppendLine(i"Found {winners:Count} overloads")
        var current := 0

        foreach var info in winners
            var overload := info:Mb
            current += 1
            sb:Append( ei"{current}. {overload:DeclaringType:Name}:{overload:Name}")
            if overload:IsGenericMethod
                var args := overload:GetGenericArguments()
                if (args != null .and. args:Length > 0)
                    sb:Append(  "<")
                    var firstArg := true
                    foreach var type in args
                        if firstArg
                            firstArg := false
                        else
                            sb:Append( ", ")
                        endif
                        sb:Append( type:Name)
                    next
                    sb:Append( ">")
                endif
            endif
            sb:Append( "(")

            var firstParam := true
            foreach p as ParameterInfo in overload:GetParameters()
                if firstParam
                    firstParam := false
                else
                    sb:Append(  ", ")
                endif
                sb:Append( p:Name+" AS "+GetTypename(p:ParameterType))
            next
            sb:AppendLine(")")
        next
        oError:Description := sb:ToString()
        throw oError

    static method GetTypename(t as System.Type) as string
        switch t:Name
        case "__Array"
            return "ARRAY"
        case "__Binary"
            return "BINARY"
        case "__Currency"
            return "CURRENCY"
        case "__Date"
            return "DATE"
        case "__Float"
            return "FLOAT"
        case "__FoxArray"
            return "ARRAY"
        case "__Psz"
            return "PSZ"
        case "__Symbol"
            return "SYMBOL"
        case "__Usual"
            return "USUAL"
        case "__VoDate"
            return "DATE"
        case "__VoFloat"
            return "FLOAT"
        end switch
        return t:Name

    static method MatchParameters<T>( methodinfo as T, args as usual[], hasByRef out logic) as object[] where T is MethodBase
        // args contains the list of arguments. The methodname has already been deleted when appropriated
        local oArgs as object[]
        local lClipper := false as logic
        local lParams  := false as logic
        local paramsType  := NULL as System.Type
        local elementType := NULL  as System.Type
        hasByRef := false
        var aPars := methodinfo:GetParameters()
        var numDefinedParameters := aPars:Length
        var numActualParameters  := args:Length
        if numDefinedParameters == 1 .and. methodinfo:IsDefined(clipperCallType,false)
            lClipper := true
        elseif numDefinedParameters >= 1
            local pi := aPars[aPars:Length-1] as ParameterInfo

            if pi:ParameterType:IsArray .and. pi:IsDefined(paramArrayType, false)
                lParams := true
                lClipper := numDefinedParameters == 1 .and. pi:ParameterType == usualArrayType
                paramsType  := pi:ParameterType
                elementType := paramsType:GetElementType()
            endif
        endif
        do case
        case lClipper
            // pass the whole array of clipper parameters (usual[]) as single parameter
            oArgs  := <object>{args}
        case aPars:Length == 0
            // no args
            oArgs := null
        otherwise
            // convert args to array of objects
            oArgs := object[]{numDefinedParameters}
            if numDefinedParameters <= numActualParameters
                // ignore extra parameters
                if lParams
                    local numFixedParameters := numDefinedParameters -1 as long
                    var oParamArgs := System.Array.CreateInstance(elementType, numActualParameters -numFixedParameters) astype System.Array
                    local nCounter := 0 as long
                    for var i := numFixedParameters to numActualParameters - 1
                        var element := OOPHelpers.ValueConvert(args[i], elementType)
                        oParamArgs:SetValue(element,nCounter)
                        nCounter += 1
                    next
                    args[numDefinedParameters -1] := oParamArgs
                    numActualParameters := numDefinedParameters
                else
                    numActualParameters := numDefinedParameters
                endif
            elseif lParams .and. numActualParameters == numDefinedParameters -1
                var oParamArgs := System.Array.CreateInstance(elementType, 0) astype System.Array
                oArgs[numDefinedParameters -1] := oParamArgs
            else
                //var oError :=  Error.VOError( EG_ARG, __function__, methodinfo:Name, (DWORD) numDefinedParameters, args:ToObjectArray())
                //oError:Description := "Not enough parameters for method "+methodinfo:Name
                //throw oError
                NOP
            endif
            for var nPar := 0 to numActualParameters -1
                local pi        := aPars[nPar] as ParameterInfo
                local parType   := pi:ParameterType as System.Type
                local arg       := args[nPar] as usual
                if parType:IsByRef
                    // Get the type that the ByRef parameter refers to. GetElementType() gives us that
                    // directly, without building the name of the type and looking it up in an assembly
                    hasByRef := true
                    var referencedType := parType:GetElementType()
                    if referencedType != null
                        parType := referencedType
                    endif
                endif
                if parType == usualType
                    // We need to box a usual here
                    oArgs[nPar] := __castclass(object, arg)
                elseif arg == nil // this is also true when arg == NULL_OBJECT
                    // This is new in X#: a NIL in the middle of the parameter list gets set to the default value now
                    oArgs[nPar] := OOPHelpers.GetDefaultValue(pi)
                elseif parType == arg:Value:GetType()
                    oArgs[nPar] := arg
                elseif arg == null .or. parType:IsAssignableFrom(arg:SystemType) // Null check must appear first !
                    oArgs[nPar] := arg
                elseif pi:IsDefined( paramArrayType, false )
                    // Parameter array of certain type
                    // -> convert remaining elements from uArgs to an array and assign that to oArgs[i]
                    local aVarArgs    := System.Array.CreateInstance(elementType, args:Length - nPar +1) as System.Array
                    for var nArg := nPar to numActualParameters -1
                        try
                            if elementType:IsAssignableFrom(args[nArg]:SystemType)
                                aVarArgs:SetValue(args[nArg], nArg-nPar)
                            else
                                aVarArgs:SetValue(OOPHelpers.ValueConvert(args[nArg], elementType), nArg-nPar)
                            endif
                        catch
                            aVarArgs:SetValue(null, nArg-nPar)
                        end try
                    next
                    oArgs[nPar] := aVarArgs
                    exit    // done with parameters
                else

                    // try to convert to the expected type, but don't do this for out parameters.
                    // We can leave the slot empty for out parameters
                    if ! pi:IsOut
                        oArgs[nPar]  := OOPHelpers.ValueConvert(args[nPar], parType)
                    endif
                endif
            next
            // set default values for missing parameters, so we start after the last parameter
            if ! lParams
                for var nArg := numActualParameters to numDefinedParameters -1
                    local oPar as ParameterInfo
                    oPar        := aPars[nArg]
                    var oArg    := OOPHelpers.GetDefaultValue(oPar)
                    if oArg != null
                        oArgs[nArg] := oArg
                    else
                        oArgs[nArg] := null
                    endif
                next
            ENDIF
        endcase
        return oArgs

    static method GetDefaultValue(oPar as ParameterInfo) as object
        local result := null as object
        if oPar:HasDefaultValue
            result := oPar:DefaultValue
        else
            local oDefAttrib as DefaultParameterValueAttribute
            oDefAttrib := (DefaultParameterValueAttribute) oPar:GetCustomAttribute(typeof(DefaultParameterValueAttribute))
            if oDefAttrib != null
                switch oDefAttrib:Flag
                case 1 // NIL
                    nop // it is already NIL
                case 2 // DATE, stored in Ticks
                    result := date{ (int64)oDefAttrib:Value }
                case 3 // SYMBOL
                    result := String2Symbol( (string)oDefAttrib:Value )
                case 4 // NULL_PSZ
                    if oDefAttrib:Value is string var strValue
                        // Note: Do not use String2Psz() because that PSZ will be freed when this method finishes !
                        result := psz{ strValue }
                    else
                        result := psz{IntPtr.Zero}
                    endif
                case  5 // NULL_PTR
                    if oDefAttrib:Value is Int32
                        result := IntPtr{ (Int32) oDefAttrib:Value}
                    else
                        result := IntPtr.Zero
                    endif
                case 6 // Decimal value stored as string, without the 'm' suffix
                    if oDefAttrib:Value is string var strDecimal
                        result := System.Decimal.Parse(strDecimal, System.Globalization.CultureInfo.InvariantCulture)
                    else
                        result := 0.0m
                    endif

                otherwise
                    result := oDefAttrib:Value
                end switch
            end if
        endif
        if result != null
            // convert to the correct type
            result := OOPHelpers.ValueConvert(result, oPar:ParameterType)
        endif
        return result

    static method IsMethod( t as System.Type, cName as string ) as logic
        local lResult := false as logic
        lResult := OOPHelpers.FindMethod(t, cName, true) != null
        if ! lResult
            var overloads := OOPHelpers.GetCachedOverLoads(t, cName)
            if overloads == null
                overloads := OOPHelpers.FindOverloads(t, cName, true)
            endif
            lResult := overloads != null .and. overloads:Count > 0
        endif
        return lResult


    static method ClassTree( t as Type ) as array
        local aList := {} as array
        do while t != null
            AAdd( aList, (symbol) t:Name)
            t := t:BaseType
        enddo

        return aList

    static method GetMembers(t as System.Type, cName as string, memberType as MemberTypes, lInstance as LOGIC, lSelf as LOGIC) as MemberInfo[]
        var flags := BindingFlags.IgnoreCase| BindingFlags.Public
        if (lInstance)
            flags := flags | BindingFlags.Instance
        else
            flags := flags | BindingFlags.Static
        endif
        if (lSelf)
            flags := flags | BindingFlags.NonPublic
        endif
        // GetMember returns all members that match the name
        return t:GetMember(cName, memberType, flags)

    static method GetField(t as System.Type, cName as string, lInstance as LOGIC, lSelf as LOGIC) as FieldInfo
        var members := GetMembers(t, cName, MemberTypes.Field,lInstance, lSelf)
        foreach var m in members
            if m is FieldInfo var fldInfo
                return fldInfo
            endif
        next
        return null

    static method GetProperty(t as System.Type, cName as string, lInstance as LOGIC, lSelf as LOGIC) as PropertyInfo
        var members := GetMembers(t, cName, MemberTypes.Property,lInstance, lSelf)
        foreach var m in members
            if m is PropertyInfo var propInfo
                return propInfo
            endif
        next
        return null
    static method IVarHelper(o as object, cName as string, lGet as logic) as dword

        if o == null
            return 0
        endif

        var t := o:GetType()

        var fi := OOPHelpers.GetField(t, cName, true, true)
        if fi != null
            if fi:IsPublic
                return 2U
            elseif fi:IsFamily
                var att := typeof( XSharp.Internal.IsInstanceAttribute )
                var atts := fi:GetCustomAttributes(att,false)
                if (atts:Length > 0)
                    return 1U
                endif
            endif
            return 0U
        endif

        do while t != null
            var pi :=  OOPHelpers.GetProperty(t, cName, true, true)
            if pi != null
                if lGet .and. pi:CanRead
                    return 3U
                endif
                if ! lGet .and. pi:CanWrite
                    return 3U
                endif
            endif
            t := t:BaseType
        enddo

        return 0U

    static method IVarList( t as Type ) as array
        if t == null
            return null_array
        endif
        // Note that VO only returns PUBLIC properties and fields
        var aFields := t:GetFields( BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic)
        // the list keeps the order in which the members were found, the set is there to detect the
        // duplicates without a linear scan through the list for every member
        var list := List<string>{}
        var seen := HashSet<string>{}
        foreach fi as FieldInfo in aFields
            if fi:IsPublic || (fi:IsFamily  .and. fi:IsDefined(typeof(IsInstanceAttribute), false))
                var name := fi:Name:ToUpperInvariant()
                if seen:Add(name)
                    list:Add(name)
                endif
            endif
        next

        var aProps := t:GetProperties( BindingFlags.Instance | BindingFlags.Public )

        foreach pi as PropertyInfo in aProps
            var name := pi:Name:ToUpperInvariant()
            if seen:Add(name)
                list:Add(name)
            endif
        next
        return list:ToVoSymArray()


    static method MethodList(t as Type) as array
        var list := List<string>{}
        var seen := HashSet<string>{}
        var aInfo := t:GetMethods( BindingFlags.Instance | BindingFlags.Public )
        foreach oMI as MethodInfo in aInfo
            // convert to uppercase. We do not want duplicates that only differ by case
            if !oMI:IsSpecialName
                var name := oMI:Name:ToUpper()
                if seen:Add(name)
                    list:Add(name)
                endif
            endif
        next
        return list:ToVoSymArray()

    static method ToVoSymArray(self list as List<string>) as array
        // convert List<STRING> to Array of Symbols
        local aResult as array
        aResult := {}
        foreach var name in list
            AAdd(aResult, String2Symbol(name))
        next
        return aResult

    static method TreeHelper( t as Type ) as array
        local aList := null_array as array
        if t == null
            return aList
        end if

        var aInheritance := List<Type>{}
        do while t != null
            aInheritance:Add(t)
            t := t:BaseType
        end do
        aList := {}
        foreach type as Type in aInheritance
            var listMethod := List<string>{}
            var listVar    := List<string>{}
            // the lists keep the order in which the members were found, the sets are there to detect
            // the duplicates without a linear scan for every member
            var seenMethod := HashSet<string>{}
            var seenVar    := HashSet<string>{}
            var aInfo := type:GetMembers(BindingFlags.Instance + BindingFlags.Public + BindingFlags.NonPublic)
            foreach oInfo as MemberInfo in aInfo
                var name := oInfo:Name:ToUpperInvariant()
                switch oInfo
                case fld as FieldInfo
                    if fld:IsPublic .and. seenVar:Add(name)
                        listVar:Add(name)
                    end if
                case prop as PropertyInfo when ! prop:IsSpecialName
                    if seenVar:Add(name)
                        listVar:Add(name)
                    end if
                case m as MethodInfo when ! m:IsSpecialName
                    if seenMethod:Add(name)
                        listMethod:Add(name)
                    end if
                end switch
            next
            var aInstance := listVar:ToVoSymArray()
            var aMethod   := listMethod:ToVoSymArray()
            AAdd(aList , {(symbol) type:FullName, aInstance, aMethod})

        next
        return aList

    static method FindProperty( t as Type , cName as string, lAccess as logic, lSelf as logic) as PropertyInfo
        if t == null .or. String.IsNullOrEmpty(cName)
            return null
        endif
        lSelf := lSelf .or. EmulateSelf
        var mi := OOPHelpers.GetFieldOrPropertyFromCache(t, cName)
        if mi is PropertyInfo var pi
            // we must check. Sometimes in a subclass the Access was overwritten but not the assign
            // then we want to read the assign from the parent class
            if lAccess .and. pi:CanRead .and. IsVisible(pi:GetMethod, lSelf)
                return pi
            elseif ! lAccess .and. pi:CanWrite .and. IsVisible(pi:SetMethod, lSelf)
                return pi
            endif
        elseif mi is FieldInfo
            return null     // it must be a field then, so there is no property with this name
        endif

        // Remember the type we were called for. When the property is found in a parent class then it
        // must still be cached for this type, otherwise the cache would never be hit for subclasses
        var bt := t
        do while t != null
            var oInfo := OOPHelpers.GetProperty(t, cName, true, lSelf)
            if oInfo != null .and. ( (lAccess .and. oInfo:CanRead) .or. (.not. lAccess .and. oInfo:CanWrite) )
                AddMemberToCache(bt, cName, oInfo)
                return oInfo
            else
                t := t:BaseType
            endif
        enddo
        return null

    static method IsVisible(oMethod as MethodInfo, lSelf as logic) as logic
        if oMethod == null_object
            return false
        elseif oMethod:IsPublic
            return true
        endif
        return lSelf


    static method GetFieldOrPropertyFromCache(t as Type, cName as string) as MemberInfo
        if t != null .and. ! String.IsNullOrEmpty(cName) .and. fieldPropCache:TryGetValue(t, out var fields)
            if fields:TryGetValue(cName, out var result)
                return result
            endif
        endif
        return null

    static method AddMemberToCache(t as Type, cName as string, mi as MemberInfo) as logic
        if t != null .and. ! String.IsNullOrEmpty(cName)
            if ! fieldPropCache:TryGetValue(t, out var fields)
                fields := ConcurrentDictionary<string, MemberInfo> {StringComparer.OrdinalIgnoreCase}
                fieldPropCache:TryAdd( t, fields)
            endif
            return fields:TryAdd(cName, mi)
        endif
        return false


    static method FindField( t as Type, cName as string, lAccess as logic, lSelf as logic ) as FieldInfo
        if t == null .or. String.IsNullOrEmpty(cName)
            return null
        endif
        lSelf := lSelf .or. EmulateSelf
        var mi := OOPHelpers.GetFieldOrPropertyFromCache(t, cName)
        if mi != null
            if mi is FieldInfo var fi .and. IsFieldVisible(fi, lSelf)
                return fi
            endif
            return null     // it must be a property then
        endif
        var bt := t
        do while t != null
            var oInfo := OOPHelpers.GetField(t, cName, true, lSelf)
            if oInfo != null
                // check for readonly (initonly) fields
                if lAccess .or. ! oInfo:Attributes:HasFlag(FieldAttributes.InitOnly)
                    OOPHelpers.AddMemberToCache(bt, cName, oInfo)
                    return oInfo
                endif
            else
                t := t:BaseType
            endif
        enddo
        return null

    static method IsFieldVisible(oFld as FieldInfo, lSelf as logic) as logic
        if oFld == null_object
            return false
        elseif oFld:IsPublic
            return true
        endif
        return lSelf

    /// <include file="XSharp.RT.Docs.xml" path="doc/OOPHelpers.IsInternalVisible/*" />
    static method IsInternalVisible(propInfo as PropertyInfo) as logic
        local asm       := propInfo:DeclaringType:Assembly  as Assembly
        local st        := StackTrace{false}                as StackTrace
        // walk the frames one by one instead of materializing the whole array with GetFrames().
        // We only need the first frame that is not inside XSharp.RT itself
        for var i := 0 upto st:FrameCount-1
            var frameMethod := st:GetFrame(i):GetMethod()
            var declType    := frameMethod?:DeclaringType
            if declType == null
                exit                                        // dynamic method: we cannot tell
            endif
            var frameAsm := declType:Assembly
            if frameAsm != rtAssembly
                return frameAsm == asm
            endif
        next
        return false

    static method GetFieldOrProperty(oType as System.Type, cName as STRING) as Object
        // This method is used in the XPP Abstract class
        var mem := OOPHelpers.GetFieldOrPropertyFromCache(oType, cName)
        if mem != null
            RETURN mem
        endif
        var bf := BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static | BindingFlags.IgnoreCase
        var oClass := oType
        do while oClass != null
            var list := oClass:GetMember(cName, MemberTypes.Field | MemberTypes.Property, bf)
            if list != null .and. list:Length > 0
                foreach var fld in list
                    OOPHelpers.AddMemberToCache(oType, cName, fld)
                next
                return list[0]
            endif
            oClass := oClass:BaseType
        enddo
        RETURN NULL_OBJECT
    static method IVarGet(oObject as object, cIVar as string, lSelf as logic) as usual
        local t as Type
        local result as object
        lSelf := lSelf .or. EmulateSelf

        // VFP Empty and XPP DataObject and other objects that implement IDynamicProperties
        if oObject is IDynamicProperties var oDynamic
            return oDynamic:NoIvarGet(cIVar)
        endif
        if oObject is ILateBound var oLB
            if lSelf
                return oLB:NoIvarGetSelf(cIVar)
            else
                return oLB:NoIvarGet(cIVar)
            endif

        endif
        t := oObject:GetType()
        if oObject is IWrappedObject var oWrapped
            oObject := oWrapped:Object
            t       := oWrapped:Type
        endif
        var found := false
        try
            var propInfo := OOPHelpers.FindProperty(t, cIVar, true, lSelf)
            if propInfo != null_object .and. propInfo:CanRead
                var visible := lSelf .or. propInfo:GetMethod:IsPublic
                found := true
                if (! visible .and. propInfo:GetMethod:IsAssembly)
                    visible := IsInternalVisible(propInfo)
                endif
                if propInfo:GetIndexParameters():Length == 0
                    if visible
                        result := propInfo:GetValue(oObject, null)
                        if result == null
                            return ConvertFromNull(propInfo:PropertyType)
                        endif
                        return result
                    endif
                else
                    return nil
                endif
            endif
            var fldInfo := OOPHelpers.FindField(t, cIVar, true, lSelf)
            if fldInfo != null_object
                result := fldInfo:GetValue(oObject)
                if result == null
                    return ConvertFromNull(fldInfo:FieldType)
                endif
                return result
            endif
        catch as Error
            throw
        catch e as TargetInvocationException
            if e:InnerException is WrappedException
                throw e:InnerException
            endif
            throw Error{e:GetInnerException()}
        catch e as Exception
            if e:InnerException is WrappedException
                throw e:InnerException
            endif
            throw Error{e:GetInnerException()}
        end try
        cIVar := cIVar:ToUpperInvariant()
        if SendHelper(oObject, "NoIVarGet", <usual>{cIVar}, out var oResult,false)
            return oResult
        end if
        if found
            // the error should indicate that the property was found but it is not visible in this code
            var oError :=  Error.VOError( EG_NOACCESS, iif( lSelf, __function__, __function__ ), nameof(cIVar), 2, <object>{oObject, cIVar, lSelf})
            oError:Description := "Access to variable '"+cIVar+"' not allowed in this context"
            throw oError
        else
            var oError := Error.VOError( EG_NOVARMETHOD, iif( lSelf, __function__, __function__ ), nameof(cIVar), 2, <object>{oObject, cIVar} )
            oError:Description := oError:Message+" '"+cIVar+"'"
            throw oError
        endif
        // This property is set in the constructor of Dynamic Classes
        // To allow the codeblock for the INIT method to access hidden/private fields
    internal static property EmulateSelf as logic auto
    static method IVarPut(oObject as object, cIVar as string, oValue as object, lSelf as logic)  as void
        local t as Type
         // VFP Empty and XPP DataObject and other objects that implement IDynamicProperties
        if oObject is IDynamicProperties var oDynamic
            oDynamic:NoIvarPut(cIVar, oValue)
            return
        endif
        if oObject is ILateBound var oLB
            if lSelf
                oLB:NoIvarPutSelf(cIVar, oValue)
            else
                oLB:NoIvarPut(cIVar, oValue)
            endif
            return
        endif
        t := oObject:GetType()
        if oObject is IWrappedObject var oWrapped
            oObject := oWrapped:Object
            t       := oWrapped:Type
        endif
        lSelf := lSelf .or. EmulateSelf
        try
            var found := false
            var propInfo := OOPHelpers.FindProperty(t, cIVar, false, lSelf)
            if propInfo != null_object .and. propInfo:CanWrite
                var visible := lSelf .or. propInfo:SetMethod:IsPublic
                found := true
                if (! visible .and. propInfo:SetMethod:IsAssembly)
                    visible := IsInternalVisible(propInfo)
                endif
                if visible
                    oValue := OOPHelpers.ValueConvert(oValue, propInfo:PropertyType)
                    propInfo:SetValue(oObject,oValue , null)
                    return
                endif
            endif
            var fldInfo := OOPHelpers.FindField(t, cIVar, false, lSelf)
            if fldInfo != null_object
                found := true
                oValue := OOPHelpers.ValueConvert(oValue, fldInfo:FieldType)
                fldInfo:SetValue(oObject, oValue)
                return
            endif
            cIVar := cIVar:ToUpperInvariant()
            if SendHelper(oObject, "NoIVarPut", <usual>{cIVar, oValue})
                return
            end if
            if found
                // the error should indicate that the property was found but it is not visible in this code
                var oError :=  Error.VOError( EG_NOACCESS, iif( lSelf, __function__, __function__ ), nameof(cIVar), 2, <object>{oObject, cIVar, oValue, lSelf})
                oError:Description := "Access to variable '"+cIVar+"' not allowed in this context"
                throw oError
            else
                var oError :=  Error.VOError( EG_NOVARMETHOD, iif( lSelf, __function__, __function__ ), nameof(cIVar), 2, <object>{oObject, cIVar, oValue, lSelf})
                oError:Description := oError:Message+" '"+cIVar+"'"
                throw oError
            endif
        catch e as TargetInvocationException
            if e:InnerException is WrappedException
                throw e:InnerException
            endif
            var inner := e:GetInnerException()
            throw Error{inner}
        catch e as Exception
            if e:InnerException is WrappedException
                throw e:InnerException
            endif
            var inner := e:GetInnerException()
            throw Error{inner}
        end try

    static method SendHelper(oObject as object, cMethod as string, uArgs as usual[]) as logic
        return SendHelper(oObject, cMethod, uArgs, false)

    static method SendHelper(oObject as object, cMethod as string, uArgs as usual[], lCallBase as logic) as logic
        local lOk := OOPHelpers.SendHelper(oObject, cMethod, uArgs, out var result, lCallBase) as logic
        oObject := result   // get rid of warning
        return lOk

    static method FindOverloads(t as System.Type, cMethod as string, lInstance as logic) as IList<MethodInfo>
        var mlist := GetCachedOverLoads(t, cMethod)
        if mlist != null .and. mlist:Count > 0
            return mlist
        endif
        mlist := List<MethodInfo>{}
        local bf as BindingFlags
        if lInstance
            bf := BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.IgnoreCase
        else
            bf := BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic| BindingFlags.DeclaredOnly| BindingFlags.IgnoreCase
        endif
        foreach mi as MethodInfo in t:GetMember(cMethod, MemberTypes.Method, bf)
            if ! mi:IsSpecialName // suppress getters and setters and operator methods
                mlist:Add(mi)
            endif
        next
        CacheOverLoads(t, cMethod, mlist:ToArray())
        return mlist

    static method GetCachedOverLoads(t as System.Type, cMethod as string) as IList<MethodInfo>
        if t == null .or. String.IsNullOrEmpty(cMethod)
            return null
        endif
        if overloadCache:TryGetValue(t, out var type)
            if type:TryGetValue(cMethod, out var result)
                return result
            endif
        endif
        return null

    static method CacheOverLoads(t as System.Type, cMethod as string, ml as IList<MethodInfo>) as logic
        if !overloadCache:TryGetValue(t, out var typeDict)
            // GetOrAdd() so that we keep using the dictionary of the thread that won the race,
            // otherwise the entries that the other thread added would be lost
            typeDict := overloadCache:GetOrAdd(t, ConcurrentDictionary<string, IList<MethodInfo> >{StringComparer.OrdinalIgnoreCase})
        endif
        // TryAdd() already returns FALSE when the method is in the cache, so no ContainsKey() first
        return typeDict:TryAdd(cMethod, ml)

    static method SendHelper(oObject as object, cMethod as string, uArgs as usual[], result out usual, lCallBase as logic) as logic
        local t := oObject?:GetType() as Type
        result := nil
        if oObject is IWrappedObject var oWrapped
            oObject     := oWrapped:Object
            t           := oWrapped:Type
            lCallBase   := true
        endif
        if t == null
            throw Error.NullArgumentError( cMethod, nameof(oObject), 1 )
        endif
        if cMethod == null
            throw Error.NullArgumentError( cMethod, nameof(cMethod), 2 )
        endif
        local mi := null as MethodInfo
        // The name is not converted to uppercase here: the overload cache uses a case insensitive
        // comparer and the reflection lookups all pass BindingFlags.IgnoreCase
        var list := OOPHelpers.GetCachedOverLoads(t, cMethod)
        if list == null
            mi := OOPHelpers.FindMethod(t, cMethod, false, true)
        endif
        if mi == null
            if list == null
                list := OOPHelpers.FindOverloads(t, cMethod, true)
            endif
            try
                if list:Count > 0
                    // FindBestOverLoad() takes an IList<> and does not change it, so there is
                    // no need to copy the (cached) list to an array first
                    mi := OOPHelpers.FindBestOverLoad(t, list, cMethod,uArgs)
                endif
            catch as Error
                throw
            catch as Exception
                mi := null
            end try
        endif
        if mi == null
            // No Error Here. THat is done in the calling code
            return false
        endif
        return OOPHelpers.SendHelper(oObject, mi, uArgs, out result, lCallBase)

        static method SendHelper(oObject as object, mi as MethodInfo , uArgs as usual[], result out usual) as logic
            return SendHelper(oObject, mi, uArgs, out result, false)

    static internal dynamicMethodCache as ConcurrentDictionary<MethodInfo, Func<object, object[], object> >

    // Build a helper that performs a non virtual call to methodInfo, so we can call the method of a
    // parent class even when it is overridden in the class of the object.
    // The idea for this comes from
    // http://www.simplygoodcode.com/2012/08/invoke-base-method-using-reflection/index.html
    // I would have never come up with this myself.
    // Thanks for the Internet and for people that share code!
    // The helper takes (object, object[]) and returns object, so it does not depend on the type of the
    // object it is called for. That way it can be cached per method and exposed as a delegate
    static method CreateNotOverriddenMethodInvoker( methodInfo as MethodInfo) as Func<object, object[], object>
        var parameters := methodInfo:GetParameters()
        var owner      := methodInfo:DeclaringType
        var dynamicMethod := DynamicMethod{"", objectType, <Type>{ objectType, typeof(object[]) }, owner}
        var iLGenerator := dynamicMethod:GetILGenerator()
        iLGenerator:Emit(OpCodes.Ldarg_0)                       // load the object to call the method on
        if owner:IsValueType
            iLGenerator:Emit(OpCodes.Unbox, owner)
        else
            iLGenerator:Emit(OpCodes.Castclass, owner)
        endif
        for var i := 0 upto parameters:Length-1
            iLGenerator:Emit(OpCodes.Ldarg_1)                   // load array argument
            // get element at index. Ldc_I4 (and not Ldc_I4_S) because that also works for
            // methods with more than 127 parameters
            iLGenerator:Emit(OpCodes.Ldc_I4, i)                 // specify index
            iLGenerator:Emit(OpCodes.Ldelem_Ref)                // get element
            var parameterType := parameters[i]:ParameterType
            // Unbox_Any for every value type and not only for the primitive ones: a Castclass on a
            // value type (a DATE or a USUAL for example) would result in invalid IL
            if parameterType:IsValueType
                iLGenerator:Emit(OpCodes.Unbox_Any, parameterType)
            elseif parameterType != objectType
                iLGenerator:Emit(OpCodes.Castclass, parameterType)
            endif
        next
        iLGenerator:Emit(OpCodes.Call, methodInfo)               // Call and not Callvirt: no override
        if methodInfo:ReturnType == typeof(void)
            iLGenerator:Emit(OpCodes.Ldnull)
        elseif methodInfo:ReturnType:IsValueType
            iLGenerator:Emit(OpCodes.Box, methodInfo:ReturnType)
        endif
        iLGenerator:Emit(OpCodes.Ret)
        return (Func<object, object[], object>) dynamicMethod:CreateDelegate(typeof(Func<object, object[], object>))

    static method InvokeNotOverriddenMethod( methodInfo as MethodInfo, targetObject as object, arguments as object[]) as object
        var parameters := methodInfo:GetParameters()
        if (parameters:Length == 0)
            if arguments != null .and. arguments:Length != 0
                 throw Exception{"Arguments count doesn't match"}
            endif
        elseif arguments == null .or. parameters:Length != arguments:Length
            throw Exception{"Arguments count doesn't match"}
        endif
        // Generating the IL is expensive, so the invoker is cached. Calling the delegate is also much
        // cheaper than invoking the DynamicMethod through reflection
        if ! dynamicMethodCache:TryGetValue(methodInfo, out var invoker)
            invoker := CreateNotOverriddenMethodInvoker(methodInfo)
            dynamicMethodCache:TryAdd(methodInfo, invoker)
        endif
        return invoker:Invoke( targetObject, arguments )

    static method SendHelper(oObject as object, mi as MethodInfo , uArgs as usual[], result out usual, lCallBase as logic) as logic
        result := nil
        if mi == null
            throw Error.NullArgumentError( __function__, nameof(mi), 2 )
        endif
        if oObject == null .and. ! mi:IsStatic
            throw Error.NullArgumentError( __function__, nameof(oObject), 1 )
        endif
        if uArgs == null
            throw Error.NullArgumentError( __function__, nameof(uArgs), 3 )
        endif
        if mi != null
            var oArgs := OOPHelpers.MatchParameters(mi, uArgs, out var hasByRef)
            try
                if mi:ReturnType == usualType
                    if lCallBase
                        // Call the base method using a helper dynamic method
                        result := InvokeNotOverriddenMethod(mi, oObject, oArgs)
                    else
                        result := mi:Invoke(oObject, oArgs)
                    endif
                else
                    local oResult as object
                    if lCallBase
                        // Call the base method using a helper dynamic method
                        oResult := InvokeNotOverriddenMethod(mi, oObject, oArgs)
                    else
                        oResult := mi:Invoke(oObject, oArgs)
                    endif
                    if oResult == null
                        result := ConvertFromNull(mi:ReturnType)
                    else
                        result := oResult
                    endif
                endif
                if hasByRef
                    OOPHelpers.CopyByRefParameters( uArgs, oArgs, mi:GetParameters())
                endif
            catch as Error
                throw
            catch e as Exception
                if e:InnerException is WrappedException
                    throw e:InnerException
                endif
                if e:InnerException != null
                    var org := e
                    var ex := Error{e:InnerException}
                    local stack := ex:StackTrace as string
                    var sb := System.Text.StringBuilder{}
                    sb:Append(stack)
                    if stack:IndexOf(mi:Name,StringComparison.OrdinalIgnoreCase) == -1
                        // we have stripped too many layers. Strip until we see the method name we are trying to call
                         do while e:InnerException != null .and. stack:IndexOf(mi:Name,StringComparison.OrdinalIgnoreCase ) ==  -1
                            e := e:InnerException
                            var s := ErrorStack(StackTrace{e,true},0)
                            if !s:StartsWith(EMPTY_ERRORSTACK)
                                sb:Insert(0, s)
                            endif
                        enddo
                    endif
                    if org:InnerException is AggregateException var aex
                        var base := aex:GetBaseException()
                        ex:Description := base:Message
                        var s := ErrorStack(StackTrace{base,true},UInt32.MaxValue)
                        sb:Insert(0,s)
                    endif
                    ex:Stack := sb:ToString()
                    throw ex
                endif
                throw // rethrow exception
            end try

        endif
        return true

    static method CopyByRefParameters(uArgs as usual[], oArgs as object[], pars as ParameterInfo[]) as void
        // Assign parameters back.
        var max    := Math.Min(uArgs:Length, oArgs:Length)  -1
        for var nParam := 0 to max
            local param := pars[nParam] as ParameterInfo
            if param:IsOut .or. param:ParameterType:IsByRef
                // We no longer check to see if the usual has the ByRef set.
                // That really does not matter. If the calling code is not
                // interested in the new value then they will not copy it back
                // to the original value anyway
                //IF uArgs[nParam]:IsByRef
                uArgs[nParam] := oArgs[nParam]
                //ENDIF
            endif
        next

    static method FindOperator(srcType as System.Type,toType as System.Type) as MethodInfo
        if srcType == null .or. toType == null
            return null_object
        endif
        if ! operatorCache:TryGetValue(srcType, out var toTypes)
            // GetOrAdd() so that we keep using the dictionary of the thread that won the race
            toTypes := operatorCache:GetOrAdd(srcType, ConcurrentDictionary<System.Type, MethodInfo>{})
        endif
        if toTypes:TryGetValue(toType, out var cached)
            // this may be NULL: the most common case is that there is no operator at all and
            // looking that up costs two calls to GetMember(), so it is cached as well
            return cached
        endif
        local result := null as MethodInfo
        foreach oMember as MethodInfo in srcType:GetMember("op_Implicit")
            if oMember:ReturnType == toType
                result := oMember
                exit
            endif
        next
        if result == null
            foreach oMember as MethodInfo in srcType:GetMember("op_Explicit")
                if oMember:ReturnType == toType
                    result := oMember
                    exit
                endif
            next
        endif
        toTypes:TryAdd(toType, result)
        return result

    static method IsNumericTypeCode(tc as TypeCode) as logic
        switch tc
        case TypeCode.Byte
        case TypeCode.SByte
        case TypeCode.Int16
        case TypeCode.Int32
        case TypeCode.Int64
        case TypeCode.UInt16
        case TypeCode.UInt32
        case TypeCode.UInt64
        case TypeCode.Double
        case TypeCode.Single
        case TypeCode.Decimal
            return true
        otherwise
            return false
        end switch



    static method ValueConvert(uValue as usual,toType as System.Type) as object
        local oResult := uValue as OBJECT
        if oResult?:GetType() == toType
            RETURN oResult
        endif
        if toType == floatType
            return (float) uValue
        elseif uValue:SystemType == toType
            return uValue
        else

            var tc := Type.GetTypeCode(toType)
            if IsNumericTypeCode(tc) .and. ! uValue:IsNumeric
                if uValue:IsLogic
                    uValue := (int) uValue
                else
                    // convert to numeric
                    uValue := Val(uValue:ToString())
                endif
            endif
            if toType == usualType
                // return a boxed usual
                return __castclass(object, uValue)
            elseif toType == dateType .and. uValue:IsDateTime
                return (date)(System.DateTime) uValue
            elseif uValue:IsArray .and. toType == arrayType
                return (array) uValue
            elseif uValue:IsString .and. toType == symbolType
                return (symbol) uValue
            elseif uValue:IsSymbol .and. toType == stringType
                return (string) uValue
            elseif uValue:IsObject .or. uValue:IsCodeblock
                return (object) uValue
            elseif uValue:IsPtr .and. (toType == ptrType .or. toType:IsPointer)
                return IntPtr{(ptr) uValue}
            elseif oResult != null
                // check to see if the source type contains an implicit converter
                var oOperator := FindOperator(oResult:GetType(), toType)
                if oOperator != null_object
                    NOP
                else
                    oOperator := FindOperator(usualType, toType)
                    if oOperator != null_object
                        // box the usual
                        oResult := __castclass(object, uValue)
                    endif
                endif
                if oOperator != null_object
                    // oValue is either a boxed USUAL (for operators of the USUAL type)
                    // or the real thing, depending on the operator that was chosen
                    try
                        return oOperator:Invoke(null, <object>{oResult})
                    catch
                        // do not throw error here. We will try to convert the value below with Convert.ChangeType
                        nop
                    end try
                endif
            endif
            // when we get here then there is no operator and we will try to change the type..
            // or the call to the operator failed
            try
                // if the type is a Nullable<T> then get the underlying type
                toType := Nullable.GetUnderlyingType(toType) DEFAULT toType
                if toType:IsEnum
                    oResult := System.Enum.ToObject(toType, oResult)
                else
                    oResult := Convert.ChangeType(oResult, toType)
                endif
            catch
                local ex as Error
                ex := Error{Gencode.EG_WRONGCLASS, "", i"Could not convert value {oResult} to type {toType}" }
                ex:FuncSym := __function__
                ex:Stack := ErrorStack()
                throw ex
            end try
            return oResult
        endif

    static method DoSend(oObject as object, cMethod as string, args as usual[], cCaller as string) as usual
        if oObject == null
            throw Error.NullArgumentError( cCaller, nameof(oObject), 1 )
        endif
        if cMethod == null
            throw Error.NullArgumentError( cCaller, nameof(cMethod), 2 )
        endif
        local result as usual
        if ! OOPHelpers.SendHelper(oObject, cMethod, args, out result, false)
            local nomethodArgs as usual[]
            cMethod := cMethod:ToUpperInvariant()
            RuntimeState.NoMethod := cMethod   // For NoMethod() function
            if XSharp.RuntimeState.Dialect == XSharpDialect.Vulcan
                // vulcan includes the method name
                nomethodArgs := usual[]{ args:Length+1 }
                nomethodArgs[0] := cMethod
                Array.Copy( args, 0, nomethodArgs, 1, args:Length )
            else
                // other dialects do not include the method name
                nomethodArgs := usual[]{ args:Length }
                Array.Copy( args, 0, nomethodArgs, 0, args:Length )
            endif
            if oObject is ILateBound var oLB
                return oLB:NoMethod(nomethodArgs)
            endif
            if ! OOPHelpers.SendHelper(oObject, "NoMethod" , nomethodArgs, out result, false)
                var oError := Error.VOError( EG_NOMETHOD, cCaller, nameof(cMethod), 2, <object>{oObject, cMethod, args} )
                oError:Description  := oError:Message + " '"+cMethod+"'"
                throw oError
            endif
        endif
        return result
    static method LoadXSharpRuntimeAssemblies() as void
        foreach asm as Assembly in FindOurAssemblies()
            if ! aXsAssemblies:Contains(asm)
                var attr := (AssemblyCompanyAttribute) asm:GetCustomAttribute(typeof(AssemblyCompanyAttribute))
                if attr != null
                    if attr:Company == XSharp.Constants.Company
                        aXsAssemblies:Add(asm)
                    endif
                endif
            endif
        next

    static method GetCallingMethod() as MethodBase
        if aXsAssemblies:Count == 0
            LoadXSharpRuntimeAssemblies()
        endif
        var st := StackTrace{}
        var level := 2
        var mi := st:GetFrame(level):GetMethod()
        var type := mi:DeclaringType
        // when nested call from the runtime walk the stack until we find a method that is not
        // declared in one of the runtime assemblies. For dynamic methods the type can be NULL
        do while type != null .and. aXsAssemblies:Contains(type:Assembly)
            level += 1
            var frame := st:GetFrame(level)
            if frame == null
                exit
            endif
            var frameMethod := frame:GetMethod()
            if frameMethod == null
                exit
            endif
            mi   := frameMethod
            type := frameMethod:DeclaringType
        enddo
        return mi

end class


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/asend/*" />
function ASend(aTarget as array, symMethod as string, MethodArgList params usual[] ) as array
    if aTarget != null .and. ! String.IsNullOrEmpty( symMethod )
        foreach var x in aTarget
            __InternalSend( x, symMethod, MethodArgList )
        next
    endif
    return aTarget


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/checkinstanceof/*" />
function CheckInstanceOf(oObject as object,symClassName as string) as logic
    if oObject == null_object
        return false
    elseif IsInstanceOf(oObject, symClassName)
        return true
    endif
    local oError := Error.VOError(EG_WRONGCLASS, __function__, nameof(oObject),1, null) as Error
    oError:Description := symClassName + " <-> " + oObject:GetType():Name
    throw oError


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/classcount/*" />
function ClassCount() as dword
    // count the classes without building the array of symbols that ClassList() returns
    local nCount := 0 as dword
    foreach assembly as System.Reflection.Assembly in System.AppDomain.CurrentDomain:GetAssemblies()
        try
            foreach type as System.Type in assembly:GetTypes()
                if type:IsPublic
                    nCount += 1
                endif
            next
        catch as Exception
            nop
        end try
    next
    return nCount

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/classlist/*" />
function ClassList() as array
    local classes    := array{} as array
    local assemblies := System.AppDomain.CurrentDomain:GetAssemblies() as System.Reflection.Assembly[]
    foreach assembly as System.Reflection.Assembly in assemblies
        try
            local types := assembly:GetTypes() as System.Type[]
            foreach type as System.Type in types
                try
                    if type:IsPublic
                        classes:Add(String2Symbol(type:Name))
                    endif
                catch as Exception
                    nop

                end try
            next
            //		CATCH oEx AS ReflectionTypeLoadException
        catch as Exception
            nop
        end try
    next
    return classes


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/classname/*" />
function ClassName(oObject as object) as string
    if oObject != null
        return oObject:GetType():Name:ToUpper()
    endif
    return ""



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/classtree/*" />
function ClassTree(oObject as object) as array
    if oObject != null
        return OOPHelpers.ClassTree(oObject:GetType())
    endif
    return {}

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/createinstance/*" />
function CreateInstance(symClassName,InitArgList) as object clipper
    if ! ( symClassName:IsSymbol || symClassName:IsString )
        throw Error.DataTypeError( __function__, nameof(symClassName), 1, symClassName)
    endif
    var nPCount := PCount()
    var uArgs := usual[]{nPCount-1}
    for var nArg := 1 to nPCount-1
        uArgs[nArg-1] := _GetFParam(nArg+1) // _GetFParam() is 1 based !
    next
    return _CreateInstance(symClassName, uArgs)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/createinstance/*" />
function _CreateInstance(type as System.Type, InitArgList as usual[]) as object
    var constructors := type:GetConstructors()
    local ctor := OOPHelpers.FindBestOverLoad(type, constructors, __function__ ,InitArgList) as ConstructorInfo
    if ctor == null
        var oError := Error.VOError( EG_NOMETHOD, __function__, "Constructor", 0 , null)
        oError:Description := "No CONSTRUCTOR defined for type "+ type:FullName
        throw oError
    endif
    local oRet as object
    try
        local oArgs := OOPHelpers.MatchParameters(ctor, InitArgList, out var hasByRef) as object[]
        oRet := ctor:Invoke( oArgs )
        if hasByRef
            OOPHelpers.CopyByRefParameters(InitArgList, oArgs, ctor:GetParameters())

        endif
    catch as Error
        throw
    catch e as Exception
        throw Error{e:GetInnerException()}
    end try
    return oRet


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/createinstance/*" />
function _CreateInstance(symClassName as string, InitArgList as usual[]) as object

    var t := OOPHelpers.FindClass(symClassName)
    if t == null
        var oError := Error.VOError( EG_NOCLASS, __function__, nameof(symClassName), 1,  <object>{symClassName}  )
        oError:Description := oError:Message+" '"+symClassName+"'"
        throw oError
    endif
    return _CreateInstance(t, InitArgList)



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/classtreeclass/*" />
function ClassTreeClass(symClass as string) as array
    var t := OOPHelpers.FindClass(symClass)
    if t != null
        return OOPHelpers.ClassTree(t)
    else
        throw Error{EG_NOCLASS,0}
    endif




/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/isaccess/*" />
function IsAccess(oObject as object,symAccess as string) as logic
    if oObject != null
        var oProp := OOPHelpers.FindProperty(oObject:GetType(), symAccess, true, true)
        if oProp != null_object
            return oProp:CanRead
        endif
    endif
    return false


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/isassign/*" />
function IsAssign(oObject as object,symAssign as string) as logic
    if oObject != null
        var oProp := OOPHelpers.FindProperty(oObject:GetType(), symAssign, false, true)
        if oProp != null_object
            return oProp:CanWrite
        endif
    endif
    return false


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/isclass/*" />
function IsClass(symClassName as string) as logic
    return OOPHelpers.FindClass(symClassName) != null

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/isclassof/*" />
function IsClassOf(symClassName as string,symSuperClassName as string) as logic
    local tSub   := OOPHelpers.FindClass(symClassName) as Type
    local tSuper := OOPHelpers.FindClass(symSuperClassName) as Type
    // IsClassOf() in VO returns TRUE when child and parent class is the same (and it exists)
    return tSub != null .and. tSuper != null .and. (tSub == tSuper .or. tSub:IsSubclassOf(tSuper))



/// <include file="XSharp.RT.Docs.xml" path="doc/FindClass/*" />
function FindClass(cClassname as string) as System.Type
    return OOPHelpers.FindClass(cClassname)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/isinstanceof/*" />
function IsInstanceOf(oObject as object,symClassName as string) as logic
    if oObject == null_object
        return false
    endif
    // this was a smarter implemenation, but has performance issues
    // especially when symClassName is not found, as we cannot cache that
    /*	LOCAL oType := OOPHelpers.FindClass(cName, FALSE) AS System.Type
    if oType == null
    return false
    end if
    return oType:IsAssignableFrom(oObject:GetType())*/
    local oType as Type
    oType := oObject:GetType()
    do while oType != null
        if String.Equals(oType:Name, symClassName, StringComparison.OrdinalIgnoreCase)
            return true
        end if
        oType := oType:BaseType
    end do
    return false


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/isinstanceofusual/*" />
function IsInstanceOfUsual(uObject as usual,symClassName as string) as logic
    switch uObject:Type
    case __UsualType.Object
    case __UsualType.Codeblock
    case __UsualType.Array
    case __UsualType.Decimal
    case __UsualType.Currency
    case __UsualType.Binary
        return IsInstanceOf(uObject, symClassName)
    end switch
    return false




/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ivarget/*" />
function IVarGet(oObject as object,symInstanceVar as string) as usual
    if oObject == null_object
        throw Error.NullArgumentError(__function__, nameof(oObject),1)
    endif
    if String.IsNullOrEmpty(symInstanceVar)
        throw Error.NullArgumentError(__function__, nameof(symInstanceVar),2)
    endif
    // when we call IvarGet within a method of the same type as oObject
    // we should allow to access private/hidden properties
    // see https://github.com/X-Sharp/XSharpPublic/issues/1335
    var lSelf := false
    local uResult as usual
    try
        uResult := OOPHelpers.IVarGet(oObject, symInstanceVar, lSelf)
    catch as Exception when !lSelf
        // retry so we can access hidden properties/fields
        // from within methods of the same type
        var mi := OOPHelpers.GetCallingMethod()
        if mi:DeclaringType == oObject:GetType()
            uResult := OOPHelpers.IVarGet(oObject, symInstanceVar, true)
        else
            // different type: rethrow exception
            throw
        endif
    catch
        // already with lSelf rethrow exception
        throw
    end try
    return uResult




/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ivargetinfo/*" />
function IVarGetInfo(oObject as object,symInstanceVar as string) as dword
    return OOPHelpers.IVarHelper(oObject, symInstanceVar, true)



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ismethod/*" />
function IsMethod(oObject as object,symMethod as string) as logic
    if oObject != null_object
        return OOPHelpers.IsMethod(oObject:GetType(), symMethod)
    endif
    return false



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ismethodusual/*" />
function IsMethodUsual(uObject as usual,symMethod as string) as logic
    if uObject:IsObject
        return IsMethod( uObject, symMethod )
    endif
    return false


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ismethodclass/*" />
function IsMethodClass( symClass as string, symMethod as string ) as logic
    var t := OOPHelpers.FindClass( symClass )
    if t != null
        return OOPHelpers.IsMethod( t, symMethod )
    endif
    return false



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ivargetself/*" />
function IVarGetSelf(oObject as object,symInstanceVar as string) as usual
    if oObject == null_object
        throw Error.NullArgumentError(__function__, nameof(oObject),1)
    endif
    if String.IsNullOrEmpty(symInstanceVar)
        throw Error.NullArgumentError(__function__, nameof(symInstanceVar),2)
    endif
    return OOPHelpers.IVarGet(oObject, symInstanceVar, true)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ivarlist/*" />
function IvarList(oObject as object) as array
    // IVarList already checks for NULL_OBJECT
    if oObject is IDynamicProperties var oDynamic
        var props := oDynamic:GetPropertyNames()
        var result := {}
        foreach var prop in props
            result:Add(prop:ToUpper())
        next
        return result
    endif
    return OOPHelpers.IVarList(oObject?:GetType())



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ivarlistclass/*" />
function IvarListClass(symClass as string) as array
    var t := OOPHelpers.FindClass(symClass)
    return OOPHelpers.IVarList(t)



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ivarputinfo/*" />
function IVarPutInfo(oObject as object,symInstanceVar as symbol) as dword
    // IVarHelper already checks for NULL_OBJECT
    return OOPHelpers.IVarHelper(oObject, symInstanceVar, false)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ivarput/*" />
function IVarPut(oObject as object,symInstanceVar as string,uValue IN usual) as usual
    if oObject == null_object
        throw Error.NullArgumentError(__function__, nameof(oObject),1)
    endif
    if String.IsNullOrEmpty(symInstanceVar)
        throw Error.NullArgumentError(__function__, nameof(symInstanceVar),2)
    endif
    var lSelf := false
    try
        OOPHelpers.IVarPut(oObject, symInstanceVar, uValue, lSelf)
    catch as Exception when !lSelf
        // when we call IVarPut within a method of the same type as oObject
        // we should allow to access private/hidden properties
        // see https://github.com/X-Sharp/XSharpPublic/issues/1335
        var mi := OOPHelpers.GetCallingMethod()
        if mi:DeclaringType == oObject:GetType()
            OOPHelpers.IVarPut(oObject, symInstanceVar, uValue, true)
        else
            throw // other type: rethrow exception
        endif
    catch
        throw // rethrow exception
    end try
    return uValue


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ivarputself/*" />
function IVarPutSelf(oObject as object,symInstanceVar as string,uValue IN usual) as usual
    if oObject == null_object
        throw Error.NullArgumentError(__function__, nameof(oObject),1)
    endif
    if String.IsNullOrEmpty(symInstanceVar)
        throw Error.NullArgumentError(__function__, nameof(symInstanceVar),2)
    endif
    OOPHelpers.IVarPut(oObject, symInstanceVar, uValue,true)
    return uValue



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/methodlist/*" />
function MethodList(oClass as object) as array
    if oClass != null
        return OOPHelpers.MethodList( oClass:GetType() )
    endif
    return null_array


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/methodlistclass/*" />
function MethodListClass( symClass as string ) as array
    local aReturn as array
    var t := OOPHelpers.FindClass( symClass )
    if t != null
        aReturn := OOPHelpers.MethodList( t )
    else
        aReturn  := null_array
    endif

    return aReturn




/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/nomethod/*" />
function NoMethod() as string
    return RuntimeState.NoMethod


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/object2array/*" />
function Object2Array(oObject as object) as array
    local t as System.Type
    if oObject == null_object
        return null_array
    endif
    local aProps as PropertyInfo[]
    local aFields as FieldInfo[]
    local aResult as array
    aResult := {}
    t := oObject:GetType()
    aProps := t:GetProperties(BindingFlags.Instance | BindingFlags.Public)
    try
        foreach p as PropertyInfo in aProps
            local uVal as usual
            if p:CanRead
                uVal := p:GetValue(oObject,null)
                AAdd(aResult, uVal)
            endif
        next
        aFields := t:GetFields(BindingFlags.Instance | BindingFlags.Public)
        foreach f as FieldInfo in aFields
            local uVal as usual
            if ! f:IsSpecialName
                uVal := f:GetValue(oObject)
                AAdd(aResult, uVal)
            endif
        next
    catch e as Exception
        throw Error{e:GetInnerException()}
    end try
    return aResult




/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ooptree/*" />
function OOPTree(oObject as object) as array
    // TreeHelper already checks for NULL_OBJECT
    return OOPHelpers.TreeHelper(oObject?:GetType())


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ooptreeclass/*" />
function OOPTreeClass(symClass as string) as array
    var type := OOPHelpers.FindClass(symClass)
    // TreeHelper already checks for NULL_OBJECT
    return OOPHelpers.TreeHelper(type)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/send/*" />
function Send(oObject as usual,symMethod as usual, MethodArgList params usual[]) as usual
    if !oObject:IsObject
        throw Error.VOError( EG_DATATYPE, __function__, nameof(oObject), 1, <object>{ oObject}  )
    endif
    if ! symMethod:IsString  .and. ! symMethod:IsSymbol
        throw Error.VOError( EG_DATATYPE, __function__, nameof(symMethod) , 2, <object>{ symMethod } )
    endif
    if MethodArgList == null
        // this happens for SEND (oObject, "method", NULL)
        MethodArgList := <usual>{null}
    endif
    local oToSend := oObject as object
    local cMethod := symMethod as string
    local uResult as usual
    uResult := OOPHelpers.DoSend(oToSend, cMethod, MethodArgList, __function__)
    return uResult


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/send/*" />
function CSend(oObject as object,symMethod as string, MethodArgList params usual[]) as usual
    return __InternalSend(oObject, symMethod, MethodArgList)



/// <exclude />
function _Send(oObject as object,symMethod as MethodInfo, MethodArgList params usual[]) as usual
    if OOPHelpers.SendHelper(oObject, symMethod, MethodArgList, out var result)
        return result
    endif
    // SendHelper never returns FALSE. It throw an exception
    return false


    // This is called by the compiler when a late bound call is made on a USUAL.
    // It is strongly typed and more efficient than Send(), which must use the
    // CLIPPER calling convention for compatiblity with VO.
    // Note: Make The first parameter in __InternalSend() in the runtime must be a USUAL!
    //       The compiler expects that
/// <exclude />
function __InternalSend( oObject as usual, cMethod as string, args params usual[] ) as usual
    return OOPHelpers.DoSend(oObject, cMethod, args, __function__)

/// <include file="XSharp.RT.Docs.xml" path="doc/_ArrayToUsualArray/*" />
function _ArrayToUsualArray (args as array) as usual[]
    local elements as int
    local uargs    as usual[]
    local x        as dword

    elements := (int) args:Length
    uargs    := usual[]{ elements }

    for x := 0 upto elements -1
        uargs[x] := args[x]
    next
    return uargs

/// <include file="XSharp.RT.Docs.xml" path="doc/_ArrayToObjectArray/*" />
function _ArrayToObjectArray (args as array) as object[]
    local elements as int
    local oArgs    as object[]
    local x        as dword

    elements := (int) args:Length
    oArgs    := object[]{ elements }

    for x := 0 upto elements -1
        oArgs[x] := args[x]
    next
    return oArgs

/// <include file="XSharp.RT.Docs.xml" path="doc/_UsualArrayToObjectArray/*" />
function _UsualArrayToObjectArray (args as usual[]) as object[]
    local elements as int
    local oArgs    as object[]
    local x        as dword

    elements := (int) args:Length
    oArgs    := object[]{ elements }

    for x := 0 upto elements -1
        oArgs[x] := args[x]
    next
    return oArgs

/// <include file="XSharp.RT.Docs.xml" path="doc/_ObjectArrayToUsualArray/*" />
function _ObjectArrayToUsualArray (args as object[]) as usual[]
    local elements as int
    local uArgs    as usual[]
    local x        as dword

    elements := (int) args:Length
    uArgs    := usual[]{ elements }

    for x := 0 upto elements -1
        uArgs[x] := args[x]
    next
    return uArgs

/// <exclude/>
// identical to CSend and __InternalSend but with a normal array of args
function _SendClassParams( oObject as object, cmethod as string, args as array ) as usual
    local uArgs as usual[]
    uArgs := _ArrayToUsualArray(args)
    return OOPHelpers.DoSend(oObject, cmethod, uArgs , __function__)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/mparamcount/*" />
function MParamCount(symClass as string,symMethod as string) as dword
    local type as Type
    type := OOPHelpers.FindClass(symClass)
    if type != null
        local met as MethodInfo
        met := OOPHelpers.FindMethod(type, symMethod, true)
        if met != null
            if met:IsDefined(typeof(ClipperCallingConventionAttribute),false)
                // calculate the # of parameters
                var oAttr := (ClipperCallingConventionAttribute) met:GetCustomAttributes(typeof(ClipperCallingConventionAttribute), false):First()
                return (dword) oAttr:ParameterNames:Length
            else
                return (dword) met:GetParameters():Length
            endif
        else
            throw Error.VOError( EG_NOMETHOD,  "MParamCount", nameof(symMethod), 2, <object>{symMethod} )
        endif
    else
        throw Error.VOError( EG_WRONGCLASS,  "MParamCount", nameof(symClass), 1, <object>{symClass} )
    endif





/// <include file="XSharp.RT.Docs.xml" path="doc/FParamCount/*" />
function FParamCount(symFunction as string) as dword
    local aFuncs as MethodInfo[]
    aFuncs := OOPHelpers.FindClipperFunctions(symFunction)
    // CLipper functions can't and shouldn't have overloads
    if aFuncs != null
        if aFuncs:Length == 1
            local oMI := aFuncs:First() as MethodInfo
            if oMI:IsDefined(typeof(ClipperCallingConventionAttribute),false)
                // calculate the # of parameters
                local oAttr as ClipperCallingConventionAttribute
                oAttr := (ClipperCallingConventionAttribute) oMI:GetCustomAttributes(typeof(ClipperCallingConventionAttribute), false):First()
                return (dword) oAttr:ParameterNames:Length
            else
                return (dword) oMI:GetParameters():Length
            endif
        else
            throw Error.VOError( EG_AMBIGUOUSMETHOD,  "FParamCount", nameof(symFunction), 1, <object>{symFunction} )
        endif
    else
        throw Error.VOError( EG_NOFUNC,  "FParamCount", nameof(symFunction), 1, <object>{symFunction} )
    endif



/// <include file="XSharp.RT.Docs.xml" path="doc/_CallClipFunc/*" />
function _CallClipFunc(symFunction as string,aArgs as array) as usual
    return _CallClipFunc(symFunction, _ArrayToUsualArray(aArgs))

/// <include file="XSharp.RT.Docs.xml" path="doc/_CallClipFunc/*" />
function _CallClipFunc(symFunction as string, uArgs params usual[]) as usual
    local aFuncs as MethodInfo[]
    local oMI as MethodInfo

    aFuncs := OOPHelpers.FindClipperFunctions(symFunction)
    // CLipper functions can't and shouldn't have overloads
    // But we try to find the best overload anyway
    if aFuncs != null
        if aFuncs:Length == 1
            oMI		:= aFuncs:First()
            if OOPHelpers.SendHelper(null, oMI, uArgs, out var result)
                return result
            endif
        elseif aFuncs:Length == 0
            return nil
        else
            oMI  := OOPHelpers.FindBestOverLoad(null, aFuncs, symFunction, uArgs)
            if oMI != null
                if OOPHelpers.SendHelper(null, oMI, uArgs, out var result)
                    return result
                endif
            endif
            throw Error.VOError( EG_AMBIGUOUSMETHOD,  __function__, nameof(symFunction), 1, <object>{symFunction} )
        endif
    else
        throw Error.VOError( EG_NOFUNC,  "_CallClipFunc", nameof(symFunction), 1, <object>{symFunction} )
    endif
    return  nil

function _HasClipFunc(symFunction as string) as logic
    local aFuncs as MethodInfo[]
    aFuncs := OOPHelpers.FindClipperFunctions(symFunction)
    return aFuncs:Length > 0


/// <include file="XSharp.RT.Docs.xml" path="doc/XSharpLoadLibrary/*" />
function XSharpLoadLibrary(cLibFileName as string) as Assembly
    local oAssembly as Assembly
    oAssembly := Assembly.LoadFrom(cLibFileName)
    local oModule as Module
    oModule := oAssembly:GetModules():First()
    local oMethod as MethodInfo
    oMethod := oModule:GetMethod("RunInitProcs")
    if oMethod != null
        oMethod:Invoke(null, null)
    end if
    return oAssembly

function EnableLBOptimizations(lSet as logic) as logic
    local lOld := OOPHelpers.EnableOptimizations as logic
    OOPHelpers.EnableOptimizations := lSet
    return lOld

