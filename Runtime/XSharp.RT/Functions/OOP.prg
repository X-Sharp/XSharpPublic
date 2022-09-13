//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

#pragma options("az", on)


using XSharp.Internal
using System.Reflection
using System.Collections.Generic
using System.Linq
using System.Text
using System.Runtime.CompilerServices
using System.Diagnostics

internal static class OOPHelpers
    static internal EnableOptimizations as logic
    static internal cacheClassesAll as Dictionary<string,Type>
    static internal cacheClassesOurAssemblies as Dictionary<string,Type>
    static internal fieldPropCache    as Dictionary<System.Type, Dictionary<string, MemberInfo> >
    static internal overloadCache     as Dictionary<System.Type, Dictionary<string, IList<MethodInfo>> >
    static constructor()
        cacheClassesAll             := Dictionary<string,Type>{StringComparer.OrdinalIgnoreCase}
        cacheClassesOurAssemblies   := Dictionary<string,Type>{StringComparer.OrdinalIgnoreCase}
        fieldPropCache              := Dictionary<System.Type, Dictionary<string, MemberInfo> >{}
        overloadCache               := Dictionary<System.Type, Dictionary<string, IList<MethodInfo>> >{}
    return

    static method FindOurAssemblies as IEnumerable<Assembly>
        return	from asm in AppDomain.CurrentDomain:GetAssemblies() ;
                where asm:IsDefined(TYPEOF( ClassLibraryAttribute ), false) ;
                select asm

    static method FindClipperFunctions(cFunction as string) as MethodInfo[]
        var cla := TYPEOF( ClassLibraryAttribute )
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
                            if ! oM:IsSpecialName .and. String.Compare(oM:Name, cFunction, true) == 0
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
        return aMethods:ToArray()


    static method FindClass(cName as string) as System.Type
        return OOPHelpers.FindClass(cName, true)

    static method FindClass(cName as string, lOurAssembliesOnly as logic) as System.Type
        // TOdo Optimize FindClass
        local ret := null as System.Type
        local aAssemblies as IEnumerable<Assembly>

        if String.IsNullOrWhiteSpace(cName)
            // otherwise asm:GetType() will throw an exception with empty name
            return ret
        end if

        if lOurAssembliesOnly
            if cacheClassesOurAssemblies:ContainsKey(cName)
                return cacheClassesOurAssemblies[cName]
            end if
            aAssemblies := OOPHelpers.FindOurAssemblies()
        else
            if cacheClassesAll:ContainsKey(cName)
                return cacheClassesAll[cName]
            end if
            aAssemblies := AppDomain.CurrentDomain:GetAssemblies()
        end if

        foreach asm as Assembly in aAssemblies
            ret := asm:GetType( cName, false, true )
            if ret != null
                exit
            endif
            // The class could be prefixed with a Namespace.
            // If there is a class library attribute and we prefixed all classes with a namespace then
            // this is visible in the ClassLibraryAttribute
            // We don't know if the current assembly is compiler with /INS, but we assume it is when they
            // use the 'old fashioned' CreateInstance().
            var att := TYPEOF( ClassLibraryAttribute )
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
            att := TYPEOF( ImplicitNamespaceAttribute )
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
            // try to find classes in a namespace
            foreach asm as Assembly in aAssemblies
                var cla := TYPEOF( ClassLibraryAttribute )
                if asm:IsDefined(  cla, false )
                    var types := asm:GetTypes()
                    foreach type as System.Type in types
                        if String.Compare(type:Name, cName, StringComparison.OrdinalIgnoreCase) == 0
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

        if ret != null
            if lOurAssembliesOnly
                if .not. cacheClassesOurAssemblies:ContainsKey(cName)
                    cacheClassesOurAssemblies:Add(cName , ret)
                end if
            else
                if .not. cacheClassesAll:ContainsKey(cName)
                    cacheClassesAll:Add(cName , ret)
                end if
            end if
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

    static method CompareMethods(m1 as MethodBase, m2 as MethodBase, uArgs as usual[]) as long
        var p1 := m1:GetParameters()
        var p2 := m2:GetParameters()
        var n1 := CountNonDefaultParameters(p1)
        var n2 := CountNonDefaultParameters(p2)
        if n1 != n2
            if n1 == uArgs:Length
                return 1
            elseif n2 == uArgs:Length
                return 2
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
                if parType1 = typeof(usual)
                    return 1
                endif
                if parType2 = typeof(usual)
                    return 2
                endif
            endif
        next
        var type1 := m1:DeclaringType
        var type2 := m2:DeclaringType
        if (type1 != type2)
            if type1:IsAssignableFrom(type2)
                return 2
            elseif type2:IsAssignableFrom(type1)
                return 1
            endif
        endif


        return 0

    static method CountNonDefaultParameters(pars as IList<ParameterInfo>) AS LONG
      for var i := 0 upto pars:Count -1
        local oPar := pars[i] as ParameterInfo
        var oArg    := OOPHelpers.GetDefaultValue(oPar)
        if oArg != NULL
            return i
        endif
     next
     return pars:Count

    static method FindBestOverLoad<T>(overloads as T[], cFunction as string, uArgs as usual[]) as T where T is MethodBase
        if overloads:Length <= 1
            return overloads:FirstOrDefault()
        endif
        // More than one
        var found := List<T>{}
        // first look for methods with the same ! of parametes
        foreach var m in overloads
            var pars := m:GetParameters()
            if pars:Length == uArgs:Length
                found:Add(m)
            elseif pars:Length > 0
                // check to see if there are default parameters for the method
                var nonDefault := CountNonDefaultParameters(pars)
                if uArgs:Length >= nonDefault
                    found:Add(m)
                endif
            endif
        next
        if found:Count == 1
            return found:First() // collection, so 0 based !
        endif
        var filtered := List<T>{}
        filtered:AddRange(found)
        // then look for methods with
        found:Clear()
        foreach var m1 in filtered
            foreach var m2 in filtered
                if (m2 != m1)
                    var result := OOPHelpers.CompareMethods(m1, m2, uArgs)
                    if result == 1
                        if ! found:Contains(m1)
                            found:Add(m1)
                        endif
                        if found:Contains(m2)
                            found:Remove(m2)
                        endif
                    elseif result == 2
                        if ! found:Contains(m2)
                            found:Add(m2)
                        endif
                        if found:Contains(m1)
                            found:Remove(m1)
                        endif
                    endif
                endif
            next
        next
        if found:Count == 1
            return found:First()
        endif
        local cClass as string
        cClass := overloads:First():DeclaringType:Name
        var oError := Error.VOError( EG_AMBIGUOUSMETHOD, cFunction, "MethodName", 1, <object>{cClass+":"+overloads:First():Name})

        local sb as StringBuilder
        sb := StringBuilder{}
        sb:AppendLine(oError:Message)
        sb:AppendLine(i"Found {found:Count} overloads")
        var current := 0

        foreach var overload in found
            current += 1
            sb:Append( ei"{current}. {overload:DeclaringType:Name}:{overload:Name}")
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
            sb:Append( "(")

            var firstParam := true
            foreach p as ParameterInfo in overload:GetParameters()
                if firstParam
                    firstParam := false
                else
                    sb:Append(  ", ")
                endif
                sb:Append( i"{p:Name} AS {GetTypename(p:ParameterType)}")
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
        hasByRef := false
        var aPars := methodinfo:GetParameters()
        var numDefinedParameters := aPars:Length
        var numActualParameters  := args:Length
        if numDefinedParameters == 1 .and. methodinfo:IsDefined(TYPEOF(ClipperCallingConventionAttribute),false)
            lClipper := true
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
            if numDefinedParameters < numActualParameters
                // ignore extra parameters
                numActualParameters := numDefinedParameters
            endif
            for var nPar := 0 to numActualParameters -1
                local pi        := aPars[nPar] as ParameterInfo
                local parType   := pi:ParameterType as System.Type
                local arg       := args[nPar] as usual
                if parType:IsByRef
                    // Get the referenced type. We assume it is in the assembly where the ByRef type is also defined
                    // I am not sure if that is always true ?
                    hasByRef := true
                    var typeName := parType:FullName
                    typeName := typeName:Substring(0, typeName:Length-1)
                    try
                        var referencedType := parType:Assembly:GetType(typeName)
                        if referencedType != null
                            parType := referencedType
                        endif
                    catch
                        nop
                    end try
                endif
                if parType == TYPEOF(usual)
                    // We need to box a usual here
                    oArgs[nPar] := __castclass(object, arg)
                elseif arg == nil
                    // This is new in X#: a NIL in the middle of the parameter list gets set to the default value now
                    oArgs[nPar] := OOPHelpers.GetDefaultValue(pi)
                elseif arg == null .or. parType:IsAssignableFrom(arg:SystemType) // Null check must appear first !
                    oArgs[nPar] := arg
                elseif pi:GetCustomAttributes( TYPEOF( ParamArrayAttribute ), false ):Length > 0
                    // Parameter array of certain type
                    // -> convert remaining elements from uArgs to an array and assign that to oArgs[i]
                    local elementType := parType:GetElementType() as System.Type
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
            for var nArg := numActualParameters to numDefinedParameters -1
                local oPar as ParameterInfo
                oPar        := aPars[nArg]
                var oArg    := OOPHelpers.GetDefaultValue(oPar)
                if oArg != null
                    oArgs[nArg] := oArg
                else
                    oArgs[nArg] := nil
                endif
            next
        endcase
        return oArgs

    static method GetDefaultValue(oPar as ParameterInfo) as object
        local result := null as object
        if oPar:HasDefaultValue
            result := oPar:DefaultValue
        else
            local oDefAttrib as DefaultParameterValueAttribute
            oDefAttrib := (DefaultParameterValueAttribute) oPar:GetCustomAttribute(TypeOf(DefaultParameterValueAttribute))
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
                    // for usuals there is no need to convert.
                    if oPar:ParameterType != typeof(usual)
                        result := Convert.ChangeType(result,oPar:ParameterType)
                    endif
                end switch
            end if
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

    static method IVarHelper(o as object, cName as string, lGet as logic) as dword

        if o == null
            return 0
        endif

        var t := o:GetType()

        var fi := t:GetField( cName, BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic  | BindingFlags.IgnoreCase)
        if fi != null
            if fi:IsPublic
                return 2U
            elseif fi:IsFamily
                VAR att := TYPEOF( XSharp.Internal.IsInstanceAttribute )
                var atts := fi:GetCustomAttributes(att,FALSE)
                if (atts:Length > 0)
                    return 1U
                endif
                RETURN 0U
            endif
        endif

        do while t != null
            var pi := t:GetProperty( cName , BindingFlags.Instance | BindingFlags.Public | BindingFlags.IgnoreCase )
            if pi != null .and. ( (lGet .and. pi:CanRead) .or. (.not. lGet .and. pi:CanWrite) )
                return 3U
            else
                t := t:BaseType
            endif
        enddo

        return 0U

    static method IVarList( t as Type ) as array
        if t == null
            return null_array
        endif
        // Note that VO only returns PUBLIC properties and fields
        var aFields := t:GetFields( BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic)
        var list := List<string>{}
        foreach fi as FieldInfo in aFields
            if fi:IsPublic || (fi:IsFamily  .and. fi:IsDefined(typeof(IsInstanceAttribute), false))
                var name := fi:Name:ToUpperInvariant()
                if ! list:Contains(name)
                    list:Add(name)
                endif
            endif
        next

        var aProps := t:GetProperties( BindingFlags.Instance | BindingFlags.Public )

        foreach pi as PropertyInfo in aProps
            var name := pi:Name:ToUpperInvariant()
            if ! list:Contains(name)
                list:Add(name)
            endif
        next
        return list:ToVoSymArray()


    static method MethodList(t as Type) as array
        var list := List<string>{}
        var aInfo := t:GetMethods( BindingFlags.Instance | BindingFlags.Public )
        foreach oMI as MethodInfo in aInfo
            if !oMI:IsSpecialName .and. ! list:Contains(oMI:Name)
                list:Add(oMI:Name )
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
            var aInfo := type:GetMembers(BindingFlags.Instance + BindingFlags.Public + BindingFlags.NonPublic)
            foreach oInfo as MemberInfo in aInfo
                var name := oInfo:Name:ToUpperInvariant()
                do case
                    case oInfo:MemberType == MemberTypes.Field
                        if listVar:IndexOf(name)  == -1 .and. ((FieldInfo)oInfo):IsPublic
                            listVar:Add(name)
                        end if
                    case oInfo:MemberType == MemberTypes.Property
                        if listVar:IndexOf(name)  == -1
                            listVar:Add(name)
                        end if
                    case oInfo:MemberType == MemberTypes.Method
                        if listMethod:IndexOf(name)  == -1 .and. .not. ((MethodInfo)oInfo):IsSpecialName
                            listMethod:Add(name)
                        end if
                end case
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
        var mi := OOPHelpers.GetMember(t, cName)
        if mi != null
            if mi is PropertyInfo var pi
                // we must check. Sometimes in a subclass the Access was overwritten but not the assign
                // then we want to read the assign from the parent class
                if lAccess .and. pi:CanRead .and. IsPropertyMethodVisible(pi:GetMethod, lSelf)
                    return pi
                elseif ! lAccess .and. pi:CanWrite .and. IsPropertyMethodVisible(pi:SetMethod, lSelf)
                    return pi
                endif
            else
                return null
            endif
//        else
//            var pi := OOPHelpers.FindProperty(t:BaseType, cName, lAccess, lSelf)
//            if pi != null
//                return pi
//            endif
        endif

        var bf := BindingFlags.Instance | BindingFlags.IgnoreCase |  BindingFlags.DeclaredOnly | BindingFlags.Public
        if lSelf
            bf |= BindingFlags.NonPublic
        endif
        do while t != null
            var oInfo := t:GetProperty( cName, bf)
            if oInfo != null .and. ( (lAccess .and. oInfo:CanRead) .or. (.not. lAccess .and. oInfo:CanWrite) )
                AddMember(t, cName, oInfo)
                return oInfo
            else
                t := t:BaseType
            endif
        enddo
        return null

    static method IsPropertyMethodVisible(oMethod as MethodInfo, lSelf as logic) as logic
        if oMethod == null_object
            return false
        elseif oMethod:IsPublic
            return true
        elseif lSelf .and. (oMethod:IsFamily .or. oMethod:IsFamilyOrAssembly)
            return true
        endif
        return false


    static method GetMember(t as Type, cName as string) as MemberInfo
        if t != null .and. ! String.IsNullOrEmpty(cName) .and. fieldPropCache:ContainsKey(t)
            var fields := fieldPropCache[t]
            if fields:ContainsKey(cName)
                var result := fields[cName]
                return result
            endif
        endif
        return null

    static method AddMember(t as Type, cName as string, mi as MemberInfo) as logic
        if t != null .and. ! String.IsNullOrEmpty(cName)
            if ! fieldPropCache:ContainsKey(t)
                fieldPropCache:Add( t, Dictionary<string, MemberInfo> {StringComparison.OrdinalIgnoreCase})
            endif
            var fields := fieldPropCache[t]
            if !fields:ContainsKey(cName)
                fields:Add(cName, mi)
                return true
            endif
        endif
        return false


    static method FindField( t as Type, cName as string, lAccess as logic, lSelf as logic ) as FieldInfo
        if t == null .or. String.IsNullOrEmpty(cName)
            return null
        endif
        var mi := OOPHelpers.GetMember(t, cName)
        if mi != null
            if mi is FieldInfo var fi .and. IsFieldVisible(fi, lSelf)
                return fi
            endif
            return null     // it must be a property then
        endif
        var bt := t
        var bf := BindingFlags.Instance | BindingFlags.IgnoreCase |  BindingFlags.DeclaredOnly | BindingFlags.Public
        if lSelf
            bf |= BindingFlags.NonPublic
        endif
        do while t != null
            var oInfo := t:GetField( cName, bf )
            if oInfo != null
                // check for readonly (initonly) fields
                if lAccess .or. ! oInfo:Attributes:HasFlag(FieldAttributes.InitOnly)
                    OOPHelpers.AddMember(bt, cName, oInfo)
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
        elseif lSelf .and. (oFld:IsFamily .or. oFld:IsFamilyOrAssembly)
            return true
        endif
        return false

    /// <summary>
    /// This method returns TRUE when the assembly from which an IVarGet()
    /// or IVarPut() was called is the same assembly in which a property was defined.
    /// </summary>
    /// <param name="propInfo">Property that we are checking</param>
    /// <returns>TRUE when the first stackframe outside of XSharp.RT is in the same assembly as <paramref name="propInfo"/></returns>
    static method IsInternalVisible(propInfo as PropertyInfo) as logic
        local asm       := propInfo:DeclaringType:Assembly  as Assembly
        local frames    := StackTrace{false} :GetFrames()   as StackFrame[]
        local thisasm   := typeof(__Usual):Assembly         as Assembly         // XSharp.RT
        foreach frame as StackFrame in frames
            var frameAsm := frame:GetMethod():DeclaringType:Assembly
            if frameAsm != thisasm
                if frameAsm == asm
                    return true
                endif
                exit
            endif
        next
        return false

    static method IVarGet(oObject as object, cIVar as string, lSelf as logic) as usual
        local t as Type
        local result as object
        if oObject == null_object
            throw Error.NullArgumentError(__function__, nameof(oObject),1)
        endif
        if String.IsNullOrEmpty(cIVar)
            throw Error.NullArgumentError(__function__, nameof(cIVar),2)
        endif
        // VFP Empty and XPP DataObject and other objects that implement IDynamicProperties
        if oObject is IDynamicProperties var oDynamic
            return oDynamic:NoIvarGet(cIVar)
        endif
        t := oObject:GetType()
        try
            var propInfo := OOPHelpers.FindProperty(t, cIVar, true, lSelf)
            if propInfo != null_object .and. propInfo:CanRead
                var visible := lSelf .or. propInfo:GetMethod:IsPublic
                if (! visible .and. propInfo:GetMethod:IsAssembly)
                    visible := IsInternalVisible(propInfo)
                endif
                if propInfo:GetIndexParameters():Length == 0
                    if visible
                        result := propInfo:GetValue(oObject, null)
                        if result == null .and. propInfo:PropertyType == TYPEOF(System.String)
                            result := String.Empty
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
                if result == null .and. fldInfo:FieldType == TYPEOF(System.String)
                    result := String.Empty
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
        if SendHelper(oObject, "NoIVarGet", <usual>{cIVar}, out var oResult)
            return oResult
        end if
        var oError := Error.VOError( EG_NOVARMETHOD, iif( lSelf, __function__, __function__ ), nameof(cIVar), 2, <object>{oObject, cIVar} )
        oError:Description := oError:Message+" '"+cIVar+"'"
        throw oError

    static method IVarPut(oObject as object, cIVar as string, oValue as object, lSelf as logic)  as void
        local t as Type
        if oObject == null_object
            throw Error.NullArgumentError(__function__, nameof(oObject),1)
        endif
        if String.IsNullOrEmpty(cIVar)
            throw Error.NullArgumentError(__function__, nameof(cIVar),2)
        endif
        // VFP Empty and XPP DataObject and other objects that implement IDynamicProperties
        if oObject is IDynamicProperties var oDynamic
            oDynamic:NoIvarPut(cIVar, oValue)
            return
        endif
        t := oObject:GetType()
        try
            var propInfo := OOPHelpers.FindProperty(t, cIVar, false, lSelf)
            if propInfo != null_object .and. propInfo:CanWrite
                var visible := lSelf .or. propInfo:SetMethod:IsPublic
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
                oValue := OOPHelpers.ValueConvert(oValue, fldInfo:FieldType)
                fldInfo:SetValue(oObject, oValue)
                return
            endif
            cIVar := cIVar:ToUpperInvariant()
            if SendHelper(oObject, "NoIVarPut", <usual>{cIVar, oValue})
                return
            end if
            var oError :=  Error.VOError( EG_NOVARMETHOD, iif( lSelf, __function__, __function__ ), nameof(cIVar), 2, <object>{oObject, cIVar, oValue, lSelf})
            oError:Description := oError:Message+" '"+cIVar+"'"
            throw oError
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
        local lOk := OOPHelpers.SendHelper(oObject, cMethod, uArgs, out var result) as logic
        oObject := result   // get rid of warning
        return lOk

    static method FindOverloads(t as System.Type, cMethod as string, lInstance as logic) as IList<MethodInfo>
        var list := List<MethodInfo>{}
        local bf as BindingFlags
        if lInstance
            bf := BindingFlags.Instance | BindingFlags.Public
        else
            bf := BindingFlags.Static | BindingFlags.Public | BindingFlags.DeclaredOnly
        endif
        foreach var minfo in t:GetMethods(bf)
            if !minfo:IsSpecialName .and. String.Compare(minfo:Name, cMethod, StringComparison.OrdinalIgnoreCase) == 0
                list:Add(minfo)
            endif
        next
        if list:Count > 0
            CacheOverLoads(t, cMethod, list)
        endif
        return list

    static method GetCachedOverLoads(t as System.Type, cMethod as string) as IList<MethodInfo>
        if t == null .or. String.IsNullOrEmpty(cMethod)
            return null
        endif
        if overloadCache:ContainsKey(t)
            var type := overloadCache[t]
            if type:ContainsKey(cMethod)
                var result := type[cMethod]
                return result
            endif
        endif
        return null

    static method CacheOverLoads(t as System.Type, cMethod as string, ml as IList<MethodInfo>) as logic
        if !overloadCache:ContainsKey(t)
            overloadCache:Add(t, Dictionary<string, IList<MethodInfo> >{StringComparer.OrdinalIgnoreCase})
        endif
        var type := overloadCache[t]
        if type:ContainsKey(cMethod)
            return false
        endif
        type:Add(cMethod, ml)
        return true

    static method SendHelper(oObject as object, cMethod as string, uArgs as usual[], result out usual) as logic
        local t := oObject?:GetType() as Type
        result := nil
        if t == null
            throw Error.NullArgumentError( cMethod, nameof(oObject), 1 )
        endif
        if cMethod == null
            throw Error.NullArgumentError( cMethod, nameof(cMethod), 2 )
        endif
        local mi := null as MethodInfo
        cMethod := cMethod:ToUpperInvariant()
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
                    var mis := list:ToArray()
                    mi := OOPHelpers.FindBestOverLoad(mis, cMethod,uArgs)
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
        return OOPHelpers.SendHelper(oObject, mi, uArgs, out result)

    static method SendHelper(oObject as object, mi as MethodInfo , uArgs as usual[], result out usual) as logic
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
                if mi:ReturnType == typeof(usual)
                    result := mi:Invoke(oObject, oArgs)
                else
                    local oResult as object
                    oResult := mi:Invoke(oObject, oArgs)
                    if oResult == null .and. mi:ReturnType == TYPEOF(string)
                        oResult := String.Empty
                    endif
                    result := oResult
                endif
                if hasByRef
                    OOPHelpers.CopyByRefParameters( uArgs, oArgs, mi:GetParameters())
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
                if e:InnerException != null
                    var ex := Error{e:GetInnerException()}
                    local stack := ex:StackTrace as string
                    if stack:IndexOf(mi:Name,StringComparison.OrdinalIgnoreCase) == -1
                        // we have stripped too many layers. Strip until we see the method name we are trying to call
                        do while e:InnerException != null .and. stack:IndexOf(mi:Name,StringComparison.OrdinalIgnoreCase ) ==  -1
                            e := e:InnerException
                            stack := e:StackTrace
                        enddo
                        ex := Error{e}
                        if e is Error var er
                            ex:Args := er:Args
                        endif
                    endif
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
        foreach oMember as MethodInfo in srcType:GetMember("op_Implicit")
            if oMember:ReturnType == toType
                return oMember
            endif
        next
        foreach oMember as MethodInfo in srcType:GetMember("op_Explicit")
            if oMember:ReturnType == toType
                return oMember
            endif
        next
        return null_object


    static method ValueConvert(uValue as usual,toType as System.Type) as object
        local oValue := null as object
        if toType == TYPEOF(float)
            return (float) uValue
        elseif uValue:SystemType == toType
            return uValue
        else
            if toType == TYPEOF(usual)
                // return a boxed usual
                return __castclass(object, uValue)
            elseif toType == typeof(date) .and. uValue:IsDateTime
                return (date)(DateTime) uValue
            elseif uValue:IsArray .and. toType == typeof(array)
                return (array) uValue
            elseif uValue:IsString .and. toType == typeof(symbol)
                return (symbol) uValue
            elseif uValue:IsSymbol .and. toType == typeof(string)
                return (string) uValue
            elseif uValue:IsObject .or. uValue:IsCodeblock
                return (object) uValue
            elseif uValue:IsPtr .and. (toType == typeof(ptr) .or. toType:IsPointer)
                return IntPtr{(ptr) uValue}
            else
                // check to see if the source type contains an implicit converter
                local oRealValue := uValue as object
                var oOperator := FindOperator(oRealValue:GetType(), toType)
                if oOperator != null_object
                    oValue := oRealValue
                else
                    oOperator := FindOperator(typeof(usual), toType)
                    if oOperator != null_object
                        // box the usual
                        oValue := __castclass(object, uValue)
                    endif
                endif
                if oOperator != null_object
                    // oValue is either a boxed USUAL (for operators of the USUAL type)
                    // or the real thing, depending on the operator that was chosen
                    TRY
                        return oOperator:Invoke(null, <object>{oValue})
                    CATCH
                        local ex as Error
                        ex := Error{Gencode.EG_WRONGCLASS, "", "Could not convert value "+oValue:ToString() + " to type " + toType:Name}
                        ex:FuncSym := __FUNCTION__
                        ex:Stack := ErrorStack()
                        throw ex

                    END TRY
                endif
            endif
            // when we get here then there is no operator and we will try to change the type..
            local oRet as object
            try
                oRet := uValue
                oRet := Convert.ChangeType(oRet, toType)
            catch
                oRet := uValue
            end try
            return oRet
        endif

    static method DoSend(oObject as object, cMethod as string, args as usual[], cCaller AS STRING) as usual
        if oObject == null
            throw Error.NullArgumentError( cCaller, nameof(oObject), 1 )
        endif
        if cMethod == null
            throw Error.NullArgumentError( cCaller, nameof(cMethod), 2 )
        endif
        if ! OOPHelpers.SendHelper(oObject, cMethod, args, out var result)
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
            if ! OOPHelpers.SendHelper(oObject, "NoMethod" , nomethodArgs, out result)
                var oError := Error.VOError( EG_NOMETHOD, cCaller, nameof(cMethod), 2, <object>{oObject, cMethod, args} )
                oError:Description  := oError:Message + " '"+cMethod+"'"
                throw oError

            endif
        endif
        return result

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
    return ClassList():Length

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
function _CreateInstance(symClassName as string, InitArgList as usual[]) as object

    var t := OOPHelpers.FindClass(symClassName)
    if t == null
            var oError := Error.VOError( EG_NOCLASS, __function__, nameof(symClassName), 1,  <object>{symClassName}  )
            oError:Description := oError:Message+" '"+symClassName+"'"
            throw oError
    endif
    var constructors := t:GetConstructors()
    local ctor := OOPHelpers.FindBestOverLoad(constructors, __function__ ,InitArgList) as ConstructorInfo
    if ctor == null
        var oError := Error.VOError( EG_NOMETHOD, __function__, "Constructor", 0 , null)
        oError:Description := "No CONSTRUCTOR defined for type "+ (string) symClassName
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


/// <summary>
/// Find a class in the referenced assemblies
/// </summary>
/// <param name="cClassName">Classname to find</param>
/// <returns>System.Type object or NULL </returns>

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
    IF oType == NULL
        RETURN FALSE
    END IF
    RETURN oType:IsAssignableFrom(oObject:GetType())*/
    local oType as Type
    oType := oObject:GetType()
    do while oType != null
        if String.Compare(oType:Name, symClassName, true) == 0
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
    return OOPHelpers.IVarGet(oObject, symInstanceVar, false)

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

function IVarPut(oObject as object,symInstanceVar as string,uValue as usual) as usual
    if oObject == null_object
        throw Error.NullArgumentError(__function__, nameof(oObject),1)
    endif
    if String.IsNullOrEmpty(symInstanceVar)
        throw Error.NullArgumentError(__function__, nameof(symInstanceVar),2)
    endif
    OOPHelpers.IVarPut(oObject, symInstanceVar, uValue, false)
    return uValue

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/ivarputself/*" />

function IVarPutSelf(oObject as object,symInstanceVar as string,uValue as usual) as usual
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
    uResult := OOPHelpers.DoSend(oToSend, cMethod, MethodArgList, __Function__)
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
    return OOPHelpers.DoSend(oObject, cMethod, args, __Function__)

/// <summary>Helper function to convert ARRAY to USUAL[]</summary>
/// <param name="args">X# array to convert</param>
/// <returns>USUAL Array</returns>
/// <remarks>This is a helper function used for late bound code that can also be called from user code.</remarks>
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

/// <summary>Helper function to convert ARRAY to OBJECT[]</summary>
/// <param name="args">X# array to convert</param>
/// <returns>OBJECT Array</returns>
/// <remarks>This is a helper function used for late bound code that can also be called from user code.</remarks>
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

/// <summary>Helper function to convert USUAL[] to OBJECT[]</summary>
/// <param name="args">USUAL array to convert</param>
/// <returns>OBJECT Array</returns>
/// <remarks>This is a helper function used for late bound code that can also be called from user code.</remarks>
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

/// <summary>Helper function to convert OBJECT[] to USUAL[]</summary>
/// <remarks>This is a helper function used for late bound code that can also be called from user code.</remarks>
/// <param name="args">OBJECT array to convert</param>
/// <returns>USUAL Array</returns>
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
    return OOPHelpers.DoSend(oObject, cmethod, uArgs , __Function__)


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/mparamcount/*" />
function MParamCount(symClass as string,symMethod as string) as dword
    local type as Type
    type := OOPHelpers.FindClass(symClass)
    if type != null
        local met as MethodInfo
        met := OOPHelpers.FindMethod(type, symMethod, true)
        if met != null
            if met:IsDefined(TYPEOF(ClipperCallingConventionAttribute),false)
                // calculate the # of parameters
                var oAttr := (ClipperCallingConventionAttribute) met:GetCustomAttributes(TYPEOF(ClipperCallingConventionAttribute), false):First()
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




/// <summary>Return the number of local arguments that a function is expecting.</summary>
/// <param name="symFunction">The name of the function to examine.</param>
/// <returns>The number of arguments that a method is expecting.</returns>
/// <remarks>Note that you can't use this for functions that are overloaded.<br/>
/// And unlike in VO this function can also be used to return the number of parameters for typed functions.</remarks>

function FParamCount(symFunction as string) as dword
    local aFuncs as MethodInfo[]
    aFuncs := OOPHelpers.FindClipperFunctions(symFunction)
    // CLipper functions can't and shouldn't have overloads
    if aFuncs != null
        if aFuncs:Length == 1
            local oMI := aFuncs:First() as MethodInfo
            if oMI:IsDefined(TYPEOF(ClipperCallingConventionAttribute),false)
                // calculate the # of parameters
                local oAttr as ClipperCallingConventionAttribute
                oAttr := (ClipperCallingConventionAttribute) oMI:GetCustomAttributes(TYPEOF(ClipperCallingConventionAttribute), false):First()
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


/// <summary>Call a clipper function by name</summary>
/// <param name="symFunction">The name of the function to call.</param>
/// <param name="aArgs">The list of arguments to pass to the function</param>
/// <returns>The return value of the function</returns>
/// <remarks>Note that X# allows to call functions that are overloaded.</remarks>

function _CallClipFunc(symFunction as string,aArgs as array) as usual
    return _CallClipFunc(symFunction, _ArrayToUsualArray(aArgs))

/// <summary>Call a function by name</summary>
/// <param name="symFunction">The name of the function to call.</param>
/// <param name="uArgs">The list of arguments to pass to the function</param>
/// <returns>The return value of the function</returns>
/// <remarks>Note that X# allows to call functions that are overloaded.</remarks>
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
            oMI  := OOPHelpers.FindBestOverLoad(aFuncs, symFunction, uArgs)
            if oMI != null
                if OOPHelpers.SendHelper(null, oMI, uArgs, out var result)
                    return result
                endif
            endif
            throw Error.VOError( EG_AMBIGUOUSMETHOD,  __Function__, nameof(symFunction), 1, <object>{symFunction} )
        endif
    else
        throw Error.VOError( EG_NOFUNC,  "FParamCount", nameof(symFunction), 1, <object>{symFunction} )
    endif
    return  nil

function _HasClipFunc(symFunction as string) as logic
    local aFuncs as MethodInfo[]
    aFuncs := OOPHelpers.FindClipperFunctions(symFunction)
    return aFuncs:Length > 0


/// <summary>Dynamically loads a library (dll) compiled with X#, running any _INIT procedures it may contain.</summary>
/// <param name="cLibFileName">The full path of the library to load.</param>
/// <returns>The Assembly object of the loaded library.</returns>
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

