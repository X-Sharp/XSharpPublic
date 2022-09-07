//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


USING System
USING System.Collections.Generic
USING System.Text
USING System.Linq
// todo: Move class to the XSharp namespace and rename to LinqExtensionMethods
begin namespace System.Linq
public static class ExtensionMethods
#region Float

    public static method Sum(self source as IEnumerable<float>) as float
        if (source == null)
            throw ArgumentNullException{"source"}
        endif
        local sum := 0 as float
        begin checked
            foreach v as float in source
                sum += v
            next
        end checked
        return sum;

    public static method Sum<TSource>(self source as IEnumerable<TSource>, selector as Func<TSource, float>) as float
        return ExtensionMethods.Sum(Enumerable.Select<TSource, float>(source, selector))


    public static method Min(SELF source as IEnumerable<float> )  as float
         if (source == null)
            throw ArgumentNullException{"source"}
         endif
         local result := 0.0 as float
         local hasValue := false as logic
         foreach var x in source
            if (hasValue)
                if (x < result)
                    result := x
                endif

            else
                result := x
                hasValue := true
            endif
         next
         if hasValue
            return result
         endif
         throw Exception{"No elements in the collection"}

    public static method Min<TSource>(self source as IEnumerable<TSource>, selector as Func<TSource, float>) as float
        return ExtensionMethods.Sum(Enumerable.Select<TSource, float>(source, selector))

    public static method Max(SELF source as IEnumerable<float> )  as float
         if (source == null)
            throw ArgumentNullException{"source"}
         endif
         local result := 0.0 as float
         local hasValue := false as logic
         foreach var x in source
            if (hasValue)
                if (x > result)
                    result := x
                endif

            else
                result := x
                hasValue := true
            endif
         next
         if hasValue
            return result
         endif
         throw Exception{"No elements in the collection"}


    public static method Max<TSource>(self source as IEnumerable<TSource>, selector as Func<TSource, float>) as float
        return ExtensionMethods.Sum(Enumerable.Select<TSource, float>(source, selector))

#endregion

#region Currency

    public static method Sum(self source as IEnumerable<currency>) as currency
        if (source == null)
            throw ArgumentNullException{"source"}
        endif
        local sum := 0 as currency
        begin checked
            foreach v as currency in source
                sum += v
            next
        end checked
        return sum;

    public static method Sum<TSource>(self source as IEnumerable<TSource>, selector as Func<TSource, currency>) as currency
        return ExtensionMethods.Sum(Enumerable.Select<TSource, currency>(source, selector))


    public static method Min(SELF source as IEnumerable<currency> )  as currency
         if (source == null)
            throw ArgumentNullException{"source"}
         endif
         local result := $0.0 as currency
         local hasValue := false as logic
         foreach var x in source
            if (hasValue)
                if (x < result)
                    result := x
                endif

            else
                result := x
                hasValue := true
            endif
         next
         if hasValue
            return result
         endif
         throw Exception{"No elements in the collection"}

    public static method Min<TSource>(self source as IEnumerable<TSource>, selector as Func<TSource, currency>) as currency
        return ExtensionMethods.Sum(Enumerable.Select<TSource, currency>(source, selector));

    public static method Max(SELF source as IEnumerable<currency> )  as currency
         if (source == null)
            throw ArgumentNullException{"source"}
         endif
         local result := $0.0 as currency
         local hasValue := false as logic
         foreach var x in source
            if (hasValue)
                if (x > result)
                    result := x
                endif

            else
                result := x
                hasValue := true
            endif
         next
         if hasValue
            return result
         endif
         throw Exception{"No elements in the collection"}


    public static method Max<TSource>(self source as IEnumerable<TSource>, selector as Func<TSource, currency>) as currency
        return ExtensionMethods.Sum(Enumerable.Select<TSource, currency>(source, selector));


#endregion


end class
end namespace
