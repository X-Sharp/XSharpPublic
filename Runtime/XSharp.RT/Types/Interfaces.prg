//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

BEGIN NAMESPACE XSharp
    /// <summary>This interface can be used to access any object with an indexer. This is mostly used for elements inside typed arrays of the 'ARRAY OF' class.
    /// If you implement the interface on the elements you can use an array syntax to assess fields/properties in the elements of the array by name or ordinal.</summary>
    /// <seealso cref='T:XSharp.INamedIndexer' />
    /// <include file="RTComments.xml" path="Comments/ZeroBasedIndex/*" />
    INTERFACE IIndexedProperties
    	/// <include file="RTComments.xml" path="Comments/ZeroBasedIndexProperty/*" /> 
        /// <param name="index"><include file="RTComments.xml" path="Comments/ZeroBasedIndexParam/*" /></param>
        PROPERTY SELF[index AS INT   ] AS USUAL GET SET
        /// <param name="name">Name which will be used to lookup a property</param>
        PROPERTY SELF[name  AS STRING] AS USUAL GET SET
    END INTERFACE

    /// <summary> This interface is used to index a collection using the VO Array syntax.
    /// The interface is implemented by the ARRAY type in the runtime, but you can also use it for your custom types.</summary>
    /// <include file="RTComments.xml" path="Comments/ZeroBasedIndex/*" />
    /// <seealso cref='T:XSharp.__Array' />
    INTERFACE IIndexer
  		/// <include file="RTComments.xml" path="Comments/ZeroBasedIndexProperty/*" /> 
        /// <param name="index"><include file="RTComments.xml" path="Comments/ZeroBasedIndexParam/*" /></param>
        PUBLIC PROPERTY SELF[index PARAMS INT[]] AS USUAL GET SET
    END INTERFACE

    /// <summary>This interface is used to index a collection using a numeric and a string index and is implemented by the
    /// typed array class ('ARRAY OF'). If your elements inside the collection implement the IIndexProperties interface then
    /// the lookup of the property inside array element will be resolved with a call to the named indexer on that object.<remarks>
    /// <include file="RTComments.xml" path="Comments/ZeroBasedIndex/*" />
    /// <seealso cref='T:XSharp.IIndexedProperties' />
    /// <seealso cref='T:XSharp.__ArrayBase' />
    INTERFACE INamedIndexer
  		/// <include file="RTComments.xml" path="Comments/ZeroBasedIndexProperty/*" /> 
        /// <param name="index"><include file="RTComments.xml" path="Comments/ZeroBasedIndexParam/*" /></param>
        /// <param name="name"><include file="RTComments.xml" path="Comments/NameBasedIndexParam/*" /></param> 
        PUBLIC PROPERTY SELF[index AS INT, name AS STRING] AS USUAL GET SET
    END INTERFACE


END NAMESPACE    
