//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System.Diagnostics.CodeAnalysis;

namespace Microsoft.VisualStudio.Project
{

   /// <summary>
   /// This interface provides the ability to identify the items which have the capability of including / excluding
   /// themselves to / from the project system. It also tells if the item is a member of the project or not.
   /// </summary>
   public interface IProjectSourceNode
   {
      /// <summary>
      /// Gets if the item is not a member of the project.
      /// </summary>
      [SuppressMessage( "Microsoft.Naming", "CA1702:CompoundWordsShouldBeCasedCorrectly", MessageId = "NonMember" )]
      bool IsNonMemberItem
      {
         get;
      }

      /// <summary>
      /// Exclude the item from the project system.
      /// </summary>
      /// <returns>Returns success or failure code.</returns>
      int ExcludeFromProject();

      /// <summary>
      /// Include the item into the project system.
      /// </summary>
      /// <returns>Returns success or failure code.</returns>
      int IncludeInProject();

      /// <summary>
      /// Include the item into the project system recursively.
      /// </summary>
      /// <param name="recursive">Flag that indicates if the inclusion should be recursive or not.</param>
      /// <returns>Returns success or failure code.</returns>
      int IncludeInProject( bool recursive );
   }
}
