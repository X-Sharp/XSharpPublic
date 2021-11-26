//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

namespace Microsoft.VisualStudio.Project
{
    using System;
    using System.Runtime.Serialization;
    using System.ComponentModel;

    /// <summary>
    /// Exception thrown when an invalid property is entered on the project property pages
    /// </summary>
    [Serializable]
    public class ProjectPropertyArgumentException : ArgumentException
    {
        // =========================================================================================
        // Constructors
        // =========================================================================================

        /// <summary>
        /// Constructor for the ProjectPropertyArgumentException
        /// </summary>
        /// <param name="message">Error message associated with the exception</param>
        public ProjectPropertyArgumentException(string message)
            : base(message)
        {
        }

        /// <summary>
        /// Creates a new deserialized exception instance.
        /// </summary>
        /// <param name="info">Serialization info.</param>
        /// <param name="context">Streaming context.</param>
        protected ProjectPropertyArgumentException(SerializationInfo info, StreamingContext context)
            : base(info, context)
        {
        }
    }
}
