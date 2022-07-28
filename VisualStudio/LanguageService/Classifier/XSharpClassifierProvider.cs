//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Utilities;
using Microsoft.VisualStudio.Text.Editor;

namespace XSharp.LanguageService
{
    /// <summary>
    /// Classifier provider. It adds the classifier to the set of classifiers.
    /// </summary>
    [Export(typeof(IClassifierProvider))]
    [ContentType(Constants.LanguageName)]
    [TextViewRole(PredefinedTextViewRoles.Document)]
    internal class XSharpClassifierProvider : IClassifierProvider
    {
        /// <summary>
        /// Classification registry to be used for getting a reference
        /// to the custom classification type later.
        /// </summary>
        [Import]
        readonly IClassificationTypeRegistryService classificationRegistry= null;

        [Import]
        readonly ITextDocumentFactoryService factory = null;

        #region IClassifierProvider

        /// <summary>
        /// Gets a classifier for the given text buffer.
        /// </summary>
        /// <param name="buffer">The <see cref="ITextBuffer"/> to classify.</param>
        /// <returns>A classifier for the text buffer, or null if the provider cannot do so in its current state.</returns>
        public IClassifier GetClassifier(ITextBuffer buffer)
        {
            // only return a classifier when this is really our document
            if (!factory.IsXSharpDocument( buffer))
                return null;
            return buffer.Properties.GetOrCreateSingletonProperty(creator: () =>
                XSharpClassifier.Create(buffer, this.classificationRegistry, this.factory));
        }

        #endregion
    }

}
