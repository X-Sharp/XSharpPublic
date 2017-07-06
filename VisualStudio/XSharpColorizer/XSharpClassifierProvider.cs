//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Utilities;

namespace XSharpColorizer
{
    /// <summary>
    /// Classifier provider. It adds the classifier to the set of classifiers.
    /// </summary>
    [Export(typeof(IClassifierProvider))]
    [ContentType("XSharp")] 
    internal class XSharpClassifierProvider : IClassifierProvider
    {
        // Disable "Field is never assigned to..." compiler's warning. Justification: the field is assigned by MEF.
#pragma warning disable 649

        /// <summary>
        /// Classification registry to be used for getting a reference
        /// to the custom classification type later.
        /// </summary>
        [Import]
        private IClassificationTypeRegistryService classificationRegistry;

        [Import]
        ITextDocumentFactoryService factory = null;

#pragma warning restore 649


        #region IClassifierProvider

        /// <summary>
        /// Gets a classifier for the given text buffer.
        /// </summary>
        /// <param name="buffer">The <see cref="ITextBuffer"/> to classify.</param>
        /// <returns>A classifier for the text buffer, or null if the provider cannot do so in its current state.</returns>
        public IClassifier GetClassifier(ITextBuffer buffer)
        {
            if (! SupportFunctions.IsXSharpDocument(factory, buffer))
                return null;
            return buffer.Properties.GetOrCreateSingletonProperty<XSharpClassifier>(creator: () => XSharpClassifier.GetColorizer(buffer, this.classificationRegistry, this.factory));
        }

        #endregion
    }
    internal static class SupportFunctions
    {
        internal static bool IsXSharpDocument(ITextDocumentFactoryService factory, ITextBuffer buffer)
        {
            string path = "";
            if (buffer.Properties.ContainsProperty(typeof(XSharpModel.XFile)))
            {
                return buffer.Properties.GetProperty(typeof(XSharpModel.XFile)) != null;
            }
            if (factory != null)
            {
                ITextDocument doc = null;
                if (factory.TryGetTextDocument(buffer, out doc))
                {
                    path = doc.FilePath;
                }
            }
            // check if X# document
            var file = XSharpModel.XSolution.FindFile(path);
            buffer.Properties.AddProperty(typeof(XSharpModel.XFile), file );
            return file != null;
        }
    }
}
