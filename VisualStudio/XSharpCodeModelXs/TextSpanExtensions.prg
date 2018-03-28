
using System

using LanguageService.CodeAnalysis.Text
using Microsoft.VisualStudio.Text
using Microsoft.VisualStudio.Text.Classification
using Microsoft.VisualStudio.Text.Tagging


BEGIN NAMESPACE XSharpModel

    STATIC CLASS TextSpanExtensions
        // Methods
        STATIC METHOD GetText( SELF snapshot AS ITextSnapshot, span AS TextSpan) AS string
            //
            RETURN snapshot:GetText(Span{span:Start, span:Length})

        STATIC METHOD ToClassificationSpan( SELF span AS TextSpan, snapshot AS ITextSnapshot, classificationType AS IClassificationType) AS ClassificationSpan
            //
            RETURN ClassificationSpan{SnapshotSpan{snapshot, span:Start, span:Length}, classificationType}

        STATIC METHOD ToTagSpan( SELF span AS TextSpan, snapshot AS ITextSnapshot, classificationType AS IClassificationType) AS ITagSpan<IClassificationTag>
            //
            RETURN TagSpan<IClassificationTag>{SnapshotSpan{snapshot, span:Start, span:Length}, ClassificationTag{classificationType}}


    END CLASS

END NAMESPACE 

