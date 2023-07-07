//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
BEGIN NAMESPACE XSharp.Settings
STATIC CLASS XEditorSettings

    PUBLIC STATIC PROPERTY KeywordCase                        AS KeywordCase AUTO
    PUBLIC STATIC PROPERTY IdentifierCase                     AS LOGIC AUTO
    PUBLIC STATIC PROPERTY UDCKeywordCase                     AS LOGIC AUTO


    // Enable / Disable features
    PUBLIC STATIC PROPERTY DisableCodeCompletion              AS LOGIC AUTO
    PUBLIC STATIC PROPERTY DisableHighLightWord               AS LOGIC AUTO
    PUBLIC STATIC PROPERTY DisableKeywordMatching             AS LOGIC AUTO
    PUBLIC STATIC PROPERTY DisableLightBulb                   AS LOGIC AUTO
    PUBLIC STATIC PROPERTY DisableParameterInfo               AS LOGIC AUTO
    PUBLIC STATIC PROPERTY DisableQuickInfo                   AS LOGIC AUTO
    PUBLIC STATIC PROPERTY DisableRegions                     AS LOGIC AUTO
    PUBLIC STATIC PROPERTY DisableSyntaxHighlighting          AS LOGIC AUTO
    PUBLIC STATIC PROPERTY DisableBraceMatching               AS LOGIC AUTO
    PUBLIC STATIC PROPERTY DisableAutoOpen                    AS LOGIC AUTO

    PUBLIC STATIC PROPERTY KeywordsInAll                AS LOGIC AUTO
    PUBLIC STATIC PROPERTY HideAdvancedMembers          AS LOGIC AUTO
    PUBLIC STATIC PROPERTY CompletionListTabs           AS LOGIC AUTO
    PUBLIC STATIC PROPERTY CompletionListAfterEachChar  AS LOGIC AUTO
    PUBLIC STATIC PROPERTY CompletionAutoPairs          AS LOGIC AUTO
    PUBLIC STATIC PROPERTY CommitChars                  AS STRING AUTO

    PUBLIC STATIC PROPERTY NavigationIncludeFields        AS LOGIC AUTO
    PUBLIC STATIC PROPERTY NavigationSorted               AS LOGIC AUTO
    PUBLIC STATIC PROPERTY NavigationMembersOfCurrentTypeOnly AS LOGIC AUTO
    PUBLIC STATIC PROPERTY NavigationExcludeMembersFromOtherFiles AS LOGIC AUTO

    PUBLIC STATIC PROPERTY TabSize                     AS LONG AUTO
    PUBLIC STATIC PROPERTY IndentSize                 AS LONG AUTO
    PUBLIC STATIC PROPERTY TabsAsSpaces               AS LOGIC AUTO
    PUBLIC STATIC PROPERTY IndentStyle                AS LONG AUTO
    PUBLIC STATIC PROPERTY IndentFactor               AS LONG AUTO
    PUBLIC STATIC PROPERTY InsertFinalNewline         AS LOGIC AUTO
    PUBLIC STATIC PROPERTY TrimTrailingWhiteSpace     AS LOGIC AUTO


    // Dividers
    PUBLIC STATIC PROPERTY ShowDividers               AS LOGIC AUTO
    PUBLIC STATIC PROPERTY ShowSingleLineDividers     AS LOGIC AUTO

    // Indentation
    PUBLIC STATIC PROPERTY IndentTypeMembers               AS LOGIC AUTO
    PUBLIC STATIC PROPERTY IndentTypeFields                AS LOGIC AUTO
    PUBLIC STATIC PROPERTY IndentStatements                AS LOGIC AUTO
    PUBLIC STATIC PROPERTY IndentCaseContent               AS LOGIC AUTO
    PUBLIC STATIC PROPERTY IndentCaseLabel                 AS LOGIC AUTO
    PUBLIC STATIC PROPERTY IndentContinuedLines            AS LOGIC AUTO
    PUBLIC STATIC PROPERTY IndentPreprocessorLines         AS LOGIC AUTO
    PUBLIC STATIC PROPERTY IndentNamespace                 AS LOGIC AUTO
    PUBLIC STATIC PROPERTY MaxCompletionEntries             AS LONG AUTO := Int32.MaxValue
    PUBLIC STATIC PROPERTY CompleteNumChars                 AS LONG AUTO := 4


    // Code completion settings
    PUBLIC STATIC PROPERTY CompleteLocals              AS LOGIC AUTO := TRUE
    PUBLIC STATIC PROPERTY CompleteSelf                AS LOGIC AUTO := TRUE
    PUBLIC STATIC PROPERTY CompleteParent              AS LOGIC AUTO := TRUE
    PUBLIC STATIC PROPERTY CompleteNamespaces          AS LOGIC AUTO := TRUE
    PUBLIC STATIC PROPERTY CompleteTypes               AS LOGIC AUTO := TRUE
    PUBLIC STATIC PROPERTY CompleteFunctions           AS LOGIC AUTO := TRUE
    PUBLIC STATIC PROPERTY CompleteFunctionsP          AS LOGIC AUTO := TRUE
    PUBLIC STATIC PROPERTY CompleteFunctionsA          AS LOGIC AUTO := TRUE
    PUBLIC STATIC PROPERTY CompleteGlobals             AS LOGIC AUTO := TRUE
    PUBLIC STATIC PROPERTY CompleteGlobalsP            AS LOGIC AUTO := TRUE
    PUBLIC STATIC PROPERTY CompleteGlobalsA            AS LOGIC AUTO := TRUE
    PUBLIC STATIC PROPERTY CompleteKeywords            AS LOGIC AUTO := TRUE
    PUBLIC STATIC PROPERTY CompleteSnippets            AS LOGIC AUTO := TRUE


END CLASS

END NAMESPACE
