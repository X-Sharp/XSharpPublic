﻿// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System.Collections.Generic;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.SignatureHelp;
using Microsoft.CodeAnalysis.Editor.UnitTests.SignatureHelp;
using Microsoft.CodeAnalysis.Editor.UnitTests.Workspaces;
using Microsoft.CodeAnalysis.SignatureHelp;
using Microsoft.CodeAnalysis.Test.Utilities;
using Roslyn.Test.Utilities;
using Xunit;

namespace Microsoft.CodeAnalysis.Editor.CSharp.UnitTests.SignatureHelp
{
    public class GenericNameSignatureHelpProviderTests : AbstractCSharpSignatureHelpProviderTests
    {
        public GenericNameSignatureHelpProviderTests(CSharpTestWorkspaceFixture workspaceFixture) : base(workspaceFixture)
        {
        }

        internal override ISignatureHelpProvider CreateSignatureHelpProvider()
        {
            return new GenericNameSignatureHelpProvider();
        }

        #region "Declaring generic type objects"

        [Fact, Trait(Traits.Feature, Traits.Features.SignatureHelp)]
        public async Task NestedGenericTerminated()
        {
            var markup = @"
class G<T> { };

class C
{
    void Goo()
    {
        G<G<int>$$>
    }
}";

            var expectedOrderedItems = new List<SignatureHelpTestItem>();
            expectedOrderedItems.Add(new SignatureHelpTestItem("G<T>", string.Empty, string.Empty, currentParameterIndex: 0));

            await TestAsync(markup, expectedOrderedItems);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.SignatureHelp)]
        public async Task DeclaringGenericTypeWith1ParameterTerminated()
        {
            var markup = @"
class G<T> { };

class C
{
    void Goo()
    {
        [|G<$$|]>
    }
}";

            var expectedOrderedItems = new List<SignatureHelpTestItem>();
            expectedOrderedItems.Add(new SignatureHelpTestItem("G<T>", string.Empty, string.Empty, currentParameterIndex: 0));

            await TestAsync(markup, expectedOrderedItems);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.SignatureHelp)]
        public async Task DeclaringGenericTypeWith2ParametersOn1()
        {
            var markup = @"
class G<S, T> { };

class C
{
    void Goo()
    {
        [|G<$$|]>
    }
}";

            var expectedOrderedItems = new List<SignatureHelpTestItem>();
            expectedOrderedItems.Add(new SignatureHelpTestItem("G<S, T>", string.Empty, string.Empty, currentParameterIndex: 0));

            await TestAsync(markup, expectedOrderedItems);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.SignatureHelp)]
        public async Task DeclaringGenericTypeWith2ParametersOn2()
        {
            var markup = @"
class G<S, T> { };

class C
{
    void Goo()
    {
        [|G<int, $$|]>
    }
}";

            var expectedOrderedItems = new List<SignatureHelpTestItem>();
            expectedOrderedItems.Add(new SignatureHelpTestItem("G<S, T>", string.Empty, string.Empty, currentParameterIndex: 1));

            await TestAsync(markup, expectedOrderedItems);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.SignatureHelp)]
        public async Task DeclaringGenericTypeWith2ParametersOn1XmlDoc()
        {
            var markup = @"
/// <summary>
/// Summary for G
/// </summary>
/// <typeparam name=""S"">TypeParamS. Also see <see cref=""T""/></typeparam>
/// <typeparam name=""T"">TypeParamT</typeparam>
class G<S, T> { };

class C
{
    void Goo()
    {
        [|G<$$|]>
    }
}";

            var expectedOrderedItems = new List<SignatureHelpTestItem>();
            expectedOrderedItems.Add(new SignatureHelpTestItem("G<S, T>",
                    "Summary for G",
                    "TypeParamS. Also see T",
                    currentParameterIndex: 0));

            await TestAsync(markup, expectedOrderedItems);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.SignatureHelp)]
        public async Task DeclaringGenericTypeWith2ParametersOn2XmlDoc()
        {
            var markup = @"
/// <summary>
/// Summary for G
/// </summary>
/// <typeparam name=""S"">TypeParamS</typeparam>
/// <typeparam name=""T"">TypeParamT. Also see <see cref=""S""/></typeparam>
class G<S, T> { };

class C
{
    void Goo()
    {
        [|G<int, $$|]>
    }
}";

            var expectedOrderedItems = new List<SignatureHelpTestItem>();
            expectedOrderedItems.Add(new SignatureHelpTestItem("G<S, T>", "Summary for G", "TypeParamT. Also see S", currentParameterIndex: 1));

            await TestAsync(markup, expectedOrderedItems);
        }

        #endregion

        #region "Constraints on generic types"

        [Fact, Trait(Traits.Feature, Traits.Features.SignatureHelp)]
        public async Task DeclaringGenericTypeWithConstraintsStruct()
        {
            var markup = @"
class G<S> where S : struct
{ };

class C
{
    void Goo()
    {
        [|G<$$|]>
    }
}";

            var expectedOrderedItems = new List<SignatureHelpTestItem>();
            expectedOrderedItems.Add(new SignatureHelpTestItem("G<S> where S : struct", string.Empty, string.Empty, currentParameterIndex: 0));

            await TestAsync(markup, expectedOrderedItems);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.SignatureHelp)]
        public async Task DeclaringGenericTypeWithConstraintsClass()
        {
            var markup = @"
class G<S> where S : class
{ };

class C
{
    void Goo()
    {
        [|G<$$|]>
    }
}";

            var expectedOrderedItems = new List<SignatureHelpTestItem>();
            expectedOrderedItems.Add(new SignatureHelpTestItem("G<S> where S : class", string.Empty, string.Empty, currentParameterIndex: 0));

            await TestAsync(markup, expectedOrderedItems);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.SignatureHelp)]
        public async Task DeclaringGenericTypeWithConstraintsNew()
        {
            var markup = @"
class G<S> where S : new()
{ };

class C
{
    void Goo()
    {
        [|G<$$|]>
    }
}";

            var expectedOrderedItems = new List<SignatureHelpTestItem>();
            expectedOrderedItems.Add(new SignatureHelpTestItem("G<S> where S : new()", string.Empty, string.Empty, currentParameterIndex: 0));

            await TestAsync(markup, expectedOrderedItems);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.SignatureHelp)]
        public async Task DeclaringGenericTypeWithConstraintsBase()
        {
            var markup = @"
class Base { }

class G<S> where S : Base
{ };

class C
{
    void Goo()
    {
        [|G<$$|]>
    }
}";

            var expectedOrderedItems = new List<SignatureHelpTestItem>();
            expectedOrderedItems.Add(new SignatureHelpTestItem("G<S> where S : Base", string.Empty, string.Empty, currentParameterIndex: 0));

            await TestAsync(markup, expectedOrderedItems);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.SignatureHelp)]
        public async Task DeclaringGenericTypeWithConstraintsBaseGenericWithGeneric()
        {
            var markup = @"
class Base<T> { }

class G<S> where S : Base<S>
{ };

class C
{
    void Goo()
    {
        [|G<$$|]>
    }
}";

            var expectedOrderedItems = new List<SignatureHelpTestItem>();
            expectedOrderedItems.Add(new SignatureHelpTestItem("G<S> where S : Base<S>", string.Empty, string.Empty, currentParameterIndex: 0));

            await TestAsync(markup, expectedOrderedItems);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.SignatureHelp)]
        public async Task DeclaringGenericTypeWithConstraintsBaseGenericWithNonGeneric()
        {
            var markup = @"
class Base<T> { }

class G<S> where S : Base<int>
{ };

class C
{
    void Goo()
    {
        [|G<$$|]>
    }
}";

            var expectedOrderedItems = new List<SignatureHelpTestItem>();
            expectedOrderedItems.Add(new SignatureHelpTestItem("G<S> where S : Base<int>", string.Empty, string.Empty, currentParameterIndex: 0));

            await TestAsync(markup, expectedOrderedItems);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.SignatureHelp)]
        public async Task DeclaringGenericTypeWithConstraintsBaseGenericNested()
        {
            var markup = @"
class Base<T> { }

class G<S> where S : Base<Base<int>>
{ };

class C
{
    void Goo()
    {
        [|G<$$|]>
    }
}";

            var expectedOrderedItems = new List<SignatureHelpTestItem>();
            expectedOrderedItems.Add(new SignatureHelpTestItem("G<S> where S : Base<Base<int>>", string.Empty, string.Empty, currentParameterIndex: 0));

            await TestAsync(markup, expectedOrderedItems);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.SignatureHelp)]
        public async Task DeclaringGenericTypeWithConstraintsDeriveFromAnotherGenericParameter()
        {
            var markup = @"
class G<S, T> where S : T
{ };

class C
{
    void Goo()
    {
        [|G<$$|]>
    }
}";

            var expectedOrderedItems = new List<SignatureHelpTestItem>();
            expectedOrderedItems.Add(new SignatureHelpTestItem("G<S, T> where S : T", string.Empty, string.Empty, currentParameterIndex: 0));

            await TestAsync(markup, expectedOrderedItems);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.SignatureHelp)]
        public async Task DeclaringGenericTypeWithConstraintsMixed1()
        {
            var markup = @"
/// <summary>
/// Summary1
/// </summary>
/// <typeparam name=""S"">SummaryS</typeparam>
/// <typeparam name=""T"">SummaryT</typeparam>
class G<S, T>
    where S : Base, new()
    where T : class, S, IGoo, new()
{ };

internal interface IGoo { }

internal class Base { }

class C
{
    void Goo()
    {
        [|G<$$|]>
    }
}";

            var expectedOrderedItems = new List<SignatureHelpTestItem>();
            expectedOrderedItems.Add(new SignatureHelpTestItem("G<S, T> where S : Base, new()", "Summary1", "SummaryS", currentParameterIndex: 0));

            await TestAsync(markup, expectedOrderedItems);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.SignatureHelp)]
        public async Task DeclaringGenericTypeWithConstraintsMixed2()
        {
            var markup = @"
/// <summary>
/// Summary1
/// </summary>
/// <typeparam name=""S"">SummaryS</typeparam>
/// <typeparam name=""T"">SummaryT</typeparam>
class G<S, T>
    where S : Base, new()
    where T : class, S, IGoo, new()
{ };

internal interface IGoo { }

internal class Base { }

class C
{
    void Goo()
    {
        [|G<bar, $$|]>
    }
}";

            var expectedOrderedItems = new List<SignatureHelpTestItem>();
            expectedOrderedItems.Add(new SignatureHelpTestItem("G<S, T> where T : class, S, IGoo, new()", "Summary1", "SummaryT", currentParameterIndex: 1));

            await TestAsync(markup, expectedOrderedItems);
        }

        #endregion

        #region "Generic member invocation"

        [Fact, Trait(Traits.Feature, Traits.Features.SignatureHelp)]
        public async Task InvokingGenericMethodWith1ParameterTerminated()
        {
            var markup = @"
class C
{
    void Goo<T>() { }

    void Bar()
    {
        [|Goo<$$|]>
    }
}";

            var expectedOrderedItems = new List<SignatureHelpTestItem>();
            expectedOrderedItems.Add(new SignatureHelpTestItem("void C.Goo<T>()", string.Empty, string.Empty, currentParameterIndex: 0));

            await TestAsync(markup, expectedOrderedItems);
        }

        [WorkItem(544091, "http://vstfdevdiv:8080/DevDiv2/DevDiv/_workitems/edit/544091")]
        [Fact, Trait(Traits.Feature, Traits.Features.SignatureHelp)]
        public async Task InvokingGenericMethodWith2ParametersOn1()
        {
            var markup = @"
class C
{
    /// <summary>
    /// Method summary
    /// </summary>
    /// <typeparam name=""S"" > type param S. see <see cref=""T""/> </typeparam>
    /// <typeparam name=""T"">type param T. </typeparam>
    /// <param name=""s"">parameter s</param>
    /// <param name=""t"">parameter t</param>
    void Goo<S, T>(S s, T t) { }

    void Bar()
    {
        [|Goo<$$|]>
    }
}";

            var expectedOrderedItems = new List<SignatureHelpTestItem>();
            expectedOrderedItems.Add(new SignatureHelpTestItem("void C.Goo<S, T>(S s, T t)",
                    "Method summary", "type param S. see T", currentParameterIndex: 0));

            await TestAsync(markup, expectedOrderedItems);
        }

        [WorkItem(544091, "http://vstfdevdiv:8080/DevDiv2/DevDiv/_workitems/edit/544091")]
        [Fact, Trait(Traits.Feature, Traits.Features.SignatureHelp)]
        public async Task InvokingGenericMethodWith2ParametersOn2()
        {
            var markup = @"
class C
{
    void Goo<S, T>(S s, T t) { }

    void Bar()
    {
        [|Goo<int, $$|]>
    }
}";

            var expectedOrderedItems = new List<SignatureHelpTestItem>();
            expectedOrderedItems.Add(new SignatureHelpTestItem("void C.Goo<S, T>(S s, T t)", string.Empty, string.Empty, currentParameterIndex: 1));

            await TestAsync(markup, expectedOrderedItems);
        }

        [WorkItem(544091, "http://vstfdevdiv:8080/DevDiv2/DevDiv/_workitems/edit/544091")]
        [Fact, Trait(Traits.Feature, Traits.Features.SignatureHelp)]
        public async Task InvokingGenericMethodWith2ParametersOn1XmlDoc()
        {
            var markup = @"
class C
{
    /// <summary>
    /// SummaryForGoo
    /// </summary>
    /// <typeparam name=""S"">SummaryForS</typeparam>
    /// <typeparam name=""T"">SummaryForT</typeparam>
    void Goo<S, T>(S s, T t) { }

    void Bar()
    {
        [|Goo<$$|]>
    }
}";

            var expectedOrderedItems = new List<SignatureHelpTestItem>();
            expectedOrderedItems.Add(new SignatureHelpTestItem("void C.Goo<S, T>(S s, T t)", "SummaryForGoo", "SummaryForS", currentParameterIndex: 0));

            await TestAsync(markup, expectedOrderedItems);
        }

        [WorkItem(544091, "http://vstfdevdiv:8080/DevDiv2/DevDiv/_workitems/edit/544091")]
        [Fact, Trait(Traits.Feature, Traits.Features.SignatureHelp)]
        public async Task InvokingGenericMethodWith2ParametersOn2XmlDoc()
        {
            var markup = @"
class C
{
    /// <summary>
    /// SummaryForGoo
    /// </summary>
    /// <typeparam name=""S"">SummaryForS</typeparam>
    /// <typeparam name=""T"">SummaryForT</typeparam>
    void Goo<S, T>(S s, T t) { }

    void Bar()
    {
        [|Goo<int, $$|]>
    }
}";

            var expectedOrderedItems = new List<SignatureHelpTestItem>();
            expectedOrderedItems.Add(new SignatureHelpTestItem("void C.Goo<S, T>(S s, T t)", "SummaryForGoo", "SummaryForT", currentParameterIndex: 1));

            await TestAsync(markup, expectedOrderedItems);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.SignatureHelp)]
        public async Task CallingGenericExtensionMethod()
        {
            var markup = @"
class G
{ };

class C
{
    void Bar()
    {
        G g = null;
        g.[|Goo<$$|]>
    }
}

static class GooClass
{
    public static void Goo<T>(this G g) { }
}";

            var expectedOrderedItems = new List<SignatureHelpTestItem>();
            expectedOrderedItems.Add(new SignatureHelpTestItem($"({CSharpFeaturesResources.extension}) void G.Goo<T>()", string.Empty, string.Empty, currentParameterIndex: 0));

            // TODO: Enable the script case when we have support for extension methods in scripts
            await TestAsync(markup, expectedOrderedItems, usePreviousCharAsTrigger: false, sourceCodeKind: Microsoft.CodeAnalysis.SourceCodeKind.Regular);
        }

        #endregion

        #region "Constraints on generic methods"

        [WorkItem(544091, "http://vstfdevdiv:8080/DevDiv2/DevDiv/_workitems/edit/544091")]
        [Fact, Trait(Traits.Feature, Traits.Features.SignatureHelp)]
        public async Task InvokingGenericMethodWithConstraintsMixed1()
        {
            var markup = @"
class Base { }
interface IGoo { }

class C
{
    /// <summary>
    /// GooSummary
    /// </summary>
    /// <typeparam name=""S"">ParamS</typeparam>
    /// <typeparam name=""T"">ParamT</typeparam>
    S Goo<S, T>(S s, T t)
        where S : Base, new()
        where T : class, S, IGoo, new()
    { return null; }

    void Bar()
    {
        [|Goo<$$|]>
    }
}";

            var expectedOrderedItems = new List<SignatureHelpTestItem>();
            expectedOrderedItems.Add(new SignatureHelpTestItem("S C.Goo<S, T>(S s, T t) where S : Base, new()", "GooSummary", "ParamS", currentParameterIndex: 0));

            await TestAsync(markup, expectedOrderedItems);
        }

        [WorkItem(544091, "http://vstfdevdiv:8080/DevDiv2/DevDiv/_workitems/edit/544091")]
        [Fact, Trait(Traits.Feature, Traits.Features.SignatureHelp)]
        public async Task InvokingGenericMethodWithConstraintsMixed2()
        {
            var markup = @"
class Base { }
interface IGoo { }

class C
{
    /// <summary>
    /// GooSummary
    /// </summary>
    /// <typeparam name=""S"">ParamS</typeparam>
    /// <typeparam name=""T"">ParamT</typeparam>
    S Goo<S, T>(S s, T t)
        where S : Base, new()
        where T : class, S, IGoo, new()
    { return null; }

    void Bar()
    {
        [|Goo<Base, $$|]>
    }
}";

            var expectedOrderedItems = new List<SignatureHelpTestItem>();
            expectedOrderedItems.Add(new SignatureHelpTestItem("S C.Goo<S, T>(S s, T t) where T : class, S, IGoo, new()", "GooSummary", "ParamT", currentParameterIndex: 1));

            await TestAsync(markup, expectedOrderedItems);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.SignatureHelp)]
        public async Task TestUnmanagedConstraint()
        {
            var markup = @"

class C
{
    /// <summary>
    /// summary headline
    /// </summary>
    /// <typeparam name=""T"">T documentation</typeparam>
    void M<T>(T arg) where T : unmanaged
    {
    }

    void Bar()
    {
        [|M<$$|]>
    }
}";

            await TestAsync(markup, new List<SignatureHelpTestItem>
            {
                new SignatureHelpTestItem("void C.M<T>(T arg) where T : unmanaged", "summary headline", "T documentation", currentParameterIndex: 0)
            });
        }

        #endregion

        #region "Trigger tests"

        [Fact, Trait(Traits.Feature, Traits.Features.SignatureHelp)]
        public void TestTriggerCharacters()
        {
            char[] expectedCharacters = { ',', '<' };
            char[] unexpectedCharacters = { ' ', '[', '(' };

            VerifyTriggerCharacters(expectedCharacters, unexpectedCharacters);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.SignatureHelp)]
        public async Task FieldUnavailableInOneLinkedFile()
        {
            var markup = @"<Workspace>
    <Project Language=""C#"" CommonReferences=""true"" AssemblyName=""Proj1"" PreprocessorSymbols=""GOO"">
        <Document FilePath=""SourceDocument""><![CDATA[
class C
{
#if GOO
    class D<T>
    {
    }
#endif
    void goo()
    {
        var x = new D<$$
    }
}
]]>
        </Document>
    </Project>
    <Project Language=""C#"" CommonReferences=""true"" AssemblyName=""Proj2"">
        <Document IsLinkFile=""true"" LinkAssemblyName=""Proj1"" LinkFilePath=""SourceDocument""/>
    </Project>
</Workspace>";
            var expectedDescription = new SignatureHelpTestItem($"D<T>\r\n\r\n{string.Format(FeaturesResources._0_1, "Proj1", FeaturesResources.Available)}\r\n{string.Format(FeaturesResources._0_1, "Proj2", FeaturesResources.Not_Available)}\r\n\r\n{FeaturesResources.You_can_use_the_navigation_bar_to_switch_context}", currentParameterIndex: 0);
            await VerifyItemWithReferenceWorkerAsync(markup, new[] { expectedDescription }, false);
        }

        [Fact, Trait(Traits.Feature, Traits.Features.SignatureHelp)]
        public async Task ExcludeFilesWithInactiveRegions()
        {
            var markup = @"<Workspace>
    <Project Language=""C#"" CommonReferences=""true"" AssemblyName=""Proj1"" PreprocessorSymbols=""GOO,BAR"">
        <Document FilePath=""SourceDocument""><![CDATA[
class C
{
#if GOO
    class D<T>
    {
    }
#endif

#if BAR
    void goo()
    {
        var x = new D<$$
    }
#endif
}
]]>
        </Document>
    </Project>
    <Project Language=""C#"" CommonReferences=""true"" AssemblyName=""Proj2"">
        <Document IsLinkFile=""true"" LinkAssemblyName=""Proj1"" LinkFilePath=""SourceDocument"" />
    </Project>
    <Project Language=""C#"" CommonReferences=""true"" AssemblyName=""Proj3"" PreprocessorSymbols=""BAR"">
        <Document IsLinkFile=""true"" LinkAssemblyName=""Proj1"" LinkFilePath=""SourceDocument""/>
    </Project>
</Workspace>";

            var expectedDescription = new SignatureHelpTestItem($"D<T>\r\n\r\n{string.Format(FeaturesResources._0_1, "Proj1", FeaturesResources.Available)}\r\n{string.Format(FeaturesResources._0_1, "Proj3", FeaturesResources.Not_Available)}\r\n\r\n{FeaturesResources.You_can_use_the_navigation_bar_to_switch_context}", currentParameterIndex: 0);
            await VerifyItemWithReferenceWorkerAsync(markup, new[] { expectedDescription }, false);
        }

        #endregion

        #region "EditorBrowsable tests"

        [WorkItem(7336, "DevDiv_Projects/Roslyn")]
        [Fact, Trait(Traits.Feature, Traits.Features.SignatureHelp)]
        public async Task EditorBrowsable_GenericType_BrowsableAlways()
        {
            var markup = @"
class Program
{
    void M()
    {
        var c = new C<$$
    }
}";

            var referencedCode = @"
[System.ComponentModel.EditorBrowsable(System.ComponentModel.EditorBrowsableState.Always)]
public class C<T>
{
}";

            var expectedOrderedItems = new List<SignatureHelpTestItem>();
            expectedOrderedItems.Add(new SignatureHelpTestItem("C<T>", string.Empty, string.Empty, currentParameterIndex: 0));

            await TestSignatureHelpInEditorBrowsableContextsAsync(markup: markup,
                                                       referencedCode: referencedCode,
                                                       expectedOrderedItemsMetadataReference: expectedOrderedItems,
                                                       expectedOrderedItemsSameSolution: expectedOrderedItems,
                                                       sourceLanguage: LanguageNames.CSharp,
                                                       referencedLanguage: LanguageNames.CSharp);
        }

        [WorkItem(7336, "DevDiv_Projects/Roslyn")]
        [Fact, Trait(Traits.Feature, Traits.Features.SignatureHelp)]
        public async Task EditorBrowsable_GenericType_BrowsableNever()
        {
            var markup = @"
class Program
{
    void M()
    {
        var c = new C<$$
    }
}";

            var referencedCode = @"
[System.ComponentModel.EditorBrowsable(System.ComponentModel.EditorBrowsableState.Never)]
public class C<T>
{
}";

            var expectedOrderedItems = new List<SignatureHelpTestItem>();
            expectedOrderedItems.Add(new SignatureHelpTestItem("C<T>", string.Empty, string.Empty, currentParameterIndex: 0));

            await TestSignatureHelpInEditorBrowsableContextsAsync(markup: markup,
                                                       referencedCode: referencedCode,
                                                       expectedOrderedItemsMetadataReference: new List<SignatureHelpTestItem>(),
                                                       expectedOrderedItemsSameSolution: expectedOrderedItems,
                                                       sourceLanguage: LanguageNames.CSharp,
                                                       referencedLanguage: LanguageNames.CSharp);
        }

        [WorkItem(7336, "DevDiv_Projects/Roslyn")]
        [Fact, Trait(Traits.Feature, Traits.Features.SignatureHelp)]
        public async Task EditorBrowsable_GenericType_BrowsableAdvanced()
        {
            var markup = @"
class Program
{
    void M()
    {
        var c = new C<$$
    }
}";

            var referencedCode = @"
[System.ComponentModel.EditorBrowsable(System.ComponentModel.EditorBrowsableState.Advanced)]
public class C<T>
{
}";

            var expectedOrderedItems = new List<SignatureHelpTestItem>();
            expectedOrderedItems.Add(new SignatureHelpTestItem("C<T>", string.Empty, string.Empty, currentParameterIndex: 0));

            await TestSignatureHelpInEditorBrowsableContextsAsync(markup: markup,
                                                       referencedCode: referencedCode,
                                                       expectedOrderedItemsMetadataReference: new List<SignatureHelpTestItem>(),
                                                       expectedOrderedItemsSameSolution: expectedOrderedItems,
                                                       sourceLanguage: LanguageNames.CSharp,
                                                       referencedLanguage: LanguageNames.CSharp,
                                                       hideAdvancedMembers: true);

            await TestSignatureHelpInEditorBrowsableContextsAsync(markup: markup,
                                                       referencedCode: referencedCode,
                                                       expectedOrderedItemsMetadataReference: expectedOrderedItems,
                                                       expectedOrderedItemsSameSolution: expectedOrderedItems,
                                                       sourceLanguage: LanguageNames.CSharp,
                                                       referencedLanguage: LanguageNames.CSharp,
                                                       hideAdvancedMembers: false);
        }
        #endregion

        [WorkItem(1083601, "http://vstfdevdiv:8080/DevDiv2/DevDiv/_workitems/edit/1083601")]
        [Fact, Trait(Traits.Feature, Traits.Features.SignatureHelp)]
        public async Task DeclaringGenericTypeWithBadTypeArgumentList()
        {
            var markup = @"
class G<T> { };

class C
{
    void Goo()
    {
        G{$$>
    }
}";

            var expectedOrderedItems = new List<SignatureHelpTestItem>();
            await TestAsync(markup, expectedOrderedItems);
        }
    }
}