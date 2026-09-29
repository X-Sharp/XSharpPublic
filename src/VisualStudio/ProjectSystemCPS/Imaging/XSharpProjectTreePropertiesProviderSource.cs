//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System;
using System.Collections.Immutable;
using System.ComponentModel.Composition;
using System.Threading.Tasks.Dataflow;
using Microsoft.VisualStudio.ProjectSystem;
using XSharp.Settings;

namespace XSharp.ProjectSystem.Imaging
{
    /// <summary>
    /// Supplies <see cref="XSharpProjectTreePropertiesProvider"/> to the project tree, and a new instance of it when
    /// the set of .Designer.prg files of the project changes.
    /// </summary>
    /// <remarks>
    /// CPS calls the property providers only for the nodes that changed in a tree update. "Add .designer file" adds a
    /// .Designer.prg below an existing .prg: that adds a child node but leaves the node of the .prg unchanged, so its
    /// form icon only appeared after the project was loaded again. When the list of property providers changes, CPS
    /// calculates the properties of all nodes again (PhysicalProjectTreeProvider.OnProjectTreePropertiesProvidersChanged),
    /// comparing the providers by instance. So this data source publishes a new provider only when a .Designer.prg was
    /// added or removed.
    /// The values carry their own version, not the versions of the source items subscription: the tree input
    /// synchronizes its sources by their data source versions (SyncLinkTo), and with the versions of the source items
    /// it waited for ever -- the tree was never published and VS hung while restoring the open documents (verified in
    /// VS). The first provider is published right away, the tree waits for one from every source.
    /// </remarks>
    [Export(typeof(IProjectTreePropertiesProviderDataSource))]
    [AppliesTo(XSharpCapabilities.XSharpCps)]
    [Order(1000)]
    internal sealed class XSharpProjectTreePropertiesProviderSource : ChainedProjectValueDataSourceBase<IProjectTreePropertiesProvider>, IProjectTreePropertiesProviderDataSource
    {
        private static readonly NamedIdentity VersionKey = new NamedIdentity("XSharpProjectTreePropertiesProvider");

        private readonly UnconfiguredProject project;
        private readonly IActiveConfiguredProjectSubscriptionService subscriptions;
        // Only used by the action block, which handles one update at a time (and before it is linked)
        private long version;
        private ImmutableHashSet<string> designerFiles;

        [ImportingConstructor]
        public XSharpProjectTreePropertiesProviderSource(UnconfiguredProject project, IActiveConfiguredProjectSubscriptionService subscriptions)
            : base(project, synchronousDisposal: false, registerDataSource: false)
        {
            this.project = project;
            this.subscriptions = subscriptions;
        }

        protected override IDisposable LinkExternalInput(ITargetBlock<IProjectVersionedValue<IProjectTreePropertiesProvider>> targetBlock)
        {
            targetBlock.Post(CreateValue());
            var action = DataflowBlockSlim.CreateActionBlock<IProjectVersionedValue<IProjectSubscriptionUpdate>>(update =>
            {
                if (DesignerFilesChanged(update.Value))
                    targetBlock.Post(CreateValue());
            });
            // Like the adapter: the source item rules are dynamic, their names come from SourceItemRuleNamesSource
            return subscriptions.SourceItemsRuleSource.SourceBlock.LinkTo(action, subscriptions.SourceItemRuleNamesSource.SourceBlock,
                new DataflowLinkOptions { PropagateCompletion = true });
        }

        private IProjectVersionedValue<IProjectTreePropertiesProvider> CreateValue()
        {
            version++;
            return new ProjectVersionedValue<IProjectTreePropertiesProvider>(new XSharpProjectTreePropertiesProvider(project),
                ImmutableDictionary<NamedIdentity, IComparable>.Empty.Add(VersionKey, version));
        }

        /// <summary>
        /// True when a .Designer.prg was added or removed since the last update. The first update only records them:
        /// the first provider was applied to all nodes anyway.
        /// </summary>
        private bool DesignerFilesChanged(IProjectSubscriptionUpdate update)
        {
            try
            {
                var builder = ImmutableHashSet.CreateBuilder<string>(StringComparer.OrdinalIgnoreCase);
                foreach (var rule in update.CurrentState.Values)
                {
                    foreach (var item in rule.Items.Keys)
                    {
                        if (item.EndsWith(".designer.prg", StringComparison.OrdinalIgnoreCase))
                            builder.Add(item);
                    }
                }
                var current = builder.ToImmutable();
                var previous = designerFiles;
                designerFiles = current;
                return previous != null && !current.SetEquals(previous);
            }
            catch (Exception e)
            {
                // Keep the provider: a failure must never break the tree
                XSettings.Exception(e);
                return false;
            }
        }
    }
}
