//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

using System.Collections.Immutable;
using System.ComponentModel.Composition;

using Microsoft.VisualStudio.Imaging;
using Microsoft.VisualStudio.Imaging.Interop;
using Microsoft.VisualStudio.ProjectSystem;
using Microsoft.VisualStudio.ProjectSystem.Tree;

namespace XSharp.VisualStudio.ProjectSystem
{
    /// <summary>
    /// Provides custom icons and tree-node flags for files inside an XSharp project.
    ///
    /// This mirrors the icon mapping that the MPF <c>XSharpFileNode</c> performs via
    /// <c>MapExtensionsToMoniker</c>, ensuring a consistent look across both project
    /// system implementations.
    /// </summary>
    [Export(typeof(IProjectTreePropertiesProvider))]
    [AppliesTo(XSharpProjectCapabilities.XSharp)]
    [Order(100)]
    internal sealed class XSharpProjectTreePropertiesProvider : IProjectTreePropertiesProvider
    {
        // Extension → moniker map built once and reused for every tree change.
        private static readonly ImmutableDictionary<string, ImageMoniker> s_extensionMonikers =
            new Dictionary<string, ImageMoniker>(System.StringComparer.OrdinalIgnoreCase)
            {
                // XSharp source files
                { ".prg",    KnownMonikers.Script          },
                { ".xs",     KnownMonikers.Script          },
                { ".ppo",    KnownMonikers.Script          },
                { ".vh",     KnownMonikers.Script          },
                { ".xh",     KnownMonikers.Script          },
                { ".ch",     KnownMonikers.Script          },

                // VO / XSharp binary editors
                { ".xsfrm",  KnownMonikers.FormInstance      },
                { ".vnfrm",  KnownMonikers.FormInstance      },
                { ".xsmnu",  KnownMonikers.MainMenuControl   },
                { ".vnmnu",  KnownMonikers.MainMenuControl   },
                { ".xsdbs",  KnownMonikers.Database          },
                { ".vndbs",  KnownMonikers.Database          },
                { ".xsfs",   KnownMonikers.ValidationRule    },
                { ".vnfs",   KnownMonikers.ValidationRule    },
                { ".xsrep",  KnownMonikers.Report            },
                { ".vnrep",  KnownMonikers.Report            },
                { ".xssql",  KnownMonikers.DatabaseScript    },
                { ".vnsqs",  KnownMonikers.DatabaseScript    },

                // WPF
                { ".xaml",   KnownMonikers.WPFFile          },
            }
            .ToImmutableDictionary(System.StringComparer.OrdinalIgnoreCase);

        /// <inheritdoc />
        public void CalculatePropertyValues(
            IProjectTreeCustomizationContext context,
            IProjectTreePropertiesUpdate update)
        {
            // Only process file nodes that have a known extension.
            if (!context.Node.Flags.Contains(ProjectTreeFlags.FileOnDisk))
                return;

            string extension = System.IO.Path.GetExtension(context.Node.FilePath);
            if (string.IsNullOrEmpty(extension))
                return;

            if (s_extensionMonikers.TryGetValue(extension, out ImageMoniker moniker))
            {
                update.Icon = moniker.ToProjectSystemType();
                update.ExpandedIcon = moniker.ToProjectSystemType();
            }

            // Mark XSharp source files as proper source files so features like
            // "Go to definition" and code-model scanning are activated.
            if (IsXSharpSource(extension))
            {
                update.Flags = update.Flags
                    .Add(ProjectTreeFlags.SourceFile)
                    .Add(ProjectTreeFlags.FileSystemEntity);
            }
        }

        private static bool IsXSharpSource(string extension) =>
            extension.Equals(".prg",  System.StringComparison.OrdinalIgnoreCase) ||
            extension.Equals(".xs",   System.StringComparison.OrdinalIgnoreCase) ||
            extension.Equals(".ppo",  System.StringComparison.OrdinalIgnoreCase) ||
            extension.Equals(".vh",   System.StringComparison.OrdinalIgnoreCase) ||
            extension.Equals(".xh",   System.StringComparison.OrdinalIgnoreCase) ||
            extension.Equals(".ch",   System.StringComparison.OrdinalIgnoreCase);
    }
}
