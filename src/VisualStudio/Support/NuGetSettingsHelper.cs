using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.VisualStudio.ComponentModelHost;
using Microsoft.VisualStudio.Settings;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Settings;
using NuGet.Configuration;
using NuGet.VisualStudio;

namespace XSharp.Support
{
    /// <summary>
    /// Helper methods to read and update the NuGet Package Manager settings from inside the extension.
    /// </summary>
    /// <remarks>
    /// Package sources and other NuGet.Config values are handled through <see cref="ISettings"/>, which
    /// is the instance the IDE itself uses. The Tools/Options toggles live in the VS settings store instead.
    /// All members must be called on the UI thread.
    /// </remarks>
    public static class NuGetSettingsHelper
    {
        private const string NuGetSettingsCollection = "NuGet";

        private static ISettings GetSettings()
        {
            ThreadHelper.ThrowIfNotOnUIThread();

            IComponentModel componentModel = (IComponentModel)Package.GetGlobalService(typeof(SComponentModel));

            return componentModel.GetService<ISettings>();
        }

        /// <summary>
        /// Returns the configured package sources, enabled and disabled alike.
        /// </summary>
        public static IReadOnlyList<PackageSource> GetPackageSources()
        {
            ThreadHelper.ThrowIfNotOnUIThread();

            ISettings settings = GetSettings();
            PackageSourceProvider provider = new PackageSourceProvider(settings);

            return provider.LoadPackageSources().ToList();
        }

        /// <summary>
        /// Adds a package source, or updates its URL when a source with the same name already exists.
        /// </summary>
        public static void AddOrUpdatePackageSource(string name, string source, bool isEnabled)
        {
            ThreadHelper.ThrowIfNotOnUIThread();

            ISettings settings = GetSettings();
            PackageSourceProvider provider = new PackageSourceProvider(settings);

            PackageSource packageSource = new PackageSource(source, name, isEnabled);

            provider.AddPackageSource(packageSource);
        }

        /// <summary>
        /// Removes the package source with the given name. Does nothing when there is no such source.
        /// </summary>
        public static void RemovePackageSource(string name)
        {
            ThreadHelper.ThrowIfNotOnUIThread();

            ISettings settings = GetSettings();
            PackageSourceProvider provider = new PackageSourceProvider(settings);

            List<PackageSource> sources = provider.LoadPackageSources()
                .Where(s => !String.Equals(s.Name, name, StringComparison.OrdinalIgnoreCase))
                .ToList();

            provider.SavePackageSources(sources);
        }

        /// <summary>
        /// Returns the sources as Visual Studio resolves them, as name/url pairs.
        /// </summary>
        public static IReadOnlyList<KeyValuePair<string, string>> GetEffectiveSources(bool includeDisabled)
        {
            ThreadHelper.ThrowIfNotOnUIThread();

            IComponentModel componentModel = (IComponentModel)Package.GetGlobalService(typeof(SComponentModel));
            IVsPackageSourceProvider sourceProvider = componentModel.GetService<IVsPackageSourceProvider>();

            return sourceProvider.GetSources(includeUnOfficial: true, includeDisabled: includeDisabled).ToList();
        }

        /// <summary>
        /// Returns the folder where NuGet stores the downloaded packages.
        /// </summary>
        public static string GetGlobalPackagesFolder()
        {
            ThreadHelper.ThrowIfNotOnUIThread();

            return SettingsUtility.GetGlobalPackagesFolder(GetSettings());
        }

        /// <summary>
        /// Reads a single value from the given NuGet.Config section, or null when it is not present.
        /// </summary>
        public static string GetConfigValue(string sectionName, string key)
        {
            ThreadHelper.ThrowIfNotOnUIThread();

            SettingSection section = GetSettings().GetSection(sectionName);

            if (section == null)
            {
                return null;
            }

            AddItem item = section.Items.OfType<AddItem>()
                .FirstOrDefault(i => String.Equals(i.Key, key, StringComparison.OrdinalIgnoreCase));

            return item?.Value;
        }

        /// <summary>
        /// Writes a single value to the given NuGet.Config section and flushes it to disk.
        /// </summary>
        public static void SetConfigValue(string sectionName, string key, string value)
        {
            ThreadHelper.ThrowIfNotOnUIThread();

            ISettings settings = GetSettings();

            settings.AddOrUpdate(sectionName, new AddItem(key, value));
            settings.SaveToDisk();
        }

        /// <summary>
        /// Reads one of the Tools/Options NuGet Package Manager toggles.
        /// </summary>
        public static bool GetOption(string key, bool defaultValue)
        {
            ThreadHelper.ThrowIfNotOnUIThread();

            ShellSettingsManager manager = new ShellSettingsManager(ServiceProvider.GlobalProvider);
            WritableSettingsStore store = manager.GetWritableSettingsStore(SettingsScope.UserSettings);

            return store.GetBoolean(NuGetSettingsCollection, key, defaultValue);
        }

        /// <summary>
        /// Updates one of the Tools/Options NuGet Package Manager toggles.
        /// </summary>
        public static void SetOption(string key, bool value)
        {
            ThreadHelper.ThrowIfNotOnUIThread();

            ShellSettingsManager manager = new ShellSettingsManager(ServiceProvider.GlobalProvider);
            WritableSettingsStore store = manager.GetWritableSettingsStore(SettingsScope.UserSettings);

            store.CreateCollection(NuGetSettingsCollection);
            store.SetBoolean(NuGetSettingsCollection, key, value);
        }
    }
}
