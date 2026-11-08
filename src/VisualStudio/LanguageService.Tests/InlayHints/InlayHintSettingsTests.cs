using System.Windows.Input;
using XSharp.LanguageService.InlayHints;
using Xunit;

namespace XSharp.LanguageService.Tests.InlayHints
{
    public class InlayHintSettingsTests
    {
        [Theory]
        // option, Alt+F1 held, "show all while Alt+F1" option -> shown
        [InlineData(true, false, true, true)]
        [InlineData(true, true, false, true)]
        [InlineData(false, false, true, false)]
        [InlineData(false, true, true, true)]
        [InlineData(false, true, false, false)]
        public void KindIsShown(bool option, bool held, bool allWhileHeld, bool shown)
        {
            Assert.Equal(shown, InlayHintSettings.Shows(option, held, allWhileHeld));
        }

        [Theory]
        // Alt+F1 arrives as Key.System with the real key in SystemKey
        [InlineData(Key.System, Key.F1, true)]
        [InlineData(Key.F1, Key.None, false)]
        [InlineData(Key.System, Key.F2, false)]
        [InlineData(Key.System, Key.LeftAlt, false)]
        public void AltF1StartsTheHold(Key key, Key systemKey, bool starts)
        {
            Assert.Equal(starts, InlayHintKeys.IsAltF1(key, systemKey));
        }

        [Theory]
        // releasing either key ends it
        [InlineData(Key.System, Key.F1, true)]
        [InlineData(Key.F1, Key.None, true)]
        [InlineData(Key.System, Key.LeftAlt, true)]
        [InlineData(Key.LeftAlt, Key.None, true)]
        [InlineData(Key.RightAlt, Key.None, true)]
        [InlineData(Key.A, Key.None, false)]
        public void ReleaseEndsTheHold(Key key, Key systemKey, bool ends)
        {
            Assert.Equal(ends, InlayHintKeys.EndsHold(key, systemKey));
        }
    }
}
