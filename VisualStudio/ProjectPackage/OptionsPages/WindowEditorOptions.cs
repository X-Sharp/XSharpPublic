using Community.VisualStudio.Toolkit;
using System;
using System.ComponentModel;
using System.Globalization;
using XSharpModel;
using XSharp.Settings;
namespace XSharp.Project.Options
{
    public class WindowEditorOptions : BaseOptionModel<WindowEditorOptions>
    {
        [Category("Grid Settings")]
        [DisplayName("Show Grid")]
        [Description("Show Grid in the Editor")]
        [DefaultValue(true)]
        public bool ShowGrid { get; set; } 

        [Category("Grid Settings")]
        [DisplayName("Grid distance Horizontal")]
        [Description("The Horizontal distance between the dots in the grid.")]
        [DefaultValue(8)]
        public int GridX { get; set; } 

        [Category("Grid Settings")]
        [DisplayName("Grid distance Vertical")]
        [Description("The vertical distance between the dots in the grid.")]
        [DefaultValue(8)]
        public int GridY { get; set; } 
        [Category("Paste Settings")]
        [DisplayName("Paste Offset Horizontal")]
        [Description("The Horizontal distance between the original control and the pasted control in dots.")]
        [DefaultValue(8)]
        public int PasteOffSetX { get; set; } 

        [Category("Paste Settings")]
        [DisplayName("Paste OffSet Vertical")]
        [Description("The vertical distance between the original control and the pasted control in dots.")]
        [DefaultValue(8)]
        public int PasteOffSetY { get; set; } 

        [Category("Miscellaneous")]
        [DisplayName("Allow Partial Lasso")]
        [Description("Allow Partial Control Lassoing (The Shift Key toggles the behavior).")]
        [DefaultValue(false)]
        public bool PartialLasso { get; set; }

        [Category("RC Files")]
        [DisplayName("Size adjustment X")]
        [Description("Correction multiplier for high DPI monitors horizontally. Adjusts control X-locations and widths written to resource (.rc) files to match the desired sizes.")]
        [DefaultValue(1.0)]
        [TypeConverter(typeof(ScaleTypeConverter))]
        public double SizeAdjustmentX { get; set; } 

        [Category("RC Files")]
        [DisplayName("Size adjustment Y")]
        [Description("Correction multiplier for high DPI monitors vertically. Adjusts control Y-locations and heights written to resource (.rc) files to match the desired sizes.")]
        [DefaultValue(1.0)]
        [TypeConverter(typeof(ScaleTypeConverter))]
        public double SizeAdjustmentY { get; set; } 


        public override void WriteToSettings()
        {
            XCustomEditorSettings.ShowGrid = this.ShowGrid;
            XCustomEditorSettings.GridX = this.GridX;
            XCustomEditorSettings.GridY = this.GridY;
            XCustomEditorSettings.PasteOffSetX = this.PasteOffSetX;
            XCustomEditorSettings.PasteOffSetY = this.PasteOffSetY;
            XCustomEditorSettings.PartialLasso = this.PartialLasso;
            XCustomEditorSettings.SizeAdjustmentX = this.SizeAdjustmentX;
            XCustomEditorSettings.SizeAdjustmentY = this.SizeAdjustmentY;
        }
    }

    public class ScaleTypeConverter : TypeConverter
    {
        public override bool CanConvertFrom(ITypeDescriptorContext context,
                                            Type sourceType)
        {
            return sourceType == typeof(string);
        }

        public override object ConvertFrom(ITypeDescriptorContext context,
            CultureInfo culture, object value)
        {
            if (value is string s)
            {
                if (double.TryParse(s, NumberStyles.AllowDecimalPoint | NumberStyles.AllowThousands, culture, out var result))
                    return result;
                return 1.0;
            }
            return base.ConvertFrom(context, culture, value);
        }
        public override object ConvertTo(ITypeDescriptorContext context,
            CultureInfo culture, object value, Type destinationType)
        {
            if (destinationType == typeof(string))
                return ((double)value).ToString("0.0########", culture);
            return base.ConvertTo(context, culture, value, destinationType);
        }

    }
}
