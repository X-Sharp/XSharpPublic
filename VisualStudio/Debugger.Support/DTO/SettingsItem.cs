using System.Collections.Generic;
using System.ComponentModel;
using System.Text;

namespace Debugger.Support
{
    public sealed class SettingsItem
    {
        public string Name { get; set; }
        public string Value { get; set; }
    }

    public sealed class SettingItems
    {
        private List<SettingsItem> items;
        public SettingItems()
        {
            items = new List<SettingsItem>();
        }
        public SettingsItem[] Items
        {
            get
            {
                return items.ToArray();
            }
            set
            {
                items = new List<SettingsItem>();
                items.AddRange(value);
            }
        }
        public void Add(SettingsItem item)
        {
            items.Add(item);
        }
        public string Serialize()
        {
            var sb = new StringBuilder();
            bool first = true;
            foreach (var item in this.Items)
            {
                if (first)
                    first = false;
                else
                    sb.Append("|");
                sb.Append(item.Name);
                sb.Append(",");
                sb.Append(item.Value.ToString().Replace("|", "ßßß").Replace(",", "ΓΓΓ"));
            }
            return sb.ToString();
        }
        public static SettingItems Deserialize(string str)
        {
            var result = new SettingItems();
            var items = str.Split('|');
            foreach (var item in items)
            {
                var fields = item.Split(',');
                if (fields.Length >= 2)
                {
                    var sitem = new SettingsItem();
                    sitem.Name = fields[0];
                    sitem.Value = fields[1];
                    sitem.Value = sitem.Value.Replace("ßßß", "|").Replace("ΓΓΓ", ",");
                    result.Add(sitem);
                }
            }
            return result;
        }
    }
}
