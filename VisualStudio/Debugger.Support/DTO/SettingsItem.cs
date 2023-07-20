using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.Text;

namespace Debugger.Support
{
    [DebuggerDisplay("{Name} {Value}")]
    public sealed class SettingsItem
    {
        public int Key { get; set; }
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
                sb.Append(item.Key);
                sb.Append(",");
                sb.Append(item.Value.ToString().Replace("|", PIPE).Replace(",", COMMA));
            }
            return sb.ToString();
        }
        private const string PIPE = "Ϣ";
        private const string COMMA = "Ϧ";

        public static SettingItems Deserialize(string str)
        {
            var result = new SettingItems();
            var items = str.Split('|');
            foreach (var item in items)
            {
                var fields = item.Split(',');
                if (fields.Length >= 3)
                {
                    var sitem = new SettingsItem();
                    sitem.Name = fields[0];
                    int.TryParse(fields[1], out var iValue);
                    sitem.Key = iValue;
                    sitem.Value = fields[2];
                    sitem.Value = sitem.Value.Replace(PIPE, "|").Replace(COMMA, ",");
                    result.Add(sitem);
                }
            }
            return result;
        }
    }
}
