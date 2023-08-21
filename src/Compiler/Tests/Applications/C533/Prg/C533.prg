// 533. compiler crash
// Code is part of a big library 
// hard to find exactly what's causing it, slight changes in the code make the crash disappear 
// and appear again. Of course when the crash is fixed, there will be compiler errors reported

#using  System.Windows.Forms 

CLASS CalendarColumnLongFood INHERIT System.Windows.Forms.DataGridViewColumn

VIRTUAL ASSIGN CellTemplate(value AS System.Windows.Forms.DataGridViewCell)
            
IF ((value != NULL) .and. ! value:GetType():IsAssignableFrom(typeof(CalendarCellLong)))
	THROW System.InvalidCastException{"Must be a CalendarCell"}
ENDIF
SUPER:CellTemplate := value  

END CLASS

INTERNAL CLASS CalendarEditingControlEx INHERIT System.Windows.Forms.DateTimePicker IMPLEMENTS System.Windows.Forms.IDataGridViewEditingControl

VIRTUAL METHOD ApplyCellStyleToEditingControl(dataGridViewCellStyle AS System.Windows.Forms.DataGridViewCellStyle) AS VOID
            //
SELF:Font := dataGridViewCellStyle:Font
SUPER:CalendarForeColor := dataGridViewCellStyle:ForeColor
SUPER:CalendarMonthBackground := dataGridViewCellStyle:BackColor

VIRTUAL METHOD EditingControlWantsInputKey(key AS System.Windows.Forms.Keys, dataGridViewWantsInputKey AS LOGIC) AS LOGIC
RETURN FALSE

VIRTUAL METHOD GetEditingControlFormattedValue(context AS System.Windows.Forms.DataGridViewDataErrorContexts) AS OBJECT
            //
RETURN SELF:EditingControlFormattedValue


VIRTUAL METHOD PrepareEditingControlForEdit(selectAll AS LOGIC) AS VOID

        // Properties
VIRTUAL ACCESS EditingControlDataGridView AS System.Windows.Forms.DataGridView
RETURN SELF:dataGridView
VIRTUAL ASSIGN EditingControlDataGridView(value AS System.Windows.Forms.DataGridView)
            //
SELF:dataGridView := value

VIRTUAL ACCESS EditingControlFormattedValue AS OBJECT
RETURN SUPER:Value:ToShortDateString()
VIRTUAL ASSIGN EditingControlFormattedValue(value AS OBJECT)
            //
IF ((System.Type.Equals(value,STRING)))
                //
	SUPER:Value := System.DateTime.Parse((STRING)value )
ENDIF

VIRTUAL ACCESS EditingControlRowIndex AS LONG
            //
RETURN SELF:rowIndex
VIRTUAL ASSIGN EditingControlRowIndex(value AS LONG)
            //
SELF:rowIndex := value

VIRTUAL ACCESS EditingControlValueChanged AS LOGIC
           //
RETURN SELF:valueChanged
VIRTUAL ASSIGN EditingControlValueChanged(value AS LOGIC)
            //
SELF:valueChanged := value

VIRTUAL ACCESS EditingPanelCursor AS System.Windows.Forms.Cursor
            //
RETURN SUPER:Cursor

VIRTUAL ACCESS RepositionEditingControlOnValueChange AS LOGIC
            //
RETURN FALSE

END CLASS


