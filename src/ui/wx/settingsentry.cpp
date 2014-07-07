#include "settingsentry.h"
#include "widgetids.h"

SettingsEntry::SettingsEntry(wxWindow * parent, char * setting_name) :
	wxTextCtrl(parent, -1)
{
	char * name, * text;
	int count;
	unsigned char type;
	void * value;

	libr3r_access_settings(&setting_name, &value, &type, &count, SETTINGS_READ);

	switch (type)
	{
		case TYPE_INTEGER:
			text = (char *) malloc(6);
			sprintf(text, "%ld", (long) value);
			break;
		case TYPE_STRING:
			text = (char *) value;
			break;
	}

	name = strdup(setting_name);

	SetValue(wxString(text, wxConvUTF8));
	SetToolTip(wxString(text, wxConvUTF8));

	if (TYPE_INTEGER == type)
	{
		free(text);
	}

	SettingsList * list = GetSettingsList();

	SettingsListElement * elem = new SettingsListElement();
	elem->SetName(name);
	elem->SetType(type);
	elem->SetValue(value);
	free(name);

	list->Append(elem);
	SetClientData(elem);
}
