<?php

/**
  * @package GUI
*/

/**
  * A window for displaying a simple message.
  * @param String The main message to display.
  * @param String The text in the title bar.
  * @param Boolean Whether the text should wrap.
*/
function alert($msg, $title = ALERT_ERROR, $wrap = true)
{
  doEvent();

  $alertWindow = &new GtkWindow(GTK_WINDOW_DIALOG);
  $alertWindow->set_title($title);

  $alertBox = &new GtkVBox();
  $alertWindow->add($alertBox);

  $label = &new GtkLabel();
  $label->set_line_wrap($wrap);
  $label->set_text($msg);

  $alertScrolledWindow = &new GtkScrolledWindow();
  $alertScrolledWindow->set_usize(300, 150);
  $alertScrolledWindow->set_policy(GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  $alertScrolledWindow->add_with_viewport($label);
  $alertBox->pack_start($alertScrolledWindow);

  $okBtnBox = &new GtkHButtonBox();
  $alertBox->pack_end($okBtnBox, false);

  $OKButton = &new GtkButton(ALERT_OK_BTN);
  $OKButton->set_usize(30, 30);
  $OKButton->connect('clicked', 'killWidget', $alertWindow);
  $okBtnBox->pack_end($OKButton);

  $OKButton->set_flags(GTK_CAN_DEFAULT);
  $OKButton->grab_default();

  $alertWindow->show_all();
}

/**
  * Create an entry widget.
  * @param String The initial entry content.
  * @return GtkEntry An entry widget.
*/
function createEdit($text = '')
{
  global $window;

  $entry = &new GtkEntry();
  $entry->set_text($text);
  $entry->connect('key-press-event', 'entryPressed');
  $entry->connect('button-release-event', 'entryClicked');
  return $entry;
}

/**
  * Updates GtkFileSelection::selection_entry.
  * @param GtkButton The widget from which the signal originated.
  * @param Array The GtkFileSelection widget and the selection widget.
*/
function updateSelectionField($source, $data)
{
  list($fileSel, $entry) = $data;
  $entry->set_text($fileSel->get_filename());
}

/**
  * Creates a file-selection dialog.
  * @param GtkWidget The widget from which the signal originated.
  * @param Array Parameters needed for widget creation.
*/
function createFileSelection($source, $data)
{
  list($target, $title, $setting, $entry) = $data;

  $fileSel = &new GtkFileSelection($title);
  $fileSel->set_filename(getSetting($setting));

  $selEntry = $fileSel->selection_entry;
  $selEntry->connect('button-release-event', 'entryClicked');
  $selEntry->connect('key-press-event', 'entryPressed');

  $okBtn = $fileSel->ok_button;
  $okBtn->connect('clicked', 'updateSelectionField', array($fileSel, $target));
  $okBtn->connect('clicked', 'killWidget', $fileSel);

  $cancelBtn = $fileSel->cancel_button;
  $cancelBtn->connect('clicked', 'killWidget', $fileSel);

  $fileSel->show();
}

/**
  * Creates an menu item with an access key.
  * @param String The item's label.
  * @param String Parameters for the type of access.
  * @return GtkMenuItem A menu item with an accelerator.
*/
function createAccelMenu($label = '', $mask = 4)
{
  global $accelGroup;

  $menuItem = &new GtkMenuItem('');
  $menuLabel = $menuItem->child;
  $menuItem->add_accelerator('activate', $accelGroup, $menuLabel->parse_uline($label), $mask, GTK_ACCEL_VISIBLE);
  return $menuItem;
}

/**
  * Retrieve the RSS-3 that are known.
  * @return Array An array of known feeds.
*/
function getKnownFeeds()
{  
  $knownFeeds = array('http://');
  $file_array = file('feeds.txt');
  $file_lines = count($file_array);

  for ($idx = 0; $idx < $file_lines; $idx++)
  {
    $file_array[$idx] = trim($file_array[$idx]);
    list($name, $addr, $subscribed) = explode(',', $file_array[$idx]);
    $url = trim(urldecode($addr));
    $knownFeeds[$idx + 1] = $url;
  }

  return $knownFeeds;
}

?>
