<?php

/**
  * @package GUI
*/

/* Generic widget callbacks */

/**
  * Kills the app: quits GTK, cleans up, etc.
  *
*/
function killApp()
{
  gtk::main_quit();
  saveSettings();
}

/**
  * Kills the specified widget.
  * @param GtkWidget Widget on which the signal fired.
  * @param GtkWidget The widget to destroy.
*/
function killWidget($source, $target)
{
  $target->destroy();
}

/**
  * Open up a web browser and go to a URL.
  * @param String The URL to which to go.
*/
function gotoLink($link)
{
  if ($link != FEED_LINE_EMPTY && $link)
  {
    if (getSetting('http-client') == 'system')
    {
      if (class_exists('COM'))
      {
        if (@$brows = &new COM('Mozilla.Browser'))
        {
          $brows->Navigate($link);
          $brows->Visible = True;
        }
        else if (@$brows = &new COM('InternetExplorer.Application'))
        {
          $brows->Navigate($link);
          $brows->Visible = True;
        }
      }
      else
      {
        $brows = `links -dump $link`;
        if ($brows)
          alert($brows, ALERT_ITEM_DETAILS, false);
      }
    }
    else
      system("\"" . getSetting('http-client') . "\" " . $link . ' > /dev/null &');
  }
}

/**
  * Send e-mail using a mail client
  * @param String The address to which the message will be sent
  * @param String The default subject of the message
*/
function sendMail($addr, $subj)
{
  $mcc = getSetting('mail-client-cl');
  $mcc = str_replace('%a', rawurlencode($addr), $mcc);
  $mcc = str_replace('%s', rawurlencode($subj), $mcc);

  if ($mcc == 'system')
    gotoLink('mailto:' . rawurlencode($addr) . '?subject=' . rawurlencode($subj));
  else
    system($mcc);
}

/**
  * Callback that's called when the menu created by createEditMenu is activated.
  * @param GtkEntry The widget that was activated.
  * @param Array Extra data: an array containing the widget to or from which the editing will take place and a string ("copy", "cut" or "paste") specifying the edit operation
*/
function entryMenuClicked($widget, $data)
{
  list($target, $operation) = $data;

  $target->select_region(0, -1);

  switch ($operation)
  {
    case 'cut':
      $target->cut_clipboard();
      break;
    case 'copy':
      $target->copy_clipboard();
      break;
    case 'paste':
      $target->paste_clipboard();
      break;
  }
}

/**
  Create an edit menu.
  * @param GtkEntry Widget to or from which an editing operation happens.
  * @param GdkEvent A GDK event used for the fifth param of GtkMenu::popup.
*/
function createEditMenu($widget, $event)
{
  $editMenu = &new GtkMenu();

  $cutItem = &new GtkMenuItem(CMENU_CUT);
  $cutItem->connect('activate', 'entryMenuClicked', array($widget, 'cut'));

  $copyItem = &new GtkMenuItem(CMENU_COPY);
  $copyItem->connect('activate', 'entryMenuClicked', array($widget, 'copy'));

  $pasteItem = &new GtkMenuItem(CMENU_PASTE);
  $pasteItem->connect('activate', 'entryMenuClicked', array($widget, 'paste'));

  $editMenu->append($cutItem);
  $editMenu->append($copyItem);
  $editMenu->append($pasteItem);
  $editMenu->show_all();

  $editMenu->popup(null, null, null, 0, $event['time']);
}

/**
  * An entry has been clicked.
  * @param GtkEntry The target widget for the editing operation.
  * @param GdkEvent A GDK event.  See createEditMenu.
*/
function entryClicked($widget, $event)
{ 
  $event = (array) $event;

  if ($event['button'] == 3)
    createEditMenu($widget, $event);
}

/**
  * An entry has been activated via the keyboard.
  * @param GtkEntry The target widget for the editing operation.
  * @param GdkEvent A GDK event.  See createEditMenu.
*/
function entryPressed($widget, $event)
{
  $event = (array) $event;

  if ($event['keyval'] == GDK_KEY_F10 && $event['state'] == GDK_SHIFT_MASK)
    createEditMenu($widget, $event);
}

/**
  * Get a local feed that has been opened.
  * @param GtkEntry The widget from which the signal originated.
  * @param GtkFileDialog The GtkFileDialog
*/
function getFeed($widget, $data)
{
  getLocalFeed($data->get_filename());
}

/**
  * The feed list menu has been activated.
  * @param GtkCList The widget from which the signal originated.
  * @param Array The action, and the user data (link URL and whether subscribed).
*/
function feedListMenuActivated($widget, $data)
{
  list($action, $user_data) = $data;

  switch ($action)
  {
    case 'subs':
      list($url, $will_subscribe) = $user_data;
      if ($url)
      {
        $subscribed_feeds = getSetting('subscribed-feeds');
        if ($will_subscribe)
          setSetting('subscribed-feeds', $subscribed_feeds .= $url . ' ');
        else
          setSetting('subscribed-feeds', ltrim(str_replace($url, '', $subscribed_feeds)));
      }
      break;
    case 'creat':
      list($addr) = $user_data;
      sendMail($addr, 'Comments about your feed');
      break;
    case 'err':
      list($addr) = $user_data;
      sendMail($addr, 'Feed errors');
      break;
    case 'lic':
      gotoLink($user_data);
      break;
    case 'md':
      list($feed) = $user_data;
      $mds = '';

      if ($feed['last-modified'])
        $mds .= IMD_LAST_MODIFIED . $feed['last-modified'] . "\n";
      if ($feed['language'])
        $mds .= IMD_LANG . $feed['language'] . "\n";
      if ($feed['rights'])
        $mds .= IMD_COPY . $feed['rights'] . "\n";
      if ($feed['guid'])
        $mds .= IMD_GUID . $feed['guid'] . "\n";
      if ($feed['uri'])
        $mds .= IMD_ADDR . $feed['uri'] . "\n";
      if ($feed['generator'])
        $mds .= IMD_GUID . $feed['generator'] . "\n";

      alert($mds, ALERT_MDATA, false);
      break;
  }
}

/**
  * Create context menu for feed list items.
  * @param GtkCList Widget from which signal originated.
  * @param GdkEvent The event.
*/
function createFeedListMenu($widget, $event)
{
  if ($widget->selection == null)
    return;

  $menu = &new GtkMenu();
  list($feed, $url, $top_feed) = $widget->get_data($widget->selection[0]);

  if ($top_feed)
  {
    if (!strstr(getSetting('subscribed-feeds'), $url))
    {
      $subsItem = createAccelMenu(ITEM_SUBSCRIBE);
      $is_subscribed = true;
    }
    else
    {
      $subsItem = createAccelMenu(ITEM_UNSUBSCRIBE);
      $is_subscribed = false;
    }
    $subsItem->connect('activate', 'feedListMenuActivated', array('subs', array($url, $is_subscribed)));
    $menu->append($subsItem);
  }

  if (isset($feed['creator']))
  {
    preg_match('/^(\S+)/', $feed['creator'], $matches);
    list($match, $addr) = $matches;

    $creatItem = createAccelMenu(ITEM_MAIL);
    $creatItem->connect('activate', 'feedListMenuActivated', array('creat', array($addr)));
    $menu->append($creatItem);
  }

  if (isset($feed['errorsto']))
  {
    preg_match('/^(\S+)/', $feed['errorsto'], $matches);
    list($match, $addr) = $matches;

    $etItem = createAccelMenu(ITEM_ERRORS);
    $etItem->connect('activate', 'feedListMenuActivated', array('err', array($addr)));
    $menu->append($etItem);
  }

  if (isset($feed['license']))
  {
    $licItem = createAccelMenu(ITEM_LIC);
    $licItem->connect('activate', 'feedListMenuActivated', array('lic', array($feed['license'])));
    $menu->append($etItem);
  }

  $mdItem = createAccelMenu(ITEM_METADATA);
  $mdItem->connect('activate', 'feedListMenuActivated', array('md', array($feed)));
  $menu->append($mdItem);

  if ($menu->children())
  {
    $menu->show_all();
    $menu->popup(null, null, null, 0, $event['time']);
  }
}

/**
  * Displays dialog for local feed retrieval.
*/
function findLocalFeed()
{
  $filesel = &new GtkFileSelection(FS_OPEN);

  $selEntry = $filesel->selection_entry;
  $selEntry->connect('button-release-event', 'entryClicked');
  $selEntry->connect('key-press-event', 'entryPressed');

  $okbtn = $filesel->ok_button;
  $okbtn->connect('clicked', 'getFeed', $filesel);
  $okbtn->connect('clicked', 'killWidget', $filesel);

  $cnlbtn = $filesel->cancel_button;
  $cnlbtn->connect('clicked', 'killWidget', $filesel);

  $filesel->show();
}

/**
  * Feed list row has been selected.  Update other widgets to reflect that.
  * @param GtkListItem Widget from the which the signal originated.
  * @param int Row that was selected.
*/
function feedListRowSelected($widget, $row)
{
  global $feedItemView, $statusBar;
  list($feed) = $widget->get_data($row);

  $text = &new GtkText();
  $text->insert_text($feed['description'], 0);
  $text->set_word_wrap(getSetting('wrap-desc'));

  $scrolledBox = &new GtkVBox();
  $scrolled = &new GtkScrolledWindow();

  // TODO: Try to accompish RSS 3.0 linking with GtkText
  /*$str = $feed['description'];

  while (preg_match('/([^\<]+)(\<URL:([^\>]+)\>)?/', $str, $matches))
  {
    list($match, $label, $link, $link_url) = $matches;
    $str = str_replace("$label$link", '', $str);
    $lw = getSetting('wrap-desc');

    $txt = &new GtkText();
    $txt->insert_text($feed['description'], 100);
    $txt->set_line_wrap($lw);
    $scrolled->add($txt);

    if ($link_url)
    {
      $linkEvBox = &new GtkEventBox();
      $linkEvBox->connect('button-press-event', 'descLinkClicked', $link_url);

      $linkLabel = &new GtkLabel($link_url);
      $linkLabel->set_pattern(str_repeat('_', strlen($link_url)));
      $linkEvBox->add($linkLabel);

      $scrolled->add($linkEvBox);
    }
  }*/

  $scrolled->set_usize(150, 45);
  $scrolled->set_policy(GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  $scrolled->add($text);
  $scrolledBox->pack_start($scrolled);

  $feedItemView->set_label($feed['title']);
  if ($feedItemView->child)
    $feedItemView->remove($feedItemView->child);
  $feedItemView->add($scrolledBox);
  $feedItemView->show_all();

  $statusBar->set_text($feed['link']);
}

/**
  * Feed list row has been unselected.  Reverse changes made by feedListRowSelected().
*/
function feedListRowUnselected()
{
  global $feedItemView, $statusBar;
  $feedItemView->set_label(DESC_NO_ITEM);

  $feedItemView->remove($feedItemView->child);

  $statusBar->remove_top();
}

/**
  * Feed-list row has been clicked.
  * @param GtkListItem Source list item.
  * @param GdkEvent GDK event.
*/
function feedListRowClicked($widget, $event)
{
  list($feed) = $widget->get_data($widget->selection[0]);
  $event = (array) $event;

  if ($event['button'] == 1 && $event['type'] == GDK_2BUTTON_PRESS)
    gotoLink($feed['link']);
  else if ($event['button'] == 3)
    createFeedListMenu($widget, $event);
}

/**
  * Feed-list row has been activated via keyboard.
  * @param GtkListItem Source list item.
  * @param GdkEvent GDK event.
*/
function feedListRowPressed($widget, $event)
{
  $event = (array) $event;

  if ($event['keyval'] == GDK_KEY_Return)
  {
    list($feed) = $widget->get_data($widget->selection[0]);
    gotoLink($feed['link']);
  }
  if ($event['keyval'] == GDK_KEY_F10 && $event['state'] == GDK_SHIFT_MASK)
    createFeedListMenu($widget, $event);
}

/**
  * Show info about the program.
*/
function showInfo()
{
  alert(INFO_NAME . ' (' . PROG_NAME . ")\n\n" . INFO_VERSION . ': ' . VERSION . "\n" . INFO_OS . ': ' . getOs() . "\n" . INFO_PHP_VERSION . ': ' . PHP_VERSION . "\n" . INFO_PHP_API . ': ' . php_sapi_name() . "\n" . INFO_SETTINGS_DIR . ': ' . SETTINGS_DIR, INFO_TITLE);
}

/**
  * The "Go" Button has been clicked.
  * @param GtkButton The "Go" button.
  * @param GtkEntry The URL field.
*/
function goBtnClicked($widget, $data)
{
  getRemoteFeed($data->get_text());
}

/**
  * URL field has been activated via keyboard.
  * @param GtkEntry The URL field.
  * @param GdkEvent GDK event.
  * @param GtkButton The "Go" button. 
*/
function urlFieldGo($widget, $event, $data)
{
  $event = (array) $event;

  if ($event['keyval'] == GDK_KEY_Return)
    goBtnClicked($data, $widget);
}

/* Settings dialog callbacks */

/**
  * The "Cancel" button in the Settings dialog has been activated.
  * @param GtkButton The widget from which the callback originated.
  * @param Array Data sent to the function, providing information about settings to be canceled.
*/
function settingsDlgDel($widget, $data)
{
  list($save_settings, $settings) = (array) $data;

  if ($save_settings)
    cancelSettingsChange($widget, $settings);
}

/**
  * Revert settings to their previous state.
  * @param GtkWidget GTK widget
  * @param Array Old settings.
*/
function cancelSettingsChange($widget, $data)
{
  global $settings;

  $settings = $data;
}

/**
  * Convenience function for checkboxed settings.
  * @param GtkCheckButton Widget from which the callback originated.
  * @param Array An array of widgets to disable/enable between toggled states, and which setting to change.
*/
function checkToggled($source, $data)
{
  list($targets, $setting) = $data;

  $sensitive = !getSetting($setting);
  foreach ($targets as $target)
    $target->set_sensitive($sensitive);
  setSetting($setting, $sensitive);
}

/**
  * An entry widget, used for changing a setting, has been changed.
  * @param GtkEntry The GtkEntry widget that was changed.
  * @param String The setting to change to the content of the GtkEntry widget.
*/
function settingsEntryChanged($widget, $data)
{
  setSetting($data, $widget->get_text());  
}

/* Item Description Callbacks*/

/**
  * A link in the description area has been clicked
  * @param GtkEventBox The clicked widget
  * @param GdkEvent The event passed
  * @param String The link to visit
*/
function descLinkClicked($widget, $event, $data)
{
  global $feeds;

  gotoLink(relToAbs($feeds[0]['link'], $data));
}

?>
