<?php

/**
  * @package Library
*/

require_once('l10n.php');

/* Global program information */
  
/**
  * The version of the program.
*/
define('VERSION', '1.1.4');

if ($home = getenv('HOME'))
{
  /**
    * The directory where the settings are stored.
  */
  define('SETTINGS_DIR', "$home/.r3r");
  /**
    * The settings file.
  */
  define('SETTINGS_FILE', 'r3rrc');
}

/* GUI vriables and classes */

/**
  * An extended status-bar class with less wordy methods.
*/
class ExtendedGtkStatusBar extends GtkStatusBar
{
  /**
    * @var int The context id of the previous statusbar entry.
    * @access private
  */
  var $_cid;

  /**
    * Set the status-bar text.
    * @param String The text to place in the status bar.
  */
  function set_text($text)
  { 
    doEvent();
    $this->_cid = $this->get_context_id($text);
    $this->push($this->_cid, $text);
  }

  /**
    * Remove the topmost entry of the status-bar.
  */
  function remove_top()
  {
    $this->pop($this->_cid);
  }
}

/**
  * Execute all pending events
*/
function doEvent()
{
  while (gtk::events_pending())
    gtk::main_iteration();
}

/**
  * Get the operating system on which the program is running.
  * @return String The operating system name and version
*/
function getOs()
{
  preg_match('/(.+)\s\S+\s(\d\.\S+)/', php_uname(), $matches);
  list($match, $os_name, $os_version) = $matches;
  return $os_name . ' ' . $os_version;
}

/**
  * GtkAccelGroup The accel group used by the program.
*/
$accelGroup = &new GtkAccelGroup();
/**
  * GtkVBox The main program box.
*/
$box = &new GtkVBox();
/**
  * GtkFrame A container for item descriptions.
*/
$feedItemView = &new GtkFrame(DESC_NO_ITEM);
/**
  * GtkCList A list in which feed items are displayed.
*/
$feedList = &new GtkCList(4, array(LV_FEED_NAME, LV_ITEM_TITLE, LV_SUBJECT, LV_CREATED));
/**
  * GtkMenuBar The main window's menu bar.
*/
$menuBar = &new GtkMenuBar();
/**
  * ExtendedGtkStatusBar The main window's status bar.
*/
$statusBar = &new ExtendedGtkStatusBar();
/**
  * GtkWindow The main window.
*/
$window = &new GtkWindow();

/* Settings */

/**
  * @global Array Internel representation of your setting.
*/
$settings = array();

/* Feeds */

/**
  * An array of items of the current feed.
*/
$feeds = null;

/**
  * The index of the current feed item.
*/
$itemIndex = 0;

/**
  * The MIME/content type of the current feed.
*/
$mime_type = null;

?>
