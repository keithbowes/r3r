<?php

/**
  * @package Library
*/

/* Global program information */
  
/**
  * The directory where the settings are stored.
*/
define('SETTINGS_DIR', getenv('HOME') . '/.r3r');
/**
  * The settings file.
*/
define('SETTINGS_FILE', 'r3rrc');

/* GUI variables and classes */

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
    * @var int The default status string.
    * @access private
  */
  var $_default_status;
  /**
    * @var int The text previously passed to the widget.
    * @access private
  */
  var $_prev_text;

  /**
    * Set the default status-bar string.
    * @param String The default string.
  */
  function set_default($str)
  {
    $this->_default_status = $str;
  }

  /**
    * Get the default status-bar string.
    * @return The default string.
  */
  function get_default()
  {
    return $this->_default_status;
  }
  
  /**
    * Set the status-bar text.
    * @param String The text to place in the status bar.
  */
  function set_text($text)
  { 
    doEvent();

    $this->_cid = GtkStatusBar::get_context_id($text);

    $this->pop($this->_cid);
    $this->push($this->_cid, $text);

    $this->_prev_text = $text;
  }
  
  /**
    * Get the status-bar text.
    * @param String The text in the status bar.
  */
  function get_text()
  { 
    return $this->_prev_text;
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
  return php_uname('s') . ' ' . php_uname('r');
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
  * URL Field
*/
$urlEntry = null;

/**
  * GtkWindow The main window.
*/
$window = &new GtkWindow();

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
  * Source URL of the current feed
*/
$feedSrc = null;

/**
  * The MIME/content type of the current feed.
*/
$mime_type = null;

?>
