<?php

/**
  * @package GUI
*/

require_once('guiConvFuncs.php');
require_once('subscriptions.php');
require_once('callbacks.php');
require_once('networking.php');
require_once('settingsGui.php');

/**
  * Create the main window
*/
function createWindow()
{
  global $box, $window;

  $window->set_title(PROG_NAME);
  $window->connect('destroy', 'killApp');

  $window->add($box);
}

/**
  * Create the main window's menu bar and its menus
*/
function createMenu()
{
  global $box, $menuBar;

  $box->pack_start($menuBar, false, false);

  $fileMenu = &new GtkMenu();
  $toolsMenu = &new GtkMenu();
  $helpMenu = &new GtkMenu();

  $fileMenuItem = createAccelMenu(FILE, 8);

  $findItem = createAccelMenu(FILE_OPEN);
  $findItem->connect('activate', 'findLocalFeed');

  $quitItem = createAccelMenu(FILE_QUIT);
  $quitItem->connect('activate', 'killApp');

  $toolsMenuItem = createAccelMenu(TOOLS, 8);

  $settingsItem = createAccelMenu(TOOLS_SETTINGS);
  $settingsItem->connect('activate', 'showSettingsDialog');

  $helpMenuItem = createAccelMenu(HELP, 8);
  $helpMenuItem->right_justify();

  $infoItem = createAccelMenu(HELP_INFO);
  $infoItem->connect('activate', 'showInfo');

  $menuBar->append($fileMenuItem);
  $menuBar->append($toolsMenuItem);
  $menuBar->append($helpMenuItem);

  $fileMenu->append($findItem);
  $fileMenu->append($quitItem);

  $toolsMenu->append($settingsItem);

  $helpMenu->append($infoItem);

  $fileMenuItem->set_submenu($fileMenu);
  $toolsMenuItem->set_submenu($toolsMenu);
  $helpMenuItem->set_submenu($helpMenu);
}

/**
  * Creates the main window's main area (below the menu bar).
*/
function createAppArea()
{
  global $box, $feedItemView, $feedList, $statusBar, $urlEntry;

  $feedListBox = &new GtkHBox();
  $box->pack_start($feedListBox);

  $feedList->set_column_width(0, 100); 
  $feedList->set_column_width(1, 200); 
  $feedList->set_column_width(2, 67);
  $feedList->column_titles_passive();
  $feedList->connect('select-row', 'feedListRowSelected');
  $feedList->connect('unselect-row', 'feedListRowUnselected');
  $feedList->connect('button-press-event', 'feedListRowClicked');
  $feedList->connect('key-press-event', 'feedListRowPressed');

  $scroll = &new GtkScrolledWindow();
  $scroll->set_usize(500, 150);
  $scroll->set_policy(GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  $scroll->add($feedList);
  $feedListBox->pack_start($scroll);
  $scroll->show();

  $feedItemView->set_usize(150, 60);
  $box->pack_start($feedItemView);

  $box->pack_end($statusBar, false);

  $addrBox = &new GtkHBox(false);
  $box->pack_end($addrBox, false, false, 5);

  $urlEntry = createEdit('http://');
  $addrBox->pack_start($urlEntry);

  $goBtn = &new GtkButton(GO_BTN);
  $goBtn->connect('clicked', 'goBtnClicked', $urlEntry);
  $addrBox->pack_start($goBtn, false, false, 5);
  $urlEntry->connect('key-press-event', 'urlFieldGo', $goBtn);
}

/**
  * Initializes GTK+, creates the widgets for the main window, etc.
*/
function startMainGui()
{
  global $accelGroup, $argc, $argv, $statusBar, $urlEntry, $window;

  getSettings();

  if (getSetting('use-rc-file'))
    gtk::rc_parse(getSetting('rc-file'));

  createWindow();
  createMenu();
  createAppArea();

  $window->add_accel_group($accelGroup);
  $window->show_all();

  if ($argc > 1)
  {
    for ($idx = 1; $idx < $argc; $idx++)
    {
      if (strstr($argv[$idx], 'http://'))
        getRemoteFeed(rawurldecode($argv[$idx]));
      else
        getLocalFeed(rawurldecode($argv[$idx]));

      $urlEntry->set_text($argv[$idx]);
    }

    $statusBar->set_text(STATUS_READ_CL);
  }

  $subs = getSubscriptions();
  if (count($subs) > 1)
  {
    while(list($url) = each($subs))
    {
      if ($url)
      {
        getRemoteFeed($url);
        $urlEntry->set_text($url);
      }
    }

    $statusBar->set_text(STATUS_READ_SUBSCRIBED);
  }

  gtk::main();
}

?>
