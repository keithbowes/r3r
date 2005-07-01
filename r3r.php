<?php

require_once('includes/l10n.php');

if (!class_exists('gtk'))
{
  if (getevn('PHP_GTK_MAJOR') >= 2 || (getenv('PHP_GTK_MAJOR') < 2 && !dl('php_gtk.' . PHP_SHLIB_SUFFIX)))
    trigger_error(ALERT_NO_PHP_GTK, E_USER_ERROR);
}

require_once('version.php');

require_once('includes/globals.php');
require_once('includes/mainAppGui.php');
require_once('includes/settings.php');

startMainGui();

?>
