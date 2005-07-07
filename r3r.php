<?php

require_once('includes/l10n.php');

define('PHP_GTK_MAJOR', getenv('PHP_GTK_MAJOR'));
define('ENABLE_DEPRECATED', getenv('ENABLE_DEPRECATED'));

if (!class_exists('gtk'))
{
  if (!dl('php_gtk.' . PHP_SHLIB_SUFFIX))
    trigger_error(ALERT_NO_PHP_GTK, E_USER_ERROR);
}

require_once('version.php');

require_once('includes/globals.php');
require_once('includes/mainAppGui.php');
require_once('includes/settings.php');

startMainGui();

?>
