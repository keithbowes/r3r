<?php

if (!extension_loaded('php-gtk'))
  if (!dl('php_gtk.' . PHP_SHLIB_SUFFIX))
    trigger_error('Could not load the PHP-GTK module.', E_USER_ERROR);

require_once('includes/globals.php');
require_once('includes/mainAppGui.php');
require_once('includes/settings.php');

startMainGui();

?>
