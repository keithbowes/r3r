<?php

  if (!extension_loaded('php-gtk'))
    if (!dl('php_gtk.' . PHP_SHLIB_SUFFIX))
      echo "PHP-GTK is not installed";
?>
