<?php

/**
  * Localization Code
  * @package Localization
*/

$cwd = getcwd();
chdir('l10n');

$lang = getenv('LANG');

if (!file_exists($lang))
  $lang = 'en_US';

$fh = fopen($lang, 'r');

while (!feof($fh))
{
  $str = fgets($fh);
  $eqidx = strpos($str, '=');

  if ($eqidx)
  {
    $const = substr($str, 0, $eqidx);
    $val = trim(substr($str, $eqidx + 1));
    define($const, $val);
  }
}
fclose($fh);

chdir($cwd);

?>
