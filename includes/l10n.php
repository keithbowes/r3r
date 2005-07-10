<?php

/**
  * Localization Code
  * @package Localization
*/

preg_match('/^([^\_]+)\_?([^\.]+)?\.?(.*)$/', getenv('LANG'), $lang_full);
list($match, $lang['name'], $lang['country'], $lang['charset']) = $lang_full;

if (extension_loaded('iconv') && isset($lang['charset']))
{
  iconv_set_encoding('input_encoding', $lang['charset']);
  iconv_set_encoding('output_encoding', $lang['charset']);
  iconv_set_encoding('internal_encoding', $lang['charset']);
}

$lfl = 'l10n/' . $lang['name'] . '.lng';

if (!is_file($lfl))
{
  $lfl = preg_replace("/\$lang\['name'\]/", "\$lang\['name'\]_\$lang['country']", $lfl);
  if (!is_file($lfl))
    $lfl = "l10n/en_US.lng";
}

require_once($lfl);

?>
