<?php

/**
  * Localization Code
  * @package Localization
*/

$lang = getenv('LANG');
$lfl = "l10n/$lang.lng";

if (!is_file($lfl))
  $lfl = "l10n/en_US.lng";

require_once($lfl);

?>
