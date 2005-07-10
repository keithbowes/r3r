<?php

/**
  * @package Library
*/

$_settings = null;
$_cat_settings = null;
$_temp_settings = null;

@include_once(SETTINGS_DIR . '/' . OLD_SETTINGS_FILE);

/**
  * Set a setting
  * @param String Name of the setting.
  * @param String Value to assign.
  * @return String The new setting applied.
*/
function setSetting($setting_name, $setting_val)
{
  global $_temp_settings;

  $_temp_settings[$setting_name] = $setting_val;
  return $setting_val;
}

/**
  * Retrieve the value of a setting.
  * @param String Name of the setting to poll.
  * @return String The setting value.
*/
function getSetting($setting_name)
{
  global $_settings;

  if (@$_settings[$setting_name] === null)
    return setInitialSetting($setting_name);
  else
    return $_settings[$setting_name];
}

function commitSettings()
{
  global $_settings, $_temp_settings;
  reset($_temp_settings);

  while (list($setting_name, $setting_val) = each($_temp_settings))
    $_settings[$setting_name] = $setting_val;

  unset($_temp_settings);
}

/**
  * Set the initial value of a new setting.
  * @param The name of the value whose value needs to be set.
*/
function setInitialSetting($setting)
{
  global $lang;
  
  switch ($setting)
  {
    case 'accept-langs':
      if (empty($lang['country']))
        $val = $lang['name'];
      else
        $val= $lang['name'] . '-' . $lang['country'] .',' . $lang['name'] . '; q=0.5';
      break;
    case 'accept-types':
      $val = 'text/x-rss, text/plain; q=0.8, application/rss+xml; q=0.6, application/rdf+xml; q=0.5, application/atom+xml; q=0.3, application/xml; q=0.2, text/xml; q=0.1, */*; q=0.0';
      break;
    case 'browser':
      $val = 'system';
      break;
    case 'display-feed-title-only':
      $val = false;
      break;
    case 'editor':
      $val = 'gvim';
      break;
    case 'enable-mime-guess':
    case 'hide-cached-feeds':
      $val = false;
      break;
    case 'mail-client-cl':
      $val = 'system';
      break;
    case 'proxy-addr':
      $val = '0.0.0.0';
      break;
    case 'proxy-port':
      $val = 8080;
      break;
    case 'rc-file':
      $val = '';
      break;
    case 'show-warnings':
      $val = true;
      break;
    case 'timeout-sec':
      $val = 30;
      break;
    case 'use-custom-accept-langs':
    case 'use-custom-accept-types':
    case 'use-custom-user-agent':
    case 'use-proxy':
    case 'use-rc-file':
      $val = false;
      break;
    case 'user-agent':
      $val = 'R3R/' . VERSION . ' (' .  getOs() . ') PHP/' . PHP_VERSION . ' (' . php_sapi_name() . ')';
      break;
    case 'wrap-desc':
      $val = true;
      break;
    default:
      $val = false;
  }
  setSetting($setting, $val);
}

/**
  * Set the initial values of all settings.
*/
function setInitialSettings()
{
  setInitialSetting('browser');
  setInitialSetting('accept-langs');
  setInitialSetting('accept-types');
  setInitialSetting('display-feed-title-only');
  setInitialSetting('editor');
  setInitialSetting('enable-mime-guess');
  setInitialSetting('hide-cached-feeds');
  setInitialSetting('mail-client-cl');
  setInitialSetting('proxy-addr');
  setInitialSetting('proxy-port');
  setInitialSetting('rc-file');
  setInitialSetting('show-warnings');
  setInitialSetting('timeout-sec');
  setInitialSetting('use-custom-accept-types');
  setInitialSetting('use-custom-accept-langs');
  setInitialSetting('use-custom-user-agent');
  setInitialSetting('use-proxy');
  setInitialSetting('use-rc-file');
  setInitialSetting('user-agent');
  setInitialSetting('wrap-desc');
}

/**
  * Retrieve settings that have been saved to the disk and internalize them.
*/
function getSettings()
{
  global $_settings;
  reset($_settings);

  $wd = getcwd();

  if (!is_dir(SETTINGS_DIR))
    mkdir(SETTINGS_DIR);

  chdir(SETTINGS_DIR);
  if (!file_exists(SETTINGS_FILE))
  {
    $fh = fopen(SETTINGS_FILE, 'w');
    fclose($fh);
    setInitialSettings();
    saveSettings();
  }

  $fh = fopen(SETTINGS_FILE, 'r');
  while (!feof($fh))
  {
    $str = fgets($fh);
    if (strpos('=', $str) > 0)
    {
      $setparts = explode('=', $str);
      $_settings[$setparts[0]] = $setparts[1];
    }
  }
  fclose($fh);

  if (!getSetting('use-custom-accept-langs'))
    setInitialSetting('accept-langs');
  if (!getSetting('use-custom-accept-types'))
    setInitialSetting('accept-types');
  if (!getSetting('use-custom-user-agent'))
    setInitialSetting('user-agent');

  commitSettings();

  chdir($wd);
}

/**
  * Categorize the settings
*/
function catSettings()
{
  global $_cat_settings, $_settings;
  //reset($_cat_settings);
  reset($_settings);
  while (list($setting_name, $setting_val) = each($_settings))
  {
    switch ($setting_name)
    {
      case 'accept-langs':
      case 'accept-types':
      case 'enable-mime-guess':
      case 'proxy-addr':
      case 'proxy port':
      case 'timeout-sec':
        $cat = 'HTTP';
        break;
      case 'show-warnings':
        $cat = 'General';
      case 'display-feed-titles-only':
      case 'hide-cached-feeds':
      case 'wrap-desc':
        $cat = 'GUI';
        break;
      case 'user-agent':
        $cat = 'Info';
        break;
      case 'browser':
      case 'editor':
      case 'mail-client-cl':
        $cat = 'Programs';
        break;
      case 'use-custom-accept-langs':
      case 'use-custom-accept-types':
      case 'use-custom-user-agent':
      case 'use-proxy':
        $cat = 'Volatile';
        break;
      default:
        $cat = 'Deprecated';
    }
    $_cat_settings[$cat][$setting_name] = $setting_val;
  }
}

/**
  * Dump the settings from memory to the settings file.
*/
function saveSettings()
{
  if (!defined('SETTINGS_DIR'))
    return;

  commitSettings();
  $wd = getcwd();

  global $_cat_settings;
  catSettings();

  // $_settings is sporadically and unpredictably not an array, causing the
  // settings file to be overwritten with nothing.  This is a hack to only
  // write settings when $_settings is an array.  If anybody finds the cause
  // of $_settings ceasing to be an array, I'd be glad to fix the real source
  // of error, rather than keeping this work-around.
  if (is_array($_cat_settings))
  {
    chdir(SETTINGS_DIR);
    if (file_exists(SETTINGS_FILE))
      chmod(SETTINGS_FILE, 0600);
    $fh = fopen(SETTINGS_FILE, 'w');

    while (list($cat, $setting) = each($_cat_settings))
    {
      fwrite($fh, "[$cat]\n");
      while (list($setting_name, $setting_val) = each($setting))
        if ($setting_name !== null)
          fwrite($fh, "$setting_name=$setting_val\n");
    }

    fclose($fh);
    chmod(SETTINGS_FILE, 0400);

    chdir($wd);
  }
  else
    system('echo "Settings were corrupted, so they weren\'t saved." >> ' . getenv('HOME') . '/r3r.log');
}

?>
 
