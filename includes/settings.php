<?php

/**
  * @package Library
*/

@include_once(SETTINGS_DIR . '/' . SETTINGS_FILE);

/**
  * Set a setting
  * @param String Name of the setting.
  * @param String Value to assign.
*/
function setSetting($setting_name, $setting_val)
{
  global $settings;

  $settings[$setting_name] = $setting_val;
}

/**
  * Retrieve the value of a setting.
  * @param String Name of the setting to poll.
  * @return String The setting value.
*/
function getSetting($setting_name)
{
  global $settings;

  if (@$settings[$setting_name] === null)
    return setInitialSetting($setting_name);
  else
    return $settings[$setting_name];
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
      $val = str_replace('_', '-', strtolower($lang));
      break;
    case 'accept-types':
      $val = 'text/x-rss, text/plain; q=0.8, application/rss+xml; q=0.6, application/rdf+xml; q=0.5, application/atom+xml; q=0.3, */*; q=0.1';
      break;
    case 'display-feed-title-only':
      $val = false;
      break;
    case 'hide-cached-feeds':
      $val = false;
      break;
    case 'http-client':
      $val = 'system';
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
      $val = false;
      break;
    case 'subscribed-feeds':
      $val = '';
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
    case 'version':
      $val = VERSION;
      break;
    case 'wrap-desc':
      $val = false;
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
  setInitialSetting('accept-langs');
  setInitialSetting('accept-types');
  setInitialSetting('display-feed-title-only');
  setInitialSetting('hide-cached-feeds');
  setInitialSetting('http-client');
  setInitialSetting('mail-client-cl');
  setInitialSetting('proxy-addr');
  setInitialSetting('proxy-port');
  setInitialSetting('rc-file');
  setInitialSetting('show-warnings');
  setInitialSetting('subscribed-feeds');
  setInitialSetting('timeout-sec');
  setInitialSetting('use-custom-accept-types');
  setInitialSetting('use-custom-accept-langs');
  setInitialSetting('use-custom-user-agent');
  setInitialSetting('use-proxy');
  setInitialSetting('use-rc-file');
  setInitialSetting('user-agent');
  setInitialSetting('version');
  setInitialSetting('wrap-desc');
}

/**
  * Retrieve settings that have been saved to the disk and internalize them.
*/
function getSettings()
{
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

  if (!getSetting('use-custom-accept-langs'))
    setInitialSetting('accept-langs');
  if (!getSetting('use-custom-accept-types'))
    setInitialSetting('accept-types');
  if (!getSetting('use-custom-user-agent'))
    setInitialSetting('user-agent');
  setInitialSetting('version');

  chdir($wd);
}

/**
  * Dump the settings from memory to the settings file.
*/
function saveSettings()
{ 
  if (!defined('SETTINGS_DIR'))
    return;

  $wd = getcwd();

  global $settings;
  reset($settings);

  chdir(SETTINGS_DIR);
  if (file_exists(SETTINGS_FILE))
    chmod(SETTINGS_FILE, 0600);
  $fh = fopen(SETTINGS_FILE, 'w');

  fwrite($fh, "<?php\n\n");
  while (list($setting_name, $setting_val) = each($settings))
    if ($setting_name)
      fwrite($fh, "\$settings['$setting_name'] = '$setting_val';\n");

  fwrite($fh, "\n?>");

  fclose($fh);
  chmod(SETTINGS_FILE, 0400);

  chdir($wd);
}

?>
