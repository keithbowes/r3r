<?php

/**
  * @package Library
*/

error_reporting(E_ALL);

$_settings = null;
$_temp_settings = null;

/**
  * Gets the backend to use for storing settings.
  * @return String The backend to use:
  * registry - the Windows registry (preferred for Windows)
  * config - The PEAR Config lib (preferred for non-Windows systems)
  * db - The PEAR db (database) lib; standard issue with PHP
  * internal - The internal buggy/legacy settings implementation.
*/
function getSettingsBackend()
{
  global $_settings;
  static $backend = null;

  if ($backend === null)
  {
    $backend = 'internal';

    if (class_exists('COM') && @$com = &new COM('WScript.Shell'))
    {
      $backend = 'registry';
      $_settings = $com;
      $com->release();
      $com = null;
    }
    else
    {
      @include_once('Config/Container.php');
      if (class_exists('Config_Container'))
      {
        $backend = 'config';
        $_settings = new Config_Container(SETTINGS_DIR . '/' . SETTINGS_FILE, 'xml');
        $_settings->setRoot(SETTINGS_DIR . '/' . SETTINGS_FILE);
      }
      else
      {
        @include_once('DB.php');
        if (class_exists('DB') && ($db = DB::connect(getenv('DB_DATASOURCE'))) && !DB::isError($db))
        {
          $backend = 'db';

          $_settings = $db;
          //$_settings->query('CREATE TABLE settings (accept_langs TEXT, accept_types TEXT, display_feed_titles_only BOOLEAN, hide_cached_feeds BOOLEAN, http_client TEXT, mail_client_cl TEXT, proxy_addr TEXT, proxy_port INT, rc_file TEXT, show_warnings BOOLEAN, user_custom_accept_langs FALSE)');
          //$_settings->query('CREATE TABLE settings (setting_name TEXT, setting_value TEXT)');
          $_settings = sqlite_open(SETTINGS_DIR . '/' . SETTINGS_FILE);
          if (@sqlite_query($_settings, 'CREATE TABLE settings (setting_name TEXT, setting_value TEXT)'))
            setInitialSettings();
          //$db->disconnect();
          //$db = null;
        }
      }
    }
  }

  return $backend;
}
echo getSettingsBackend();

/* Functions for the registry backend */

define('KEY', 'HKEY_CURRENT_USER\Software\Zooplah\R3R\\');

function getSetting_registry($setting_name)
{
  global $_settings;

  return $_settings->regRead(KEY . $setting_name);
}

function commitSetting_registry($setting_name, $setting_val)
{
  global $_settings;

  $_settings->regwrite(KEY . $setting_name, $setting_val);
}

function settingsCleanup_registry()
{
  global $_settings;

  $_settings->release();
  $_settings = null;
}

/* Functions for the config backend */

function commitSetting_config($setting_name, $setting_val)
{
  global $_settings;

  $_settings->writeData($setting_name, $setting_value);
}

/* Functions for db backend */

/**
  * Retrieve the value of a setting.
  * @param String Name of the setting to poll.
  * @return String The setting value.
*/
function getSetting_db($setting_name)
{
  global $_settings, $window;

  $result = sqlite_query($_settings, "SELECT * FROM settings WHERE setting_name='$setting_name'");
  $arr = sqlite_fetch_array($result);
  //if (!isset($arr['setting_val']))
    //$arr['setting_val'] = null;
  echo $result . ':  ' . $arr['setting_name'] . ' => ' . $arr['setting_val'] . "\n";
  return $arr['setting_value'];
}

function commitSetting_db($setting_name, $setting_val)
{
  global $_settings;

  if (getSetting($setting_name) !== null)
    sqlite_query($_settings, "UPDATE settings SET setting_value='$setting_val' WHERE setting_name='$setting_name'");
  else
    sqlite_query($_settings, "INSERT INTO settings VALUES ('$setting_name', '$setting_val')");
}

function settingsCleanup_db()
{
  global $_settings;

  sqlite_close($_settings);
  //$_settings->disconnect();
  $_settings = null;
}

/* Functions for internal backend */

/**
  * Retrieve the value of a setting.
  * @param String Name of the setting to poll.
  * @return String The setting value.
*/
function getSetting_internal($setting_name)
{
  global $_settings;

  if (@$_settings[$setting_name] === null)
    return setInitialSetting($setting_name);
  else
    return $_settings[$setting_name];
}

function commitSetting_internal($setting_name, $setting_val)
{
  global $_settings;

  $_settings[$setting_name] = $setting_val;
}

function saveSettings_internal()
{
  if (!defined('SETTINGS_DIR'))
    return;

  $wd = getcwd();

  global $_settings;
  reset($_settings);

  // $_settings is sporadically and unpredictably not an array, causing the
  // settings file to be overwritten with nothing.  This is a hack to only
  // write settings when $_settings is an array.  If anybody finds the cause
  // of $_settings ceasing to be an array, I'd be glad to fix the real source
  // of error, rather than keeping this work-around.
  if (is_array($_settings))
  {
    chdir(SETTINGS_DIR);
    if (file_exists(SETTINGS_FILE))
      chmod(SETTINGS_FILE, 0600);
    $fh = fopen(SETTINGS_FILE, 'w');

    fwrite($fh, "<?php\n\n");
    while (list($setting_name, $setting_val) = each($_settings))
      if ($setting_name)
        fwrite($fh, "\$_settings['$setting_name'] = '$setting_val';\n");

    fwrite($fh, "\n?>");

    fclose($fh);
    chmod(SETTINGS_FILE, 0400);

    chdir($wd);
  }
  else
    system('echo "Settings were corrupted, so they weren\'t saved." >> r3r.log');
}

/* Code that isn't backend-specific */

/*@include_once(SETTINGS_DIR . '/' . SETTINGS_FILE);
if ($settings)
  $_settings = $settings;*/

/**
  * Set a setting
  * @param String Name of the setting.
  * @param String Value to assign.
*/
function setSetting($setting_name, $setting_val)
{
  global $_temp_settings;

  $_temp_settings[$setting_name] = $setting_val;
}

/**
  * Retrieve the value of a setting.
  * @param String Name of the setting to poll.
  * @return String The setting value.
*/
function getSetting($setting_name)
{
  global $_settings;

  switch (getSettingsBackend())
  {
    case 'db':
      return getSetting_db($setting_name);
    case 'internal':
      return getSetting_internal($setting_name);
  }
}

function commitSettings()
{
  global $_temp_settings;

  if ($_temp_settings != null)
  {
    reset($_temp_settings);

    while (list($setting_name, $setting_val) = each($_temp_settings))
      switch (getSettingsBackend())
      {
        case 'registry':
          commitSetting_registry($setting_name, $setting_val);
          break;
        case 'config':
          commitSetting_config($setting_name, $setting_val);
          break;
        case 'db':
          commitSetting_db($setting_name, $setting_val);
          break;
        case 'internal':
          commitSetting_internal($setting_name, $setting_val);
          break;
      }
  }

  $_temp_settings = null;
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
  //commitSettings();
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
  commitSettings();

  chdir($wd);
}

/**
  * Dump the settings from memory to the settings file.
*/
function saveSettings()
{
  commitSettings();

  switch (getSettingsBackend())
  {
    case 'registry':
      settingsCleanup_registry();
      break;
    case 'config':
      //settingsCleanup_config();
      break;
    case 'db':
      //settingsCleanup_db();
      break;
    case 'internal':
      saveSettings_internal();
  }
}

?>
 
