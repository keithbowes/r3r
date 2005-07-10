<?php

/**
  * An array of subscriptions.
  * @access private
*/
$_subscriptions = null;

/**
  * Retrieve subscriptions for the file.
  * @return Array An array of subscriptions.
*/
function getSubscriptions()
{
  global $_subscriptions;

  if ($_subscriptions === null)
  {
    getSubscriptionsOld();
    @reset($_subscriptions);

    $wd = getcwd();
    chdir(SETTINGS_DIR);
    if (file_exists('subscriptions'))
    {
      $fh = fopen('subscriptions', 'r');
      while (!feof($fh))
      {
        $str = trim(fgets($fh));
        $_subscriptions[$str] = 1;
      }
      fclose($fh);
    }
    chdir($wd);
  }

  return $_subscriptions;
}

/**
  * Get subscriptions from the settings file.
  * @return An array of subscriptions.
  * @deprecated
*/
function getSubscriptionsOld()
{
  global $_subscriptions;
  
  $subs = getSetting('subscribed-feeds');
  $sarr = explode(' ', $subs);
  $narr = count($sarr);

  for ($idx = 0; $idx < $narr; $idx++)
    $_subscriptions[$sarr[$idx]] = 1;

  return $_subscriptions;
}

/**
  * Add a subscription to the list.
  * @param String URL of subscription to add.
*/
function addSubscription($url)
{
  global $_subscriptions;
  $_subscriptions[$url] = true;
}

/**
  * Remove a suscription from the list.
  * @param String URL to remove from subscriptions.
*/
function removeSubscription($url)
{
  global $_subscriptions;
  $_subscriptions[$url] = false;
}

/**
  * Save the list of subscriptions to disk.
  */
function saveSubscriptions()
{
  global $_subscriptions;
  reset($_subscriptions);
  
  $nsubs = count($_subscriptions);
  
  $wd = getcwd();
  chdir(SETTINGS_DIR);

  $fh = fopen('subscriptions', 'w');
  while (list($url, $state) = each($_subscriptions))
    if ($url && $state)
      fwrite($fh, $url . "\n");

  fclose($fh);
  chdir($wd);
}

?>
