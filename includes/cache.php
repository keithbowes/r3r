<?php

/**
  * @package Networking
*/

/* Caching, duh */

/**
  * Handle of the cached data
  * @access private
*/
$_cache_cache = null;
/**
  * Cache directory
  * @access private
*/
$_cache_dir = '';
/**
  * Handle of the cached feed
  * @access private
*/
$_cache_handle = null;
/**
  * Identification of the current cache
  * @access private
*/
$_cache_id = '';
/**
  * The data regarding the cache.
  * @access private
*/
$_cache_data = null;
/**
  * Number indicating the feed's cached status
  * @access private
*/
$_cached_status = 0;

/**
  * Opens the cache
  * @param String The URL of the cache
*/
function openCache($url)
{
  global $_cache_cache, $_cache_dir, $_cache_handle, $_cache_id, $_cached_status;

  if ($_cache_handle)
    closeCache();

  $_cache_id = rawurlencode($url);
  $_cache_dir = SETTINGS_DIR . '/cache';

  $_cache_data = 'data';
  $_cache_feed = 'feed';

  $cwd = getcwd();

  if (!is_dir($_cache_dir))
    mkdir($_cache_dir);

  chdir($_cache_dir);

  if (!is_dir($_cache_id))
    mkdir($_cache_id);

  chdir($_cache_id);

  if (file_exists($_cache_data))
    $_cached_status = 1;
  else
  {
    $_cached_status = 0;
    $fh = fopen($_cache_data, 'w');
    fclose($fh);
  }

  if (!file_exists($_cache_feed))
  {
    $fh = fopen($_cache_feed, 'w');
    fclose($fh);
  }

  unset($fh);

  $_cache_cache = fopen($_cache_data, 'ar+');
  $_cache_handle = fopen($_cache_feed, 'ar+');

  chdir($cwd);
}

/**
  * Close the cache
*/
function closeCache()
{
  global $_cache_cache, $_cache_data, $_cache_handle, $_cache_id, $_cached_status;

  @fclose($_cache_cache);
  @fclose($_cache_handle);

  $_cache_data = null;
  $_cache_id = '';
  $_cached_status = 0;
}

/**
  * Test whether the feed is cached.
  * @return Boolean Whether the feed is cached
*/
function getCachedStatus()
{
  global $_cache_data, $_cached_status;

  if (!$_cache_data)
    $_cache_data = readCacheData();

  if (!$_cache_data['expires'])
    $_cached_status = 0;
  else if ($_cache_data['expires'] > time())
    $_cached_status = 2;

  return $_cached_status;
}

/**
  * Write a line of the feed's content to the cache
  * @param String Line of text to write
*/
function cacheWrite($str)
{
  global $_cache_handle;

  if ($_cache_handle)
    fwrite($_cache_handle, str_replace("\r", '', $str));
}

/**
  * Record the cacheable data
*/
function writeCacheData()
{
  global $_cache_cache, $_cache_data, $_cached_status, $feeds;

  if (!$_cache_data)
    $_cache_data = readCacheData();

  if (!$_cache_data || $_cache_data['mod-val'])
    return;

  $now = time();

  if ($data = $feeds[-1]['cache-contol'])
  {
    if ($data == 'must-revalidate' || $data == 'no-cache' || $data == 'no-store')
      fwrite($_cache_cache, 0);
    else
    {
      preg_match('/max-age=(\d+)/', $data, $matches);
      list($match, $age) = $matches;
      fwrite($_cache_cache, $now + $age);
    }
  }
  else if ($data = $feeds[-1]['expires'])
    fwrite($_cache_cache, strtotime($data));
  else
    fwrite($_cache_cache, $now);

  fwrite($_cache_cache, "\t");

  if ($data = $feeds[-1]['etag'])
    fwrite($_cache_cache, "etag=$data");
  else if ($data = $feeds[-1]['last-modified'])
    fwrite($_cache_cache, "modified=$data");

  fwrite($_cache_cache, "\t" . strtotime($feeds[-1]['date']));
}

/**
  * Retrieve the cacheable data
  * @return Array The cached data
*/
function readCacheData()
{
  global $_cache_cache;

  if (!$_cache_cache)
    return;

  $arr = array();

  $str = fgets($_cache_cache);
  $data = explode("\t", $str);
  list($expires, $mod, $date) = $data;

  $arr['expires'] = $expires;
  $arr['date'] = $date;

  preg_match('/^(\w+)=(.+)$/', $mod, $matches);
  list($match, $mod_type, $mod_val) = $matches;

  $arr['mod-type'] = $mod_type;
  $arr['mod-val'] = $mod_val;

  return $arr;
}

/**
  * Get the handle of the feed in the cache
  * @return The resource of a cached feed
*/
function getCacheFeedHandle()
{
  global $_cache_handle;

  return $_cache_handle;
}

/**
  * Sends a header to get whether the document has been modified
  * @param Resource Socket handle to send the data to
*/
function sendCacheHeader($res)
{
  global $_cache_data;

  if (!$_cache_data)
    $_cache_data = readCacheData();

  if ($_cache_data['mod-type'] == 'etag')
    fwrite($res, 'If-None-Match: ' . $_cache_data['mod-val'] . "\r\n");

  if ($_cache_data['mod-type'] == 'modified')
  {
    if (($_cache_data['mod-val'] + 60) < $_cache_data['date'])
      fwrite($res, 'If-Modified-Since: ' . gmdate('D, d M Y H:i:s GMT', $_cache_data['mod-val']) . "\r\n");
  }
}

/**
  * Invalidates the cache, requiring the feed to be retrieved remotely.
*/
function invalidateCache()
{
  global $_cache_dir, $_cache_id, $_cached_status;

  unlink("$_cache_dir/$_cache_id/data");
  unlink("$_cache_dir/$_cache_id/feed");

  $_cached_status = 0;
}

?>
