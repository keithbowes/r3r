<?php

/**
  * @package Networking
*/

/* Caching, duh */

/**
  * Handle of the cached data (current ETag only)
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
  * Boolean of whether the feed is cached
  * @access private
*/
$_is_cached = false;

/**
  * Opens the cache
  * @param String The URL of the cache
*/
function openCache($url)
{
  global $_cache_cache, $_cache_dir, $_cache_handle, $_cache_id, $_is_cached;

  $_cache_id = rawurlencode($url);
  $_cache_dir = SETTINGS_DIR . '/cache';

  $_cache_etag = 'etag';
  $_cache_feed = 'feed';

  $cwd = getcwd();

  if (!is_dir($_cache_dir))
    mkdir($_cache_dir);

  chdir($_cache_dir);

  if (!is_dir($_cache_id))
    mkdir($_cache_id);

  chdir($_cache_id);

  if (file_exists($_cache_etag))
    $_is_cached = true;
  else
  {
    $_is_cached = false;
    $fh = fopen($_cache_etag, 'w');
    fclose($fh);
  }

  if (!file_exists($_cache_feed))
  {
    $fh = fopen($_cache_feed, 'w');
    fclose($fh);
  }

  unset($fh);

  $_cache_cache = fopen($_cache_etag, 'ar+');
  $_cache_handle = fopen($_cache_feed, 'ar+');

  chdir($cwd);
}

/**
  * Close the cache
*/
function closeCache()
{
  global $_cache_cache, $_cache_handle, $_cache_id;

  fclose($_cache_cache);
  fclose($_cache_handle);

  $_cache_id = '';
}

/**
  * Test whether the feed is cached.
  * @return Boolean Whether the feed is cached
*/
function isCached()
{
  global $_is_cached;

  return $_is_cached;
}

/**
  * Write a line of the feed's content to the cache
  * @param String Line of text to write
*/
function cacheWrite($str)
{
  global $_cache_handle, $itemIndex;

  if ($_cache_handle)
    fwrite($_cache_handle, str_replace("\r", '', $str));
}

/**
  * Record the cacheable data
*/
function writeCacheData()
{
  global $_cache_cache, $feeds;

  $etag = $feeds[-1]['etag'];

  if ($etag && readCacheData() != $etag)
    fwrite($_cache_cache, $etag);
}

/**
  * Retrieve the cacheable data
  * @return String The cached data
*/
function readCacheData()
{
  global $_cache_cache;

  rewind($_cache_cache);
  return fgets($_cache_cache);
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
  * Invalidates the cache, requiring the feed to be retrieved remotely.
*/
function invalidateCache()
{
  global $_cache_dir, $_cache_id, $_is_cached;

  unlink("$_cache_dir/$_cache_id/etag");
  unlink("$_cache_dir/$_cache_id/feed");

  $_is_cached = false;
}

?>
