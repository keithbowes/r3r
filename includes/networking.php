<?php

/**
  * @package Networking
*/

require_once('feedParser.php');
require_once('cache.php');

/* General networking code */

/**
  * Takes a parsed URL array, performs checks, and returns a string of the normalized URL.
  * @param Array A parsed URL array.
  * @return String A normalized URL string.
*/
function rejoinurl($aurl)
{
  $url = $aurl['scheme'] . '://' . $aurl['host'];
  if (isset($aurl['port']))
    $url .= ':' . $aurl['port'];
  if (substr($aurl['path'], 0, 1) != '/')
    $aurl['path'] = '/' . $aurl['path'];
  $url .= $aurl['path'];
  if (isset($aurl['query']))
    $url .= '?' . $aurl['query'];
  if (isset($aurl['fragment']))
    $url .= '#' . $aurl['fragment'];
  return $url;
}

/**
  * If an RSS field is absent, create one.
  * @param Array The original item.
  * @return Array The new item.
*/
function generateFields($objFeed)
{
  $elems = array('title', 'description', 'link', 'subject', 'created');

  for ($idx = 0; $idx < 5; $idx++)
  {
    if (empty($objFeed[$elems[$idx]]))
      $objFeed[$elems[$idx]] = FEED_LINE_EMPTY;
  }

  return $objFeed;
}

/**
  * Converts a relative URL to an absolute URL from a base URL.
  * @param String The base URL
  * @param The relative URL
  * @return The resultant absolute URL
*/
function relToAbs($base, $rel)
{
  $curl = parse_url($rel);
  $purl = parse_url($base);
  if ($rel != FEED_LINE_EMPTY)
  {
    while(list($key, $val) = each($curl))
      $purl[$key] = $val;

    return rejoinurl($purl);
  }
  else
    return FEED_LINE_EMPTY;
}

/**
  * Take the data from the stream $res and convert to to a form that's used by
  * displayFeedData()
  * @param The resource whose stream is to be read
*/
function internalizeFeedData($res)
{
  global $feeds, $itemIndex, $mime_type, $statusBar;

  while (!@feof($res))
  {
    if (!$str = @fgets($res))
      break;

    $statusBar->set_text(STATUS_GETTING_DATA);

    if ($mime_type == null)
    {
      preg_match('/^([^\;]+)/', $feeds[-1]['content-type'], $matches);
      list($match, $mime_type) = $matches;
    }

    if ($itemIndex > -1)
    {
      if ($mime_type == 'text/x-rss')
        parseTxt($str);
      else if ($mime_type == 'text/plain')
        parseEsf($str);
      else if (strpos($mime_type, 'xml'))
        parseXml($str);
      else
      {
        alert(ALERT_WRONG_MIME . " $mime_type.");
        return;
      }
    }
    else
      parseTxt($str);

    if (!isCached())
      cacheWrite($str);

    doEvent();
  }

  freeXmlParser();
}

/**
  * Display feed data in $feedList.
  * @param Resource Reference to a file or URL.
  * @return Array The data of the feed.
*/
function displayFeedData($res)
{
  global $feedList, $feeds, $feedSrc, $itemIndex, $mime_type, $statusBar;
  static $displayedFeeds;
  static $row_index = -1;

  $baseItem = $itemIndex;
  $feeds = null;

  internalizeFeedData($res);

  if (isset($displayedFeeds[$feeds[0]['title']]))
  {
    $statusBar->set_text(STATUS_TWICE);
    return;
  }

  $statusBar->set_text(STATUS_LV_FILL);
  $row_index++;

  if ($feeds[0]['title'] || $feeds[0]['subject'] || $feeds[0]['created'])
  {
    $feeds[0] = generateFields($feeds[0]);
    $feedList->append(array($feeds[0]['title'], '', $feeds[0]['subject'], $feeds[0]['created']));
    $feedList->set_data($row_index, array($feeds[0], $feedSrc, true));
  }

  $feedList->freeze();
  if (!getSetting('display-feed-title-only'))
  {
    $nFeeds = count($feeds) + $baseItem;
    for ($idx = 1; $idx < $nFeeds; $idx++)
    { 
      if (is_array($feeds[$idx]))
      {
        // Check for stray blank lines.  It's messy, I know.
        if (count($feeds[$idx]) == 1 && isset($feeds[$idx]['']))
          continue;

        $row_index++;
        $feeds[$idx] = generateFields($feeds[$idx]);

        $feeds[$idx]['link'] = relToAbs($feeds[0]['link'], $feeds[$idx]['link']);
        $feedList->append(array('', $feeds[$idx]['title'], $feeds[$idx]['subject'], $feeds[$idx]['created']));
        $feedList->set_data($row_index, array($feeds[$idx], $feedSrc, false));
      }
    }
  }
  $feedList->thaw();

  $displayedFeeds[$feeds[0]['title']] = true;
  $mime_type = null;

  writeCacheData();
}

/**
  * Retrieve a feed from a remote server.
  * @param String $url to retrieve.
*/
function getRemoteFeed($url)
{
  global $feeds, $feedSrc, $itemIndex, $mime_type, $statusBar, $urlCombo_entry;
  static $displayedFeeds;

  $itemIndex = -1;
  $feedSrc = $url;

  $aurl = parse_url($url);
  if (empty($aurl['scheme']))
    $aurl['scheme'] = 'http';
  if (empty($aurl['port']))
    $aurl['port'] = 80;
  if (empty($aurl['path']))
    $aurl['path'] = '/';

  $req = rejoinurl($aurl);

  if (getSetting('use-proxy'))
  {
    $server = getSetting('proxy-addr');
    $port = getSetting('proxy-port');
  }
  else
  {
    $port = $aurl['port'];
    $server = $aurl['host'];
  }

  if (empty($displayedFeeds[$url]))
  {
    openCache($url);
    $is_cached = isCached();

    if ($is_cached != 2)
      @$pfeed = fsockopen("$server", $port, $errno, $errstr, getSetting('timeout-sec'));

    if (!$pfeed && $is_cached < 2)
    {
      $statusBar->set_text(STATUS_CONNECT_FAIL);
      alert(ALERT_CANT_CONN . " ($errstr)");

      if (isCached())
        displayFeedData(getCacheFeedHandle());
      else
        return null;
    }
    else
    {
      if ($is_cached != 2)
      {
        $statusBar->set_text(STATUS_OPEN_CONNECTION);

        if (!getSetting('use-proxy'))
          fwrite($pfeed, 'GET ' . $aurl['path'] . " HTTP/1.1\r\n");
        else
          fwrite($pfeed, 'GET ' . $req . " HTTP/1.1\r\n");

        fwrite($pfeed, 'Host: ' . $aurl['host'] . ':' . $aurl['port'] . "\r\n");
        fwrite($pfeed, 'User-Agent: ' . getSetting('user-agent') . "\r\n");
        fwrite($pfeed, 'Accept: ' . getSetting('accept-types') . "\r\n");
        fwrite($pfeed, "Accept-Encoding: \r\n");
        if ($is_cached == 1)
          sendCacheHeader($pfeed);
        fwrite($pfeed, "Connection: close\r\n");
        fwrite($pfeed, "\r\n");
      }

      if ($is_cached)
      {
        $code = '304';
        if ($is_cached == 2 || (($is_cached == 1) && strpos(fgets($pfeed), $code)))
        {
          if (!getSetting('hide-cached-feeds'))
            displayFeedData(getCacheFeedHandle());

          if ($pfeed)
            fclose($pfeed);

          $statusBar->set_text(STATUS_CACHED_MSG);
        }
        else
        {
          invalidateCache();
          getRemoteFeed($url);
        }
      }
      else if ($pfeed)
      {
        @fwrite($pfeed, "Connection: close\r\n");
        @fwrite($pfeed, "\r\n");

        displayFeedData($pfeed);

        $statusBar->set_text(STATUS_OPENING_CONNECTION);

        if (!$feeds)
          return;
      }
    }
  }
  else
    $statusBar->set_text(STATUS_TWICE);

  @fclose($pfeed);

  if (empty($code))
  {
    list($key, $val) = each($feeds[-1]);
    preg_match('/\w+\/(\d\.\d)\s+(\d{3})\s+(.+)\r\n/', $key, $matches);
    list($match, $version, $code, $reason) = $matches;
  }

  switch (substr($code, 0, 1))
  {
    case '':
      $statusBar->set_text(STATUS_DONE_MSG);
      break;
    case 2:
      $statusBar->set_text(STATUS_DONE_MSG . " ($reason)");
      if ($code == 206)
        invalidateCache();
      break;
    case 3:
      if (($header = $feeds[-1]['location']) || ($header = $feeds[-1]['content-location']))
      {
        if ($code == 301)
        {
          $feedSrc = $header;
          $urlCombo_entry->set_text($header);
        }

        $itemIndex--;
        getRemoteFeed($header);
      }
      break;
    case 4:
    case 5:
      $statusBar->set_text(STATUS_DONE_MSG . ", with error: $reason");
      break;
    default:
      $statusBar->set_text(STATUS_UNIMP_STATUS);
  }

  closeCache();
}

/**
  * Retrieve a feed that's on your computer.
  * @param String The file to parse.
  * @param The index of the first item.
*/
function getLocalFeed($file, $start_index = 0)
{
  global $feedSrc, $itemIndex, $mime_type, $statusBar;

  $itemIndex = $start_index;
  $feedSrc = $file;

  $cwd = getcwd();
  chdir(dirname($file));

  $pi = pathinfo($file);
  switch ($pi['extension'])
  {
    case 'esf':
      $mime_type = 'text/plain';
      break;
    case 'r3':
    case 'rss3':
    case 'txt':
      $mime_type = 'text/x-rss';
      break;
    case 'rdf':
    case 'rss':
    case 'xml':
      $mime_type = 'application/xml';
  }

  @$fh = fopen(basename($file), 'r');
  if (!$fh)
  {
    alert("$file " . ALERT_CANT_OPEN);
    return;
  }

  displayFeedData($fh);
  $statusBar->set_text(STATUS_READ_LOCAL);
  fclose($fh);
  chdir($cwd);
}

?>
