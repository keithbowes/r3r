<?php
  
# This is a script to turn old configuration-style files into
# source files that R3R can use.

# Syntax: php cnf2lng.php [file1 [file2 [...]]]
# Specify no arguments for all files in the current directory

if ($argc < 2)
{
  $dir = opendir(getcwd());
  while (($file = readdir()) !== FALSE)
  {
    if ((substr($file, 0, 1) != '.') && !strstr($file, '.php') && !strstr($file, '.lng') && !is_dir($file))
    {
      $argv[$argc] = $file;
      $argc++;
    }
  }
  closedir($dir);
}

for ($i = 1; $i < $argc; $i++)
{
  $cnf = fopen($argv[$i], 'r');
  $lng = fopen($argv[$i] . '.lng', 'w');
  fwrite($lng, "<?php\n\n");

  while (!feof($cnf))
    fwrite($lng, preg_replace('/(^[^=]+)=(.+)$/', 'define(\'$1\', "$2");', fgets($cnf)));

  fwrite($lng, "\n?>");

  fclose($lng);
  fclose($cnf);
}

?> 
