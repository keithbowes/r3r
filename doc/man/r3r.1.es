\" vi:ft=nroff
.TH R3R 1 "2011-12-25"

.SH NOMBRE
  r3r - Un simple legador de fuentes

.SH SINTAXIS

.P
r3r

.SH DESCRIPCIÓN
.P
r3r obtiene una dirección de  HTTP o un archivo local y lo lee. El archivo de subscripciones para leer fuentes siempre.

.SS "Los formatos de archivos"

.P
R3R actualmente apoya los formatos RSS (0.9 a 2.0), RSS 3.0, Atom y ESF. Extractará el formato de la tipo de HTTP o extensión de un archivo. Puede activar adivinando que analizará URL-es de HTTP para esperemos mejores resultes.

.P
Tipos de HTTP:
 application/rdf+xml, application/rss+xml:  RSS (0.9 a 2.0)
 application/atom+xml: Atom
 text/x-rss, text/plain: RSS 3.0
 esf/text: ESF

.P
Extensiones de Archivos:
 .rss, .rdf: RSS (0.9 a 2.0)
 .r3, .rss3: RSS 3.0
 .atom: Atom
 .esf: ESF

.SH FILES

.P
 config/r3r/r3r.ini
   Archivo de configuración.
 data/r3r/clav
   Asignaciones de teclas.
 data/r3r/history
   Historia de visitadas fuentes.
 data/r3r/subscriptions.txt
   URL-es a que estás subscribiendo.
 
 
.SS Filtradores 

.P
Filtradores pueden haber creados en la carpeta de datos. Tenemos el nombre <campo>.filter, donde <campo> puede ser title (título), subject (tema), author (autor), etc. El archivo contiene texto o expresiones regulares para bloquear, uno por línea. Ejemplo:
.br
foo
.br
/b[aeiou]r/i
.br
/Baz/

.SH TUI

.P
TUI está la más madura de las interfaces de usuarios (la interfaz que uso la más frecuentemente). Muestra un lista de entradas en el cuadro izquierdo y datos importantes sobre ella en el cuadro derecho. Puede escrolear de las entradas por las claves normales. Oprimir h en dentro del programa por ayuda sobre las claves.

.SH GUI

.P
GUI está solo una demostración de tecnología de usar la biblioteca compartido de Pascal en un programa de C++ (debería funciona de otros idiomas de programación), pero es utilizable. Muestra una lista de entradas en una ventana de listas. Eligir una entrada mostrará su descripción en una ventana de web. Puede administrar subscripciones en el dialogo de configuración.

.SH HTML

.P
Puede usar también la interfaz de HTML. Durante invocación escribe los contenidos de fuentes a una página web. Puede direccionar eso a un archivo. En una servidor web, puede fácilmente invocar el programa para remoto acceso de fuentes.

.SH "Documentación para programadores"

.P
Por favor mirar en las subcarpetas en ./doc de la fuente de código para información pertinente sobre construyendo, desarrollando, escribiendo tus proprias interfaces, etc.

.SH AUTOR
  Keith Bowes
