\" vi:ft=nroff
.encoding iso-8859-1
.TH R3R 1 "2011-12-25"

.SH NOMBRE
  r3r - Un simple legador de fuentes

.SH SINTAXIS

.P
 r3r [URL]
.br
 r3r-conv -i archivo_entrado -t tipo_salido [-o archivo_salido]
.br
 r3r_opml [finalidad]

.SH DESCRIPCI�N
.P
r3r obtiene una direcci�n de  HTTP o un archivo local y lo lee. El archivo de subscripciones para leer fuentes siempre.

.SS "Los formatos de archivos"

.P
R3R actualmente apoya los formatos RSS (0.9 a 2.0), RSS 3.0, Atom y ESF. Extractar� el formato de la tipo de HTTP o extensi�n de un archivo. Puede activar adivinando que analizar� URL-es de HTTP para esperemos mejores resultes.

.P
Tipos de HTTP:
.br
 application/rdf+xml, application/rss+xml:  RSS (0.9 a 2.0)
.br
 application/atom+xml: Atom
.br
 text/x-rss, text/plain: RSS 3.0
.br
 esf/text: ESF

.P
Extensiones de Archivos:
.br
 .rss, .rdf: RSS (0.9 a 2.0)
.br
 .r3, .rss3: RSS 3.0
.br
 .atom: Atom
.br
 .esf: ESF
.br

.P
r3r-conv est� utilizable para convertir entre formatos de fuentes.

.SH FILES

.P
 config/r3r/r3r.ini
.br
   Archivo de configuraci�n.
.br
 data/r3r/clav
.br
   Asignaciones de teclas.
.br
 data/r3r/history
.br
   Historia de visitadas fuentes.
.br
 data/r3r/abos.opml
.br
   URL-es a que est�s subscribiendo.
.br
   r3r_opml est� utilizable para convertir el antiguo archivo subscriptions.txt a abos.opml.
 
 
.SS Filtradores 

.P
Filtradores pueden haber creados en la carpeta de datos. Tenemos el nombre <campo>.filter, donde <campo> puede ser title (t�tulo), subject (tema), author (autor), etc. El archivo contiene texto o expresiones regulares para bloquear, uno por l�nea. Ejemplo:
.br
foo
.br
/b[aeiou]r/i
.br
/Baz/

.SH TUI

.P
TUI est� la m�s madura de las interfaces de usuarios (la interfaz que uso la m�s frecuentemente). Muestra un lista de entradas en el cuadro izquierdo y datos importantes sobre ella en el cuadro derecho. Puede escrolear de las entradas por las claves normales. Oprimir h en dentro del programa por ayuda sobre las claves.

.SH GUI

.P
GUI est� solo una demostraci�n de tecnolog�a de usar la biblioteca compartido de Pascal en un programa de C++ (deber�a funciona de otros idiomas de programaci�n), pero es utilizable. Muestra una lista de entradas en una ventana de listas. Eligir una entrada mostrar� su descripci�n en una ventana de web. Puede administrar subscripciones en el dialogo de configuraci�n.

.SH HTML

.P
Puede usar tambi�n la interfaz de HTML. Durante invocaci�n escribe los contenidos de fuentes a una p�gina web. Puede direccionar eso a un archivo. En una servidor web, puede f�cilmente invocar el programa para remoto acceso de fuentes.

.SH "Documentaci�n para programadores"

.P
Por favor mirar en las subcarpetas en ./docs de la fuente de c�digo para informaci�n pertinente sobre construyendo, desarrollando, escribiendo tus proprias interfaces, etc.

.SH AUTOR
  Keith Bowes
