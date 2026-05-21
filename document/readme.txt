Dieser Zitierstyle für das biblatex Paket setzt die Anforderungen
der neuen Zitieranleitung des Geographischen Institut Köln um.

Enthaltene Dateien:
test.tex             (Eine Beispiel .tex Datei, die ein Literaturverzeichnis
                      nach den Regeln des Geographischen Instituts erstellt)
test.bib             (Eine Beispielbibliographie, die Beispiele für alle
                      relevanten Typen enthält)
geographie_koeln.cbx (Die eigentlichen Zitierstyle Dateien für die Verwendung
geographie_koeln.bbx  mit dem biblatex Paket)

Hier eine kurze Anleitung, wie er zusammen mit dem MikTex TeX Projekt
für Windows benutzt werden kann.
Nach Installation von MikTex (http://miktex.org) muss der in dieser .zip
Datei enthaltene "tex" Ordner in den Ordner 
"C:\Benutzer\BENUTZERNAME\AppData\Roaming\MiKTeX\2.9" entpackt werden.
Danach muss im MikTex Settings Programm (Start - Alle Programme - MikTex 2.9 - 
Maintenance - Settings) die Datenbank aktualisiert werden, hierzu auf den
"Refresh FNDB" Knopf clicken.
Für Unicode Unterstützung muss dann noch Biber installiert werden.
Über diesen Link http://sourceforge.net/projects/biblatex-biber/files/latest
bekommt man eine .zip Datei die eine "biber.exe" Datei enthält.
Diese muss dann in den Ornder "C:\Programme\MiKTeX 2.9\miktex\bin\x64" bzw.
bei 32-bit Windows Installationen in "C:\Programme\MiKTeX 2.9\miktex\bin"
entpackt werden.
Um aus der .tex Datei dann ein PDF zu erstellen, müssen folgende Befehle nacheinander
ausgeführt werden:
pdflatex test.tex
biber test
pdflatex test.tex
pdflatex test.tex
(Bei Verwendund eines Tex-Editors gilt entsprechendes.)

Wenn alles richtig geklappt hat, wird eine "testprojekt.pdf" Datei erstellt, die das
Endergebnis enthält.

test.bib ist eine BibTex Datei, die Testeinträge für alle in der Zitieranleitung
vorgesehen Eintragstypen enthält.
Anzumerken ist, dass beim Eintragstyp "Map" für Kartenwerke der Maßstab im Feld "usera"
stehen muss, und dass bei allen Eintragstypen im Feld "options" "orgauthor" stehen muss,
falls der Autor des zitierten Werks keine Person, sondern eine Organisation ist, und
somit an der Zitatstelle nicht mit Kapitälchen geschrieben werden darf.
Anzumerken ist, dass die .tex Datei und die .bib Datei als Unicode (UTF-8) Dateien gespeichert
bzw. bearbeitet werden sollten.
Damit bei englisch-sprachigen Sammelwerken die englische Abkürzung für den Herausgeber gewählt wird,
muss im Feld "hyphenation" der Wert "english" stehen, bzw. die entsprechende Sprache. Diese
muss auch bei den Optionen des babel Pakets mitangeben werden (ngerman sollte die letzte angegebene
Sprache sein)

Anleitung zur Einrichtung unter MacOS Mojave unter der MacTeX-2018 Distribution 

Die MacTeX Distribution kann unter http://www.tug.org/mactex/ heruntergeladen werden.

Kopieren der Zitieranleitung in die TeX-Distribution:

1. Navigieren zum Ordner texmf-local/tex/latex über die Verknüpfung Library/TeX/Local 
(oder alternativ über den Finder -> Shift+Command+G -> "/usr/local/texlive" ohne Anführungszeichen eingeben)

2. Kopieren des Ordners "biblatex" mit allen Unterverzeichnissen in das zuvor geöffnete Verzeichnis "texmf-local/tex/latex/[hier Ordner biblatex einfügen]"

Aktualisieren der TeX Database:

3. Öffnen des Terminals (z.B. über Spotlight Suche).

4. Im Terminal folgenden Befehl eingeben und danach mit Administrator-Passwort bestätigen:

sudo -H texhash

Alternativ, falls der o.g. Befehl nicht akzeptiert wird:

sudo -H mktexlsr

5. Fertig (Zum testen die mitgelieferte Datei test.tex kompilieren). Biber ist in der o.g. Tex Distribution enthalten. Sollte eine andere TeX Distribution unter MacOS verwendet werden muss biber ggf. noch hinzugefügt werden.

Kompilieren erfolgt analog zur Reihenfolge unter MikTex (Windows):
PdfLaTeX + biber + PdfLaTeX (x2)


Ich hoffe, das ganze hilft jemandem weiter.

Sebastian Brocks
sbrocks@uni-koeln.de