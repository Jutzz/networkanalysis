Die Skripts in /code sind in aufsteigender Reihenfolge ihrer Benennung auszuführen.
Da sie z.T. sehr speicherintensiv sind, empfiehlt es sich, dabei schrittweise vorzugehen.
Die Anhangsverweise beziehen sich auf das Verzeicnis des digitalen Anhang im Dokument.

01_osm_extract.R
Hier werden grundlegende Daten erzeugt. Verwaltungsgrenzen und RegioStaR-Klassifizierungen werden heruntergeladen und verknüpft, aktuelle OSM-Daten werden heruntergeladen und zugeschnitten. Die verschiedenen Zuschnitte werden für verschiedene r5r-Netzwerke genutzt.
Die für die BA verwendeten Zuschnitte sind im digitalen Anhang in r5r_networks.zip (A3-1--A3-3) zu finden, die Ausgangsdaten vom 21.05.2026 in input.zip (A1-2).

Vor dem Ausführen muss ein Auszug des zentralen Haltestellenverzeichnis in /geodata/zhv abgelegt werden.
Die für die Bachelorarbeit verwendete Version ist im digitalen Anhang in input.zip zu finden (A1-6).

zhv_date ist in dataenv.R so anzupassen, dass die korrekte Version für die Berechnung der Bedienungsqualität verwendet wird.

02_de_gtfs_cleaning.R
Vor dem Ausführen muss ein DELFI-GTFS-Feed in /feeds/raw abgelegt werden.
Der für die Bachelorarbeit verwendete Feed ist im digitalen Anhang in input.zip zu finden (A1-1).
Der Feed wird zugeschnitten und notwendige transfers und trips wieder hinzugefügt.
Die in der BA verwendeten Extrakte sind in r5r_networks.zip zu finden (A3-1--A3-2)

03_find_valid_dates.R
Ferien- und Feiertage sowie Wochenenden werden aus dem zugeschnittenen Feed entfernt und dieser mit check_gtfs_discont.R auf Datenlücken einzelner VU geprüft.
In code/temp werden zwei Textdateien geschrieben, die Wochen- bzw. Normtage enthalten, die für die Analyse genutzt werden.

04_stop_frequencies.R
Die Tabellen des Feeds werden so kombiniert, das ein vollständiger Abfahrtenplan aller untersuchten Tage entsteht. Die stop_ids werden vereinheitlicht und eine Bedienungsqualität ermittelt. Endhalte werden zuvor entfernt.
Output sind eine Liste der Halte mit Bedienungsqualität, in der BA A2-9.

	mean_frequencies berechnet auf dieser Grundlage Bedienungsqualitäten anhand des Tagesmittels, des Stundenmittels, des Wochentags und des gesamten Untersuchungszeitraums. Outputs als A2-7, A2-8, A2-10 und A2-11 im digitalen Anhang.

05_bq_summaries.R
Aggregierung und Plotten der Bedienungsqualitäten. Auch hier tägliche und Stundenmittel-Aggregation, allerdings auf Grundlage der stündlichen Bedienungsqualitäten, nicht der Abfahrten. Output Plots unter A4-1-1. Variabilitätsstatistiken der BQ, Output als Geopackage für das gesamte Gebiet und als Zuschnitt, A1-5-2 bzw. A4-3-1 im Anhang.

06_grid_creation.R
Erstellung eines Geogitters für das Untersuchungsgebiet mit INSPIRE-konformen IDs.

07_POI_creation.R
Vor der Ausführung muss tags_filter.cmd ausgeführt werden, bzw. ein OSM-Extrakt mit osmium tags-filter entsprechend der dort aufgeführten Konfiguration vorgefiltert werden und unter osmdata/zentraler_ort_pois.pbf gespeichert werden.

Die vorgefilterten OSM-Daten werden entsprechend osmconf_cpt.txt in ein GeoPackage umgewandelt. Daraus werden zwei POI-Sätze mit Basiskategorien und erweiterten Kategorien erstellt. Die für die BA verwendeten POI-Sätze sind unter A1-7-1 und A1-7-2 im digitalen Anhang.

Sportplätze die weniger als 100 m auseinanderliegen werden zu einem POI zusammengefasst, um zu verhindern, das eine einzige zentralörtliche Funktion die Dichte zu stark beeinflusst.

08_cpt_KDE.R
Auf Grundlage der erstellten POI-Sätze und Gemeinden des Untersuchungsgebietes wird für jede Gemeinde eine Kerndichtenschätzung durchgeführt. Dabei entstehen Raster wie in A2-6 beispielhaft abgelegt. Die Raster werden klassifiziert und die Flächen bewertet. Die ZO-Kandidaten werden entsprechend ihrer fußläufigen Erreichbarkeit von POI bewertet. Dazu muss zunächst ein Ordner mit einem OSM-Extrakt angelegt werden, in dem r5r den Netzwerkgraphen erstellen kann, in diesem Fall /r5core_2026-05-21_osmonly_nrw, im Anhang unter A3-3. Entsprechend der im Dokument beschriebenen Methode werden zentrale Orte bestimmt.
Zudem werden die rheinland-pfälzischen zentralen Orte aus dem WFS abgerufen.

09_grid_zensusvalues.R
Zuordnung der Zensusbevölkerungszahlen zum Geogitter. Vor der Ausführung muss die Zensus-csv im Ordner /base_data abgelegt werden, verfügbar unter https://www.destatis.de/static/DE/zensus/gitterdaten/Zensus2022_Bevoelkerungszahl.zip

10_i2_traveltimes.R
Berechnung von Gehzeiten zwischen Haltestellen und Zensuszellen. Ergebnis im digitalen Anhang unter A2-12.
Vor der Ausführung muss ein Ordner mit einem OSM- und einem GTFS-Extrakt angelegt werden, in dem r5r den Netzwerkgraphen erstellen kann, in diesem Fall /r5core_2026-05-18, im Anhang unter A3-1.

11_i2_analysis.R
Ermittlung von Erschließungsqualitäten der Zensuszellen auf Grundlage der BQ und der Reisezeiten. Für die weitere Verarbeitung wurden nur die stündlichen Werte genutzt, zusätzlich kann die EQ auf Grundlage der täglichen, wochentäglichen, stundengemittelten und gesamtzeiträumlichen BQ-Werte berechnet werden.
Outputs im digitalen Anhang: A2-1--A2-3

12_i2_summaries.R
Aggregieren der stündlichen EQ über verschiedene Zeiträume mit dem p50-Quantil (quantile(Erschließungsqualität, probs = 0.5, type = 1)) als Statistik. Output im digitalen Anhang: A4-4--A4-7.

13_i2_togrid.R
Verknüpfen des Zensusgitters mit den Ergebnissen der EQ, aggregieren auf verschiedenen räumlichen Ebenen (Gemeinde, Kreis, RegioStaR7).

14_i3_traveltimes.R
Berechnung von ÖPNV-Reisezeitmatrizen für die Erreichbarkeit zentraler Orte.
Vor dem Ausführen muss ein Ordner mit einem OSM- und einem GTFS-Extrakt angelegt werden, in dem r5r den Netzwerkgraphen erstellen kann, in diesem Fall /r5core_2026-05-18_large, im Anhang unter A3-2, und die als Zielort verwendeten zentralen Orte abgelegt werden. Die hier verwendete Auswahl im digitalen Anhang als A1-5-1.

15_i3_shortest_times.R
Berechnung des Anbindungsindikators: Ermittlung kürzester Reisezeiten zu je mindestens Grundzentrum, Mittelzentrum, Oberzentrum, Festlegung der maximalen Reisezeit nach Zentralität des Startortes und binäre Anbindung (capt_{gz,mz,oz}). Für jede untersuchte Stunde. Output im digitalen Anhang unter A2-4-1.

16_i3_summaries.R
Verknüpfung des Anbindungsindikators mit dem Zensusgitter mit dem Anteil der erreichten Stunden als Statistik (>0.5 --> erreicht). Aggregieren der stündlichen Anbindung nach verschiedenen räumlichen und zeitlichen Ebenen.


