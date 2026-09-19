# ============================================================
# CSV -> LaTeX hierarchische enumerate-Liste
# ============================================================
# Erzeugt aus dem Anhangsverzeichnis eine hierarchisch
# verschachtelte enumerate-Liste anhand der Spalte "layer".
#
# Die Nummerierung wird automatisch von LaTeX erzeugt.
#
# Benötigte LaTeX-Pakete:
#   \usepackage{enumitem}
#   \usepackage{xurl}
#   \usepackage{microtype} % optional
# ============================================================

# -----------------------------
# Einstellungen
# -----------------------------

input_file  <- "../anhangsverzeichnis_ano.csv"
output_file <- "document/anhangsverzeichnis.tex"

# CSV-Encoding
file_encoding <- "Windows-1252"

# CSV-Trennzeichen
csv_sep <- ";"

# Soll der Dateipfad angezeigt werden?
show_filename <- TRUE

# Soll der Typ angezeigt werden?
show_type <- TRUE

# -----------------------------
# Pakete prüfen
# -----------------------------

if (!requireNamespace("readr", quietly = TRUE)) {
  stop("Bitte zuerst installieren: install.packages('readr')")
}

# -----------------------------
# CSV einlesen
# -----------------------------

dat <- readr::read_delim(
  input_file,
  delim = csv_sep,
  locale = readr::locale(encoding = file_encoding),
  show_col_types = FALSE,
  trim_ws = TRUE,
  na = c("", "NA")
)

# Spaltennamen vereinheitlichen
names(dat) <- trimws(names(dat))

required <- c(
  "name",
  "layer",
  "filename",
  "Typ",
  "Erläuterung"
)

missing <- setdiff(required, names(dat))

if (length(missing) > 0) {
  stop(
    "Folgende Spalten fehlen in der CSV: ",
    paste(missing, collapse = ", ")
  )
}

# -----------------------------
# Hilfsfunktionen
# -----------------------------

latex_escape <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  
  # Backslash zuerst
  x <- gsub(
    "\\\\",
    "\\\\textbackslash{}",
    x
  )
  
  # LaTeX-Sonderzeichen
  x <- gsub(
    "([#$%&_{}])",
    "\\\\\\1",
    x,
    perl = TRUE
  )
  
  # Tilde
  x <- gsub(
    "~",
    "\\\\textasciitilde{}",
    x,
    fixed = TRUE
  )
  
  # Zirkumflex
  x <- gsub(
    "\\^",
    "\\\\textasciicircum{}",
    x
  )
  
  x
}

# Dateipfade werden NICHT mehr mit \allowbreak verändert.
# xurl übernimmt die möglichen Umbruchstellen.
latex_path <- function(x) {
  latex_escape(x)
}

# -----------------------------
# Daten vorbereiten
# -----------------------------

layer_num <- suppressWarnings(
  as.numeric(dat$layer)
)

layer_num[is.na(layer_num)] <- 1

dat$name <- latex_escape(dat$name)
dat$filename <- latex_path(dat$filename)
dat$Typ <- latex_escape(dat$Typ)
dat$Erläuterung <- latex_escape(dat$Erläuterung)

# -----------------------------
# Item erzeugen
# -----------------------------

make_item <- function(i) {
  
  # Name
  content <- paste0(
    "\\textbf{",
    dat$name[i],
    "}"
  )
  
  # Typ
  if (
    show_type &&
    nzchar(dat$Typ[i])
  ) {
    content <- paste0(
      content,
      " \\textnormal{(",
      "\\textit{",
      dat$Typ[i],
      "}",
      ")}"
    )
  }
  
  # Erläuterung
  if (nzchar(dat$Erläuterung[i])) {
    content <- paste0(
      content,
      "\\\\",
      "\n",
      dat$Erläuterung[i]
    )
  }
  
  # Dateipfad
  if (
    show_filename &&
    nzchar(dat$filename[i])
  ) {
    content <- paste0(
      content,
      "\\\\",
      "\n",
      "\\texttt{\\small ",
      dat$filename[i],
      "}"
    )
  }
  
  paste0(
    "\\item ",
    content
  )
}

# -----------------------------
# Verschachtelte enumerate-Liste
# -----------------------------

# Maximale erlaubte Verschachtelung.
# LaTeX unterstützt standardmäßig 4 Ebenen.
max_level <- 4

# Ebenen begrenzen
layer_num <- pmax(
  1,
  pmin(layer_num, max_level)
)

lines <- character(0)

current_level <- 0

for (i in seq_len(nrow(dat))) {
  
  level <- layer_num[i]
  
  # ---------------------------
  # Unterlisten öffnen
  # ---------------------------
  
  while (current_level < level) {
    
    next_level <- current_level + 1
    
    # Unterschiedliche Nummerierungsformate:
    #
    # Ebene 1: 1.
    # Ebene 2: 1.1.
    # Ebene 3: 1.1.1.
    # Ebene 4: 1.1.1.1.
    
    label <- switch(
      as.character(next_level),
      
      "1" = "\\arabic*",
      "2" = "\\arabic{enumi}.\\arabic*",
      "3" = "\\arabic{enumi}.\\arabic{enumii}.\\arabic*",
      "4" = "\\arabic{enumi}.\\arabic{enumii}.\\arabic{enumiii}.\\arabic*",
      
      "\\arabic*."
    )
    
    lines <- c(
      lines,
      paste0(
        "\\begin{enumerate}[",
        "label=",
        label,
        ", leftmargin=*, ",
        "itemsep=0.5em, ",
        "topsep=0.3em",
        "]"
      )
    )
    
    current_level <- next_level
  }
  
  # ---------------------------
  # Ebenen schließen
  # ---------------------------
  
  while (current_level > level) {
    
    lines <- c(
      lines,
      "\\end{enumerate}"
    )
    
    current_level <- current_level - 1
  }
  
  # ---------------------------
  # Item hinzufügen
  # ---------------------------
  
  lines <- c(
    lines,
    make_item(i)
  )
}

# -----------------------------
# Offene Listen schließen
# -----------------------------

while (current_level > 0) {
  
  lines <- c(
    lines,
    "\\end{enumerate}"
  )
  
  current_level <- current_level - 1
}
# -----------------------------
# LaTeX erzeugen
# -----------------------------

tex <- c(
  "% Automatisch erzeugt aus anhangsverzeichnis_ano.csv",
  paste0("% Anzahl Einträge: ", nrow(dat)),
  "",
  "% Benötigte Pakete:",
  "% \\usepackage{enumitem}",
  "% \\usepackage{xurl}",
  "% \\usepackage{microtype} % optional",
  "",
  lines
)

# -----------------------------
# Datei schreiben
# -----------------------------

writeLines(
  tex,
  output_file,
  useBytes = TRUE
)

message(
  "LaTeX-Datei erstellt: ",
  normalizePath(
    output_file,
    mustWork = FALSE
  ),
  "\nEinträge: ",
  nrow(dat)
)

