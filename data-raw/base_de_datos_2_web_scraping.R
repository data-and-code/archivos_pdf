# Extraccion del texto de los PDFs
pacman::p_load(fs, pdftools, tesseract, rvest, purrr, furrr, stringr)

# Definir el numero de workers que permitiran ejecutar funciones de purrr de forma paralela
plan(multisession, workers = 4)

# Obtener las direcciones de los PDFs diarios del mes (Septiembre)
pdf_urls <- read_html("https://www.gob.mx/salud/documentos/coronavirus-covid-19-comunicados-tecnicos-diarios-septiembre-2020") %>%
  # Obtener los nodos HTML que corresponden a la etiqueta "a"
  html_nodes(css = "a") %>%
  # Extraer de los nodos el valor dentro del atributo "href", el cual contiene URLs
  html_attr(name = "href") %>%
  # Descartar los valores NA's
  discard(is.na) %>%
  # Quedarse con las URLS que terminen con ".pdf"
  keep(str_detect, pattern = ".pdf$") %>%
  # Completar cada URL
  map_chr(~ str_c("https://www.gob.mx", .x))

# Extraer el texto de cada PDF utilizando distintos valores de dpi
pdf_files_1000 <- pdf_urls %>%
  # Usar Tesseract para detectar el texto dentro de cada PDF (incluso del texto dentro de las imagenes)
  future_map(pdf_ocr_text, pages = c(1, 5), language = "spa", dpi = 1000) %>%
  # Renombrar cada elemento del resultado con su nombre de archivo original
  set_names(nm = str_extract(pdf_urls, pattern = "(?<=/)Comunicado.*\\.pdf$")) %>%
  # Invertir el orden del resultado para que los documentos se ordenen por fecha de forma ascendente
  rev()

pdf_files_800 <- pdf_urls %>%
  # Usar Tesseract para detectar el texto dentro de cada PDF (incluso del texto dentro de las imagenes)
  future_map(pdf_ocr_text, pages = c(1, 5), language = "spa", dpi = 800) %>%
  # Renombrar cada elemento del resultado con su nombre de archivo original
  set_names(nm = str_extract(pdf_urls, pattern = "(?<=/)Comunicado.*\\.pdf$")) %>%
  # Invertir el orden del resultado para que los documentos se ordenen por fecha de forma ascendente
  rev()

pdf_files_750 <- pdf_urls %>%
  # Usar Tesseract para detectar el texto dentro de cada PDF (incluso del texto dentro de las imagenes)
  future_map(pdf_ocr_text, pages = c(1, 5), language = "spa", dpi = 750) %>%
  # Renombrar cada elemento del resultado con su nombre de archivo original
  set_names(nm = str_extract(pdf_urls, pattern = "(?<=/)Comunicado.*\\.pdf$")) %>%
  # Invertir el orden del resultado para que los documentos se ordenen por fecha de forma ascendente
  rev()

pdf_files_700 <- pdf_urls %>%
  # Usar Tesseract para detectar el texto dentro de cada PDF (incluso del texto dentro de las imagenes)
  future_map(pdf_ocr_text, pages = c(1, 5), language = "spa", dpi = 700) %>%
  # Renombrar cada elemento del resultado con su nombre de archivo original
  set_names(nm = str_extract(pdf_urls, pattern = "(?<=/)Comunicado.*\\.pdf$")) %>%
  # Invertir el orden del resultado para que los documentos se ordenen por fecha de forma ascendente
  rev()

pdf_files_650 <- pdf_urls %>%
  # Usar Tesseract para detectar el texto dentro de cada PDF (incluso del texto dentro de las imagenes)
  future_map(pdf_ocr_text, pages = c(1, 5), language = "spa", dpi = 650) %>%
  # Renombrar cada elemento del resultado con su nombre de archivo original
  set_names(nm = str_extract(pdf_urls, pattern = "(?<=/)Comunicado.*\\.pdf$")) %>%
  # Invertir el orden del resultado para que los documentos se ordenen por fecha de forma ascendente
  rev()

pdf_files_600 <- pdf_urls %>%
  # Usar Tesseract para detectar el texto dentro de cada PDF (incluso del texto dentro de las imagenes)
  future_map(pdf_ocr_text, pages = c(1, 5), language = "spa", dpi = 600) %>%
  # Renombrar cada elemento del resultado con su nombre de archivo original
  set_names(nm = str_extract(pdf_urls, pattern = "(?<=/)Comunicado.*\\.pdf$")) %>%
  # Invertir el orden del resultado para que los documentos se ordenen por fecha de forma ascendente
  rev()

pdf_files_550 <- pdf_urls %>%
  # Usar Tesseract para detectar el texto dentro de cada PDF (incluso del texto dentro de las imagenes)
  future_map(pdf_ocr_text, pages = c(1, 5), language = "spa", dpi = 550) %>%
  # Renombrar cada elemento del resultado con su nombre de archivo original
  set_names(nm = str_extract(pdf_urls, pattern = "(?<=/)Comunicado.*\\.pdf$")) %>%
  # Invertir el orden del resultado para que los documentos se ordenen por fecha de forma ascendente
  rev()

pdf_files_500 <- pdf_urls %>%
  # Usar Tesseract para detectar el texto dentro de cada PDF (incluso del texto dentro de las imagenes)
  future_map(pdf_ocr_text, pages = c(1, 5), language = "spa", dpi = 500) %>%
  # Renombrar cada elemento del resultado con su nombre de archivo original
  set_names(nm = str_extract(pdf_urls, pattern = "(?<=/)Comunicado.*\\.pdf$")) %>%
  # Invertir el orden del resultado para que los documentos se ordenen por fecha de forma ascendente
  rev()

# Almacenar los resultados en una sola lista
pdf_files_2 <- list(
  dpi_500 = pdf_files_500,
  dpi_550 = pdf_files_550,
  dpi_600 = pdf_files_600,
  dpi_650 = pdf_files_650,
  dpi_700 = pdf_files_700,
  dpi_750 = pdf_files_750,
  dpi_800 = pdf_files_800,
  dpi_1000 = pdf_files_1000
)

# Se almacena el resultado en un archivo RData para usarlo el resto del proyecto
save(pdf_files_2, file = "extdata/base_de_datos_2_raw.RData")
