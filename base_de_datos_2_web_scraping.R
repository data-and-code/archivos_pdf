# Extraccion del texto de los PDFs
pacman::p_load(pdftools, tesseract, rvest, purrr, stringr)

# Obtener las direcciones de los PDFs del mes (Septiembre)
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

# Extraer el texto de cada PDF
pdf_files <- pdf_urls %>%
  # Usar Tesseract para detectar el texto dentro de cada PDF (incluso del texto dentro de las imagenes)
  map(pdf_ocr_text, language = "spa") %>%
  # Renombrar cada elemento del resultado con su nombre de archivo original
  set_names(nm = str_extract(pdf_urls, pattern = "(?<=/)Comunicado.*\\.pdf$")) %>%
  # Invertir el orden del resultado para que los documentos se ordenen por fecha de forma ascendente
  rev()

# Se almacena el resultado en un archivo RData para usarlo el resto del proyecto
save(pdf_files, file = "base_de_datos_2.RData")
