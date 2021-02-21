pacman::p_load(fs, magrittr, pdftools, tesseract, rvest, purrr, dplyr, stringr, ggplot2)

pdf_file <- pdf_ocr_text("https://www.gob.mx/cms/uploads/attachment/file/581797/Comunicado_Tecnico_Diario_COVID-19_2020.09.30.pdf", language = "spa")

html_file <- read_html("https://www.gob.mx/salud/documentos/coronavirus-covid-19-comunicados-tecnicos-diarios-septiembre-2020")

html_file %>%
  html_nodes(css = "a") %>%
  html_attr(name = "href") %>%
  discard(is.na) %>%
  keep(str_detect, pattern = "cms")
