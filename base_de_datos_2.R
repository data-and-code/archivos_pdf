pacman::p_load(pdftools, tesseract)

pdf_file <- pdf_ocr_text("https://www.gob.mx/cms/uploads/attachment/file/581797/Comunicado_Tecnico_Diario_COVID-19_2020.09.30.pdf", language = "spa")
