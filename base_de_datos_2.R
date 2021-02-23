# BAse de datos 2
pacman::p_load(purrr, stringr)

# Importar al entorno el texto de los PDFs diarios (obtenidos previamente mediante web scraping)
load("base_de_datos_2.RData")

# 2. Con la información proporcionada a nivel mundial, cree un tabla y muestre un gráfico que visualice la información mensual.

# Extraer el texto del que se obtiene la informacion de los continentes
continents_info <- map_depth(pdf_files, 2, pluck, 1)

# Extraer los datos de cada continente

# America
america <-
  # Extraer el numero de acuerdo con la secuencia de valores detectada al principio y final del mes
  map(continents_info, str_extract_all, pattern = "1[3-6],([:digit:]{3},?){2}") %>%
  # Eliminar los numeros que no corresponden a posibles resultados
  map_depth(2, str_subset, pattern = "0{3}", negate = TRUE) %>%
  # Poner los resultados en un solo arreglo por dia
  transpose() %>%
  map_depth(1, flatten_chr) %>%
  # Eliminar los numeros repetidos
  map(unique) %>%
  # Elegir manualmente ciertos numeros
  map_if(~ length(.x) == 2, pluck, 1) %>%
  # Completar manualmente los numero faltantes
  map_at(24, ~ "15,872,421")

# Europa
europe <-
  # Extraer el numero de acuerdo con la secuencia de valores detectada al principio y final del mes
  map(continents_info, str_extract_all, pattern = "(?<=\\n|\\s)[4-5],([:digit:]{3},?){2}") %>%
  # Eliminar los numeros que no corresponden a posibles resultados
  map_depth(2, str_subset, pattern = "0{3}", negate = TRUE) %>%
  # Poner los resultados en un solo arreglo por dia
  transpose() %>%
  map_depth(1, flatten_chr) %>%
  # Eliminar los numeros repetidos
  map(unique) %>%
  # Elegir manualmente ciertos numeros
  map_at(1, pluck, 1) %>%
  map_if(~ length(.x) > 1, pluck, 2)

# Asia Sudoriental
southeastern_asia <-
  # Extraer el numero de acuerdo con la secuencia de valores detectada al principio y final del mes
  map(continents_info, str_extract_all, pattern = "(?<=\\s)[4-6],([:digit:]{3},?){2}") %>%
  # Eliminar los numeros que no corresponden a posibles resultados
  map_depth(2, str_subset, pattern = "0{3}", negate = TRUE) %>%
  # Poner los resultados en un solo arreglo por dia
  transpose() %>%
  map_depth(1, flatten_chr) %>%
  # Eliminar los numeros repetidos
  map(unique) %>%
  # Elegir manualmente ciertos numeros
  map_if(~ length(.x) > 1, pluck, 1)

# Mediterráneo Oriental
eastern_mediterranean <-
  # Extraer el numero de acuerdo con la secuencia de valores detectada al principio y final del mes
  map(continents_info, str_extract_all, pattern = "[4-5],([:digit:]{3},?){2}") %>%
  # Eliminar los numeros que no corresponden a posibles resultados
  map_depth(2, str_subset, pattern = "0{3}", negate = TRUE) %>%
  # Poner los resultados en un solo arreglo por dia
  transpose() %>%
  map_depth(1, flatten_chr) %>%
  # Eliminar los numeros repetidos
  map(unique)

# África
africa <-
  # Extraer el numero de acuerdo con la secuencia de valores detectada al principio y final del mes
  map(continents_info, str_extract_all, pattern = "[4-5],([:digit:]{3},?){2}") %>%
  # Eliminar los numeros que no corresponden a posibles resultados
  map_depth(2, str_subset, pattern = "0{3}", negate = TRUE) %>%
  # Poner los resultados en un solo arreglo por dia
  transpose() %>%
  map_depth(1, flatten_chr) %>%
  # Eliminar los numeros repetidos
  map(unique)

# Pacífico Occidental
western_pacific <-
  # Extraer el numero de acuerdo con la secuencia de valores detectada al principio y final del mes
  map(continents_info, str_extract_all, pattern = "[4-5],([:digit:]{3},?){2}") %>%
  # Eliminar los numeros que no corresponden a posibles resultados
  map_depth(2, str_subset, pattern = "0{3}", negate = TRUE) %>%
  # Poner los resultados en un solo arreglo por dia
  transpose() %>%
  map_depth(1, flatten_chr) %>%
  # Eliminar los numeros repetidos
  map(unique)


a <- map_depth(pdf_files, 2, pluck, 1)

a <- map_depth(pdf_files, 1, pluck, 1)

b <-

str_extract_all(a, pattern = "([:digit:][:punct:]?){1,3}\\s?") %>%
  map(str_c, collapse = "") %>%
  map(str_extract_all, pattern = "(?<=[:punct:]|\\s)[1-9][:digit:]{0,2},([:digit:]{1,3}(,|\\.)?){1,2}") %>%
  map(flatten_chr) %>%
  map(str_subset, pattern = "[:digit:]{1,3},(0{3},?){2}", negate = TRUE) %>%
  map(str_subset, pattern = "^[:digit:],([:digit:]{1,3},?)$", negate = TRUE) %>%
  map(str_subset, pattern = "^[2,3][:digit:]{1,2},([:digit:]{3},?){2}$", negate = TRUE)


str_extract_all(a, pattern = "([:digit:][:punct:]?){1,3}\\s?")
([:digit:]{3}(,|\\.)?)*
