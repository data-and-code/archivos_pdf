# Base de datos 2
pacman::p_load(purrr, stringr, dplyr, tidyr, ggplot2, lubridate)

# Importar al entorno el texto de los PDFs diarios (obtenidos previamente mediante web scraping)
load("base_de_datos_2.RData")

# 2. Con la información proporcionada a nivel mundial, cree un tabla y muestre un gráfico que visualice la información mensual.

# Extraer el texto del que se obtiene la informacion por regiones de la OMS
regions_info <- map_depth(pdf_files, 2, pluck, 1)

# Extraer los datos de cada continente

# America
america <-
  # Extraer el numero de acuerdo con la secuencia de valores detectada al principio y final del mes
  map(regions_info, str_extract_all, pattern = "1[3-6],([:digit:]{3},?){2}") %>%
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
  map_at(24, ~ "15,872,421") %>%
  # Convertir la lista en un arreglo numerico
  flatten_chr() %>%
  str_remove_all(pattern = ",") %>%
  as.integer()

# Europa
europe <-
  # Extraer el numero de acuerdo con la secuencia de valores detectada al principio y final del mes
  map(regions_info, str_extract_all, pattern = "(?<=\\n|\\s)[4-5],([:digit:]{3},?){2}") %>%
  # Eliminar los numeros que no corresponden a posibles resultados
  map_depth(2, str_subset, pattern = "0{3}", negate = TRUE) %>%
  # Poner los resultados en un solo arreglo por dia
  transpose() %>%
  map_depth(1, flatten_chr) %>%
  # Eliminar los numeros repetidos
  map(unique) %>%
  # Elegir manualmente ciertos numeros
  map_at(c(1, 7), pluck, 1) %>%
  map_if(~ length(.x) > 1, pluck, 2) %>%
  # Corregir manualmente ciertos numeros
  map_at(9, ~ "4,600,967") %>%
  # Convertir la lista en un arreglo numerico
  flatten_chr() %>%
  str_remove_all(pattern = ",") %>%
  as.integer()

# Asia Sudoriental
southeastern_asia <-
  # Extraer el numero de acuerdo con la secuencia de valores detectada al principio y final del mes
  map(regions_info, str_extract_all, pattern = "(?<=\\s)[4-6],([:digit:]{3},?){2}") %>%
  # Eliminar los numeros que no corresponden a posibles resultados
  map_depth(2, str_subset, pattern = "0{3}", negate = TRUE) %>%
  # Poner los resultados en un solo arreglo por dia
  transpose() %>%
  map_depth(1, flatten_chr) %>%
  # Eliminar los numeros repetidos
  map(unique) %>%
  # Elegir manualmente ciertos numeros
  map_at(7, pluck, 2) %>%
  map_if(~ length(.x) > 1, pluck, 1) %>%
  # Corregir manualmente ciertos numeros
  map_at(24, ~ "6,436,394") %>%
  map_at(28, ~ "6,810,494") %>%
  # Convertir la lista en un arreglo numerico
  flatten_chr() %>%
  str_remove_all(pattern = ",") %>%
  as.integer()

# África
africa <-
  # Extraer el numero de acuerdo con la secuencia de valores detectada al principio y final del mes
  map(regions_info, str_extract_all, pattern = "(?<=\\n)1,([:digit:]{3},?){2}") %>%
  # Eliminar los numeros que no corresponden a posibles resultados
  map_depth(2, str_subset, pattern = "0{3}", negate = TRUE) %>%
  # Poner los resultados en un solo arreglo por dia
  transpose() %>%
  map_depth(1, flatten_chr) %>%
  # Eliminar los numeros repetidos
  map(unique) %>%
  # Elegir manualmente ciertos numeros
  map_at(c(2, 4, 23), pluck, 1) %>%
  map_at(5:6, pluck, 2) %>%
  # Convertir la lista en un arreglo numerico
  flatten_chr() %>%
  str_remove_all(pattern = ",") %>%
  as.integer()

# Mediterráneo Oriental
eastern_mediterranean <-
  # Extraer el numero de acuerdo con la secuencia de valores detectada al principio y final del mes
  map(regions_info, str_extract_all, pattern = "[1-2],([:digit:]{3},?){2}") %>%
  # Eliminar los numeros que no corresponden a posibles resultados
  map_depth(2, str_subset, pattern = "0{3}", negate = TRUE) %>%
  # Poner los resultados en un solo arreglo por dia
  transpose() %>%
  map_depth(1, flatten_chr) %>%
  # Eliminar los numeros repetidos
  map(unique) %>%
  # Elegir manualmente ciertos numeros
  map_at(c(1:2, 17, 18, 28), pluck, 1) %>%
  map_at(c(22:27, 30), pluck, 2) %>%
  map_at(3, pluck, 3) %>%
  map_if(~ length(.x) == 2, pluck, 1) %>%
  # Convertir la lista en un arreglo numerico
  flatten_chr() %>%
  str_remove_all(pattern = ",") %>%
  as.integer()

# Pacífico Occidental
western_pacific <-
  # Extraer el numero de acuerdo con la secuencia de valores detectada al principio y final del mes
  map(regions_info, str_extract_all, pattern = "(?<=\\n)[4-6][:digit:]{2},[:digit:]{3}") %>%
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
  map_at(3, ~ "505,156") %>%
  map_at(11, ~ "535,413") %>%
  map_at(14, ~ "550,664") %>%
  map_at(16, ~ "560,287") %>%
  map_at(19, ~ "573,120") %>%
  map_at(20, ~ "577,905") %>%
  map_at(26, ~ "598,060") %>%
  map_at(27, ~ "600,891") %>%
  # Convertir la lista en un arreglo numerico
  flatten_chr() %>%
  str_remove_all(pattern = ",") %>%
  as.integer()

# Almacenar los resultados en una tabla
worldwide_info <- as_tibble(
  list(
    "Días" = seq(from = ymd('2020-09-01'), to = ymd('2020-09-30'), by = 'days'),
    "América" = america,
    "Asia Sudoriental" = southeastern_asia,
    "Europa" = europe,
    "Mediterráneo Oriental" = eastern_mediterranean,
    "África" = africa,
    "Pacífico Occidental" = western_pacific
  )
)

worldwide_info

# Crear grafico para visualizar la información mensual
worldwide_info %>%
  pivot_longer(2:7, names_to = "Regiones de la OMS", values_to = "cases") %>%
  ggplot(aes(x = `Días`, y = cases, color = `Regiones de la OMS`)) +
    geom_line() +
    scale_colour_manual(values = c("green", "red2", "purple", "blue2", "green4", "yellow3")) +
    labs(y = "Número de casos", title = "Casos acumulados de COVID-19 por SARS-CoV-2 por regiones de la OMS\n(Septiembre 2020)")
