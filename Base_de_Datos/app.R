
pacman::p_load(shiny,shinydashboard,fs, magrittr, purrr, tibble, dplyr, stringr, quanteda, pdftools)
load("data_base_de_datos_1.RData")

header <- dashboardHeader(title = "Archivos en PDF")

sidebar <- dashboardSidebar(
    sidebarMenu(
            menuItem("Base de Datos 1", tabName = "data_1", icon = icon("chart-bar")),
            menuItem("Base de Datos 2", tabName = "data_2", icon = icon("database"))
        )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName = "data_1",
                h2("Base de Datos 1"),
                fluidRow(
                    box(title = "Número de palabras totales por ciclo", status = "info", tableOutput("table_1")),

                    box(title = "Las 10 palabras con mayor frecuencia", status = "info",
                        selectInput("variable", "Ciclos:",
                                    c("Primer Ciclo" = "Primer Ciclo",
                                      "Segundo Ciclo" = "Segundo Ciclo",
                                      "Tercer Ciclo" = "Tercer Ciclo",
                                      "Cuarto Ciclo" = "Cuarto Ciclo")),
                        tableOutput("table_2")),

                    box(title = "Palabras únicas en cada archivo", status = "info",
                        selectInput("archivo", "Archivos:",
                                    c("ID0101 Diseño de patrones para datos estructurados" = "ID0101 Diseño de patrones para datos estructurados",
                                      "ID0102 Física clásica" = "ID0102 Física clásica",
                                      "ID0103 Organización y diseño de computadoras" = "ID0103 Organización y diseño de computadoras",
                                      "ID0160 Pensamiento crítico para ingeniería" = "ID0160 Pensamiento crítico para ingeniería")),
                        tableOutput("table_3")),
                )
        ),

        tabItem(tabName = "data_2",
                h2("Widgets tab content")
        )
    )
)


# Define UI for application that draws a histogram
ui <- dashboardPage(header,
                    sidebar,
                    body)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$table_1 <- renderTable(num_words_by_cycle, width = "90%", striped = T, hover = T, align = 'c')
    output$table_2 <- renderTable(filter(most_freq_by_cycle,most_freq_by_cycle$Ciclo == input$variable), width = "90%", striped = T, hover = T, align = 'c')
    output$table_3 <- renderTable(filter(uniq_words_by_syllabus,uniq_words_by_syllabus$Archivo == input$archivo), width = "90%", striped = T, hover = T, align = 'c')
}

# Run the application
shinyApp(ui = ui, server = server)
