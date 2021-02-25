# Aplicacion de Shiny
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)
library(purrr)
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)


# Cargar los datos al entorno
load("base_de_datos_1.RData")
load("base_de_datos_2.RData")

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

                    boxPlus(title = "10 palabras con mayor frecuencia por ciclo",
                            closable = F,
                            solidHeader = F,
                            collapsible = T,
                            status = "info",
                            # Create a new Row in the UI for selectInputs
                            fluidRow(
                                column(4,
                                       selectInput("pal",
                                                   "Palabras:",
                                                   c("todos",
                                                     unique(as.character(most_freq_by_cycle$Palabras))))
                                ),
                                column(4,
                                       selectInput("frec",
                                                   "Frecuencia:",
                                                   c("todos",
                                                     unique(as.character(most_freq_by_cycle$Frecuencia))))
                                ),
                                column(4,
                                       selectInput("cic",
                                                   "Ciclo:",
                                                   c("todos",
                                                     unique(as.character(most_freq_by_cycle$Ciclo))))
                                )
                            ),
                            # Create a new row for the table.
                            DT::dataTableOutput("table_2")

                    ),

                    boxPlus(title = "Palabras únicas en cada archivo",
                            closable = F,
                            solidHeader = F,
                            collapsible = T,
                            status = "info",
                            # Create a new Row in the UI for selectInputs
                            fluidRow(
                                column(4,
                                       selectInput("pala",
                                                   "Palabras:",
                                                   c("todos",
                                                     unique(as.character(uniq_words_by_syllabus$Palabras))))
                                ),
                                column(4,
                                       selectInput("arc",
                                                   "Archivo:",
                                                   c("todos",
                                                     unique(as.character(uniq_words_by_syllabus$Archivo))))
                                ),
                            ),
                            # Create a new row for the table.
                            DT::dataTableOutput("table_3")

                    ),

                    boxPlus(title = "Número de palabras totales por ciclo",
                            closable = F,
                            solidHeader = F,
                            collapsible = T,
                            status = "info",
                            tableOutput("table_1")),

                    boxPlus(title = "Tabla de información",
                            width = 12,
                            closable = F,
                            solidHeader = F,
                            collapsible = T,
                            status = "info",
                            # Create a new Row in the UI for selectInputs
                            fluidRow(
                                column(4,
                                       selectInput("asig",
                                                   "Nombre:",
                                                   c("todos",
                                                     unique(as.character(subjects_info$`Nombre de la asignatura`))))
                                ),
                                column(4,
                                       selectInput("cla",
                                                   "Clave:",
                                                   c("todos",
                                                     unique(as.character(subjects_info$Clave))))
                                ),
                                column(4,
                                       selectInput("cic",
                                                   "Ciclo:",
                                                   c("todos",
                                                     unique(as.character(subjects_info$Ciclo))))
                                ),
                                column(4,
                                       selectInput("cred",
                                                   "Créditos:",
                                                   c("todos",
                                                     unique(as.character(subjects_info$Créditos))))
                                ),
                            ),
                            # Create a new row for the table.
                            DT::dataTableOutput("table_4"),

                            downloadButton('downloadDataCSV', 'Descargar CSV'),
                            downloadButton('downloadDataXLS', 'Descargar XLS'),
                            downloadButton('downloadDataDOC', 'Descargar DOC'),
                            downloadButton('downloadDataTXT', 'Descargar TXT'),
                            downloadButton('downloadDataHTML', 'Descargar HTML')

                    ),
                )
        ),

        tabItem(tabName = "data_2",
                h2("Base de Datos 2"),
                fluidRow(
                    boxPlus(title = "Tabla de información mensual",
                            width = 10,
                            closable = F,
                            solidHeader = F,
                            collapsible = T,
                            status = "info",
                            dataTableOutput("table_world")),

                    boxPlus(title = "Gráfico de información mensual",
                            width = 10,
                            closable = F,
                            solidHeader = F,
                            collapsible = T,
                            status = "info",
                            plotOutput("plot_1")),

                    boxPlus(title = "Tabla de defunciones positivas",
                            width = 11,
                            closable = F,
                            solidHeader = F,
                            collapsible = T,
                            status = "info",
                            dataTableOutput("table_death")),

                    boxPlus(title = "Gráfico de defunciones positivas",
                            width = 11,
                            closable = F,
                            solidHeader = F,
                            collapsible = T,
                            status = "info",
                            plotOutput("plot_2")),
                )
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

    output$table_2 <- DT::renderDataTable(DT::datatable({
        data <- most_freq_by_cycle
        if (input$pal != "todos") {
            data <- data[data$Palabras == input$pal,]
        }
        if (input$frec != "todos") {
            data <- data[data$Frecuencia == input$frec,]
        }
        if (input$cic != "todos") {
            data <- data[data$Ciclo == input$cic,]
        }
        data
    }))

    output$table_3 <- DT::renderDataTable(DT::datatable({
        data <- uniq_words_by_syllabus
        if (input$pala != "todos") {
            data <- data[data$Palabras == input$pala,]
        }
        if (input$arc != "todos") {
            data <- data[data$Archivo == input$arc,]
        }
        data
    }))

    output$table_4 <- DT::renderDataTable(DT::datatable({
        data <- subjects_info
        if (input$asig != "todos") {
            data <- data[data$`Nombre de la asignatura` == input$asig,]
        }
        if (input$cla != "todos") {
            data <- data[data$Clave == input$cla,]
        }
        if (input$cic != "todos") {
            data <- data[data$Ciclo == input$cic,]
        }
        if (input$cred != "todos") {
            data <- data[data$Créditos == input$cred,]
        }
        data
    }))

    output$downloadDataCSV <- downloadHandler(
        filename = function() {
            paste("dataset-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            write.csv(subjects_info, file)
        })

    output$downloadDataXLS <- downloadHandler(
        filename = function() {
            paste("dataset-", Sys.Date(), ".xls", sep="")
        },
        content = function(file) {
            write.csv(subjects_info, file)
        })

    output$downloadDataDOC <- downloadHandler(
        filename = function() {
            paste("dataset-", Sys.Date(), ".doc", sep="")
        },
        content = function(file) {
            write.csv(subjects_info, file)
        })

    output$downloadDataTXT <- downloadHandler(
        filename = function() {
            paste("dataset-", Sys.Date(), ".txt", sep="")
        },
        content = function(file) {
            write.csv(subjects_info, file)
        })

    output$downloadDataHTML <- downloadHandler(
        filename = function() {
            paste("dataset-", Sys.Date(), ".html", sep="")
        },
        content = function(file) {
            write.csv(subjects_info, file)
        })

    output$table_world <- renderDataTable(worldwide_info)

    output$plot_1 <- renderPlot({
        worldwide_info %>%
            pivot_longer(-1, names_to = "Regiones de la OMS", values_to = "cases") %>%
            ggplot(aes(x = `Días`, y = cases, color = `Regiones de la OMS`)) %>%  +
            geom_line() +
            scale_colour_manual(values = c("green", "red2", "purple", "blue2", "green4", "yellow3")) +
            labs(y = "Número de casos", title = "Casos acumulados de COVID-19 por SARS-CoV-2 por regiones de la OMS\n(Septiembre 2020)")
    })

    output$table_death <- renderDataTable(positive_deaths_info)

    output$plot_2 <- renderPlot({
        positive_deaths_info %>%
            pivot_longer(-1, names_to = "Entidades federativas", values_to = "cases") %>%
            ggplot(aes(x = `Días`, y = cases, color = `Entidades federativas`)) %>% +
            geom_line() +
            labs(y = "Defunciones positivas", title = "Defunciones positivas a COVID-19 por entidad federativa\n(Septiembre 2020)")
    })
}

# Run the application
shinyApp(ui = ui, server = server)
