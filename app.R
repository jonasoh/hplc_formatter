library(snakecase)
library(janitor)
library(readr)
library(tidyr)
library(dplyr)
library(shiny)
library(zoo)

ui <- fluidPage(
    titlePanel("HPLC report reformatter"),

    sidebarLayout(
        sidebarPanel(
          htmlOutput("intro"),
          hr(),
          fileInput("file1", "Choose GLPrprt.txt File",
                    multiple = FALSE,
                    accept = c("text/plain",
                               ".txt")),
          numericInput("dilutionFactor", 'Dilution factor:', value=1, min=0.0001),
          downloadButton("downloadData", "Download reformatted data")
        ),
        mainPanel(
          tableOutput("contents")
        )
    )
)

server <- function(input, output) {
  output$intro <- renderUI({ HTML(paste(c("This application reformats compound summary files from Agilent Chemstation.",
                                       "Data will be transformed from long to wide, and a dilution factor applied to all samples",
                                       "Upload your GLPrprt.txt file below to begin."), collapse='<br><br>')) })

  output$contents <- renderTable({
    req(input$file1)
    tryCatch(
      {
        df <- format_hplc_file(input$file1$datapath, input$dilutionFactor)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      })
    df
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      sub('.txt$', '-wide.csv', input$file1$name, input$dilutionFactor)
    },
    content = function(file) {
      write.csv2(format_hplc_file(input$file1$datapath, input$dilutionFactor), file, 
                 row.names=F, na='')
    }
  )
}

format_hplc_file <- function(fname, dil_fac) {
  # XXX: UTF-16 file encoding seems to work in most cases... figure out what to do when it doesn't
  data <- read.fwf(fname, 
                   widths=c(17, -(28-17), -(37-28), -(48-37), -(56-48), 68-56, 100-67),
                   fileEncoding='UTF-16')
  names(data) <- c('SampleName', 'Amount', 'Compound')

  # the header section comprises a variable number of rows,
  # so look for the header indicatorto figure out what to strip
  skip <- which(data$SampleName == '----------------|')
  data <- data[skip+1:nrow(data),]
  
  data$SampleName[grep('^ +$', data$SampleName)] <- NA

  # fill down sample names
  data$SampleName <- na.locf(data$SampleName)
  
  data$Amount <- as.numeric(data$Amount)
  
  # strip unnecessary whitespace
  data$Compound <- sub(' *$', '', data$Compound)
  data$SampleName <- sub(' *$', '', data$SampleName)
  
  data <- data[data$Compound != '-',]
  
  # apply dilution factor
  if (is.numeric(dil_fac) & dil_fac > 0) {
    data$Amount <- data$Amount * dil_fac
  }
  
  # clean up and convert to wide format
  data <- data %>% remove_constant %>% remove_empty
  data <- data %>% pivot_wider(names_from = Compound, values_from=Amount)
  data <- data[!is.na(data$SampleName),]
  names(data) <- to_any_case(names(data), case='title', sep_in=' ')
  
  data
}

# Run the application 
shinyApp(ui = ui, server = server)
