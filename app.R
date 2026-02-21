
#    https://shiny.posit.co/

library(shiny)
library(sf)
library(tidyverse)
library(tidygeocoder)
library(sf)
library(ggplot2)
library(leaflet)
library(osmdata)
library(dbscan)


ui <- fluidPage(
  titlePanel("Precinct/District Auditor"),
  wellPanel(
    h4("Instructions:"),
    p("1. Upload address CSV file with columns titled \"street\", \"city\", \"state\", and \"zip\". The file should contain just the addresses assigned to the precinct/district you are auditing. Do NOT include voter information."),
    fileInput("file", "Upload Address File CSV",accept=".csv"),
    p("2. Upload a boundary file (.geojson, .shp, or .json) containing precinct or district shapes."),
    fileInput("file2", "Upload GeoJSON File",accept=c(".geojson",".json")),
    fileInput("file3", "Upload All Shapefile Components",multiple=TRUE,accept = c(
      ".shp", ".shx", ".dbf", ".prj", ".cpg")),
    p("3. Select the column that contains precinct/district names."),
    selectInput(
      "column",
      "Select Column:",
      choices = NULL
    ),
    p("4. Select the specific precinct/district you want to analyze."),
    selectInput(
      "value",
      "Select Precinct/District:",
      choices = NULL
    ),
    p("5. Click 'Generate Report' to download the results."),   downloadButton("report", "Generate Report"),
    mainPanel(
      verbatimTextOutput("selected")
      
  ),
  

  
  )
)

server <- function(input, output, session) {
  
  addresslist_path <- reactiveVal(NULL)
  observeEvent(input$file, {
    addresslist_path(input$file$datapath)
  })
  
  geographyfile_path <- reactiveVal(NULL)
  observeEvent(input$file2, {
    geographyfile_path(input$file2$datapath)
  })

  geographyfile_path <- reactive({
    req(input$file3)
    
    # temp dir for reconstructed shapefile
    shp_dir <- tempfile()
    dir.create(shp_dir)
    
    # find the .shp filename (original name)
    shp_name <- input$file3$name[grepl("\\.shp$", input$file3$name)]
    base <- tools::file_path_sans_ext(shp_name)
    
    # copy + rename all components to same basename
    for (i in seq_len(nrow(input$file3))) {
      ext <- tools::file_ext(input$file3$name[i])
      file.copy(
        input$file3$datapath[i],
        file.path(shp_dir, paste0(base, ".", ext)),
        overwrite = TRUE
      )
    }
    
    file.path(shp_dir, paste0(base, ".shp"))
  })
  
  
  observeEvent(geographyfile_path(), {
    req(geographyfile_path())
    geo <- st_read(geographyfile_path(), quiet = TRUE)
    updateSelectInput(
      session,
      "column",
      choices = sort(names(st_drop_geometry(geo)))
    )
  })
  
  observeEvent(input$column, {
    req(geographyfile_path(), input$column)
    geo <- st_read(geographyfile_path(), quiet = TRUE)
    values <- unique(geo[[input$column]])
    updateSelectInput(
      session,
      "value",
      choices = sort(values)
    )
  })
  
  output$report <- downloadHandler(
    filename = function() {
      paste0("Report_", input$value, ".html")
    },
    content = function(file) {
      req(addresslist_path(), geographyfile_path(), input$column, input$value)
      rmarkdown::render(
        input = "testmarkdowncopy.Rmd",
        output_file = file,
        params = list(
          geographyfile_path = geographyfile_path(),
          addresslist_path = addresslist_path(),
          filter_column = input$column,
          filter_value = input$value
        ),
        envir = new.env(parent = globalenv())
      )
    }
  )
}
# Run the application 
shinyApp(ui = ui, server = server)
