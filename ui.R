# Run this before publishing:
# .rs.files.restoreBindings()

library(shiny)
library(DT)
library(bslib)
library(shinydashboard)
library(shiny.fluent)

# UPLOAD DATA TAB
upload_data_content <- fluidPage(
  
  #Styling
  tags$style(HTML("
  .input-section {
    display: flex;
    flex-direction: column; 
  }
 

  .inputNotice {
    margin-top: -20px;
    color: #555;
  }

  .h3 {
    color: rgb(56,126,63);
    font-weight: 600;
  }
  ")),
  
  #Content
      h3(class = "h3", "Upload and Review Raw Data"),
      div(class = "input-section",
          div(class = "fileInput", fileInput("file", "", buttonLabel = "Upload CSV File...", accept = '.csv')),
          div(class = "inputNotice", "Important: Dairy One files must be in the '-29.csv' format.")
          ),
  
  div(class = 'table', DTOutput('rawTable')),
  actionButton('save_upload', 'Save and Continue')
)

#META DATA TAB
metadata_content <- fluidPage(
  
  h3("Select Header Meta Data"),
  fluidRow(
    
    column(2, div(class= 'metadata-dropdown', uiOutput("metadata_dropdowns"))),
    column(10, div(class = 'table', DTOutput("metadataTable")))
    ),
  
  actionButton('check_metadata', 'Check Selection'),
  actionButton('save_metadata', 'Save and Continue'),
  )

#NUTRIENT NAMES TAB
nutrient_names_content <- fluidPage(

  h3("Align Nutrient Names with ZooDiets Standards"),
  div(class = 'table', DTOutput('nutrientTable')),
  actionButton('save_nutrients', 'Save and Continue'),
  
  )

#UNIT CONVERSION TAB
unit_conversion_content <- fluidPage(
  
  h3("Provide Conversion Values for All Units"),
  div(class = 'table', DTOutput('conversionTable')),
  actionButton('save_units', 'Save and Continue'),
  
)


ui_nav <- dashboardPage(
  dashboardHeader(
    title = "Convert Zoo Diet Data"),
  
  dashboardSidebar(

    sidebarMenu(
      id = "sidebar",
      
      menuItem("Upload Data", tabName = "upload_data", icon = icon("upload")),
      menuItem("Meta Data", tabName = "meta_data", icon = icon("cogs")),
      menuItem("Nutrient Names", tabName = "nutrient_names", icon = icon("leaf")),
      menuItem("Unit Conversions", tabName = "unit_conversions", icon = icon("exchange-alt")),
      menuItem("Review and Download", tabName = "review_download", icon = icon("download")),
      menuItem("Help", icon = icon("question-circle"),
               menuSubItem("Conversion Example", tabName = "conversion_example"),
               menuSubItem("Zoo Diet Docs", tabName = "zoo_docs"))
    )
  ),
  
  dashboardBody(
    includeCSS("www/styles.css"),
    
    tabItems(
      tabItem(tabName = "upload_data", upload_data_content),
      tabItem(tabName = "meta_data", metadata_content),
      tabItem(tabName = "nutrient_names", nutrient_names_content),
      tabItem(tabName = "unit_conversions", unit_conversion_content),
      tabItem(tabName = "review_download", h3("Review Data Conversion")),
      tabItem(tabName = "conversion_example", h3("Data Conversion Example")),
      tabItem(tabName = "zoo_docs", h3(a("Zoo Diet Docs", href = "https://zoodiets.com/user-guide/", target = "_blank")))
    )
  )
)



  


# ui_clean <- sidebarLayout(
#   sidebarPanel(
#     checkboxInput("DAK", "Does this dataset contain zoo sample ID numbers (within the D1 descriptions)?", value = FALSE),
# 
#   ),
#   mainPanel(
#     h3("Cleaned data ready for ZDN"),
#     tableOutput("preview2")
#   )
# )



ui_download <- fluidRow(
  column(width = 12, downloadButton("download", class = "btn-block",  style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
)

ui <- fluidPage(
  # titlePanel(HTML(paste0("Convert ", "<b>","Dairy One","</b> ", " to ", "<b>","Zoo Diets NaviGator","</b>"))),
  ui_nav,
  # ui_upload,
  # ui_clean,
  # ui_download
)