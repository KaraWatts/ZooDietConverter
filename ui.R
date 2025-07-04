# Run this before publishing:
# .rs.files.restoreBindings()

library(shiny)
library(DT)
library(bslib)
library(shinydashboard)
library(shiny.fluent)
library(shinyjs)

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
  actionButton('save_metadata', 'Save and Continue')
)

#NUTRIENT NAMES TAB
nutrient_names_content <- fluidPage(

  h3("Align Nutrient Names with ZooDiets Standards"),
  div(class = 'table', DTOutput('nutrientTable')),
  actionButton('save_nutrients', 'Save and Continue'),
  
  )

#UNIT CONVERSION TAB
unit_conversion_content <- fluidPage(
  tags$style(HTML("
  .form-box {
    background-color: white;
    padding: 20px;
    border-radius: 8px;
    box-shadow: 0 2px 6px rgba(0, 0, 0, 0.15);
    margin-top: 20px;
    margin-bottom: 20px;
    width: 100%;
  }
    .form-row {
      margin-bottom: 10px;
      padding: 10px;
      border-bottom: 1px solid #e0e0e0;
    }
    .form-header {
      font-weight: bold;
      background-color: #f5f5f5;
      padding: 8px 0;
      border-bottom: 2px solid #ccc;
    }
  ")),

  h3("Provide Conversion Values for All Units"),
  # div(class = 'table', DTOutput('conversionTable')),
  # Table headers
  div(class="form-box",
  fluidRow(
    column(2, div(class = "form-header", "Nutrient")),
    column(2, div(class = "form-header", "Description")),
    column(2, div(class = "form-header", "Sample")),
    column(2, div(class = "form-header", "Convert to")),
    # column(2, div(class = "form-header", "Form")),
    column(2, div(class = "form-header", "Conversion Multiplier")),
    column(2, div(class = "form-header", "Converted Value"))
  ),
  
  # Dynamic form
  uiOutput("nutrient_forms"),
      
      ),

  actionButton('save_conversions', 'Save and Continue'),
  
)

#REVIEW AND DOWNLOAD TAB
review_content <- fluidPage(
  
  h3("Review Converted Data"),
  div(class = 'table', DTOutput('convertedTable')),
  downloadButton('download', 'Download Data'),
  actionButton('return', 'Upload A New File')
  
)


ui <- dashboardPage(

  
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
               # menuSubItem("Conversion Example", tabName = "conversion_example"),
               menuSubItem("Zoo Diet Docs", href = "https://zoodiets.com/user-guide/"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    includeCSS("www/styles.css"),
    
    # allow disabled tabs
    tags$script(HTML("
  Shiny.addCustomMessageHandler('disableTabs', function(disabledTabs) {
    $('.sidebar-menu li a').each(function() {
      const tabText = $(this).text().trim();
      if (disabledTabs.includes(tabText)) {
        $(this).addClass('disabled-tab');
        $(this).css('pointer-events', 'none');
        $(this).css('opacity', '0.5');
      } else {
        $(this).removeClass('disabled-tab');
        $(this).css('pointer-events', '');
        $(this).css('opacity', '');
      }
    });
  });
")),
    
    tabItems(
      tabItem(tabName = "upload_data", upload_data_content),
      tabItem(tabName = "meta_data", metadata_content),
      tabItem(tabName = "nutrient_names", nutrient_names_content),
      tabItem(tabName = "unit_conversions", unit_conversion_content),
      tabItem(tabName = "review_download", review_content)
      # tabItem(tabName = "conversion_example", h3("Data Conversion Example"))
    )
  )
)



  






