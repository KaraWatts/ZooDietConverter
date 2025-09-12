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
    background-color: #f8f9fa;
    padding: 15px;
    border-radius: 8px;
    margin: 20px 0;
    border-left: 4px solid rgb(56,126,63);
  }
 

  .inputNotice {
    margin-top: 10px;
    color: #666;
    font-size: 12px;
  }

  .h3 {
    color: rgb(56,126,63);
    font-weight: 600;
  }
  
  .settings-section {
    background-color: #f8f9fa;
    padding: 15px;
    border-radius: 8px;
    margin: 20px 0;
    border-left: 4px solid rgb(56,126,63);
  }
  ")),
  
  #Content
      h3(class = "h3", "Upload and Review Raw Data"),
      
      # Instructions section
      div(class = "input-section", style = "border-left: 4px solid #387E3F; background-color: #E6F0E7;",
          h4("About This Step", style = "color: #1C4420; margin-bottom: 10px;"),
          p("This is where you begin the data conversion process by uploading your raw nutrition data. The converter is specifically designed to work with Dairy One laboratory reports in the '-29.csv' format, which contains detailed nutritional analysis data. If you've used this converter before, you can also upload a previously saved settings file to automatically configure your metadata mappings and unit conversions, saving you time on similar datasets.")
      ),
      
      # Data upload section
      div(class = "input-section",
          h4("Upload Nutrition Data", style = "color: rgb(56,126,63); margin-bottom: 10px;"),
          fileInput("file", "", buttonLabel = "Upload CSV File...", accept = '.csv'),
          div(class = "inputNotice", "Important: Dairy One files must be in the '-29.csv' format.")
      ),
  
  # Settings upload section
  div(class = "settings-section",
      h4("Load Previous Settings (Optional)", style = "color: rgb(56,126,63); margin-bottom: 10px;"),
      fileInput("settings_file", "Upload Settings File", 
                buttonLabel = "Load Settings...", 
                accept = '.rds',
                placeholder = "No settings file selected"),
      div(style = "margin: 10px 0;",
          checkboxInput("auto_apply_settings", 
                       "Auto-apply settings and skip to download", 
                       value = FALSE,
                       width = "100%"),
          div(style = "font-size: 12px; color: #666; margin-top: 5px;", 
              "When checked, the app will automatically process your data using the uploaded settings and skip directly to the download page.")
      ),
      div(style = "font-size: 12px; color: #666;", 
          "Upload a previously saved settings file to quickly configure metadata and nutrient mappings.")
  ),
  
  div(class = 'table', DTOutput('rawTable')),
  actionButton('save_upload', 'Save and Continue')
)

#META DATA TAB
metadata_content <- fluidPage(
  
  h3("Select Header Meta Data"),
  
  # Instructions section
  div(style = "background-color: #E6F0E7; padding: 15px; border-radius: 8px; margin: 20px 0; border-left: 4px solid #387E3F;",
      h4("About This Step", style = "color: #1C4420; margin-bottom: 10px;"),
      p("Metadata mapping is crucial for organizing your nutrition data correctly within the Zoo Diet NaviGator system. Here you'll identify which columns in your uploaded file contain essential information like food names, diet identifiers, animal names, and dates. This mapping ensures that your converted data maintains proper structure and can be easily imported into Zoo Diet NaviGator with all the necessary context intact."),
      p(style = "margin-top: 10px; font-weight: bold; color: #1C4420;", "Fields marked with * are required.")
  ),
  
  fluidRow(
    
    column(2, div(class= 'metadata-dropdown', uiOutput("metadata_dropdowns"))),
    column(10, div(class = 'table', DTOutput("metadataTable")))
    ),
  
  actionButton('check_metadata', 'Check Selection'),
  actionButton('save_metadata', 'Save and Continue')
)

#NUTRIENT NAMES TAB
nutrient_names_content <- fluidPage(

  h3("Align Nutrient Names with Zoo Diet NaviGator Standards"),
  
  # Instructions section
  div(style = "background-color: #E6F0E7; padding: 15px; border-radius: 8px; margin: 20px 0; border-left: 4px solid #387E3F;",
      h4("About This Step", style = "color: #1C4420; margin-bottom: 10px;"),
      p("Standardizing nutrient names is essential for compatibility with the Zoo Diet NaviGator database. Different laboratories and data sources often use varying terminology for the same nutrients (e.g., 'Crude Protein' vs 'CP' vs 'Protein'). This alignment process ensures that your data will integrate seamlessly with Zoo Diet NaviGator' standardized nutrient database, allowing for accurate nutritional calculations and comparisons across different food items and diets.")
  ),
  
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

  h3("Unit Conversions & Data Format"),
  
  # Instructions section
  div(style = "background-color: #E6F0E7; padding: 15px; border-radius: 8px; margin: 20px 0; border-left: 4px solid #387E3F;",
      h4("About This Step", style = "color: #1C4420; margin-bottom: 10px;"),
      p("Unit conversion ensures your nutritional data is expressed in the correct format for Zoo Diet NaviGator. Laboratory reports may present nutrients in various units (percentages, grams per kilogram, milligrams per gram, etc.), but Zoo Diet NaviGator requires specific standardized units for accurate diet formulation and analysis. The converter automatically applies the conversion factors you specify to ensure unit standardization for Zoo Diet NaviGator.")
  ),
  
  # Table headers
  div(class="form-box",
  fluidRow(
    column(2, div(class = "form-header", "Nutrient")),
    column(2, div(class = "form-header", "Description")),
    column(2, div(class = "form-header", "Sample Value")),
    column(2, div(class = "form-header", "Convert to")),
    # column(2, div(class = "form-header", "Form")),
    column(2, div(class = "form-header", "Conversion Multiplier")),
    column(2, div(class = "form-header", "Converted Value"))
  ),
  
  # Dynamic form
  uiOutput("nutrient_forms"),

    
      
      ),

  # Add the conversion method selector
  div(class = "form-box", 
      style = "margin-bottom: 10px;",
      uiOutput("conversion_method_selector")
  ),

  actionButton('save_conversions', 'Save and Continue'),
  
)

#REVIEW AND DOWNLOAD TAB
review_content <- fluidPage(
  
  h3("Review Converted Data"),
  
  # Instructions section
  div(style = "background-color: #E6F0E7; padding: 15px; border-radius: 8px; margin: 20px 0; border-left: 4px solid #387E3F;",
      h4("About This Step", style = "color: #1C4420; margin-bottom: 10px;"),
      p("This final step presents your fully converted data, ready for import into Zoo Diet NaviGator. The conversion process has standardized your nutrient names, applied appropriate unit conversions, and organized your metadata according to Zoo Diet NaviGator requirements. You can download the converted file for immediate use, and optionally save your conversion settings as a template for processing similar datasets in the future, streamlining your workflow for routine data conversions.")
  ),
  
  div(class = 'table', DTOutput('convertedTable')),
  
  # Download buttons section
  div(style = "margin: 20px 0;",
      fluidRow(
        column(6, 
               downloadButton('download', 'Download Converted Data', 
                             class = "btn-primary", 
                             style = "margin-right: 10px; color: white;")
        ),
        column(6,
               downloadButton('download_settings', 'Save Settings for Reuse', 
                             class = "btn-success",
                             style = "background-color: rgb(56,126,63); border-color: rgb(56,126,63); color: white;")
        )
      )
  ),
  
  div(style = "margin-top: 20px;",
      actionButton('return', 'Upload A New File')
  )
  
)


ui <- dashboardPage(

  
  dashboardHeader(
    title = "Zoo Diet Navigator Data Converter"),
  
  dashboardSidebar(

    sidebarMenu(
      id = "sidebar",
      
      menuItem("Upload Data", tabName = "upload_data", icon = icon("upload")),
      menuItem("Meta Data", tabName = "meta_data", icon = icon("cogs")),
      menuItem("Nutrient Names", tabName = "nutrient_names", icon = icon("leaf")),
      menuItem("Unit Conversions", tabName = "unit_conversions", icon = icon("exchange-alt")),
      menuItem("Review and Download", tabName = "review_download", icon = icon("download")),
      menuItem("Help", icon = icon("question-circle"),
               menuSubItem("Zoo Diet Docs", href = "https://zoodiets.com/user-guide/"))
    ),
    div(
      class = "sidebar-version",
      "Version 1.0.0"
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    includeCSS("www/styles.css"),
    
    
    # Custom sidebar toggle button
    tags$div(
      class = "sidebar-toggle-custom",
      onclick = "toggleSidebar()",
      tags$i(class = "fa fa-chevron-left", id = "toggle-icon")
    ),
    
    # JavaScript for sidebar toggle functionality
    tags$script(HTML("
      let sidebarCollapsed = false;
      
      function toggleSidebar() {
        const body = document.body;
        const icon = document.getElementById('toggle-icon');
        
        if (sidebarCollapsed) {
          // Expand sidebar
          body.classList.remove('sidebar-collapsed');
          body.classList.add('sidebar-expanded');
          icon.className = 'fa fa-chevron-left';
          sidebarCollapsed = false;
        } else {
          // Collapse sidebar
          body.classList.remove('sidebar-expanded');
          body.classList.add('sidebar-collapsed');
          icon.className = 'fa fa-chevron-right';
          sidebarCollapsed = true;
        }
      }
      
      // Initialize sidebar as expanded
      document.addEventListener('DOMContentLoaded', function() {
        document.body.classList.add('sidebar-expanded');
      });
    ")),
    
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










