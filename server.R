#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readxl)
library(writexl)
library(lubridate)
constants <- read_excel("constants.xlsx")

server <- function(input, output, session) {
  metaDataHeaders <- c("Sample Date", "External LabID", "Internal LabID", "Desc 1", "Desc 2", "Desc 3", "Desc 4")
  
  nutrData <- reactiveValues()
  
  saved_settings <- reactiveValues(
    nutrient_key = NULL,
    metadata_key = NULL
  )
  
  saved_metadata_values <- reactiveValues(
    Sample_Date = NULL,
    External_LabID = NULL,
    Internal_LabID = NULL,
    Desc_1 = NULL,
    Desc_2 = NULL,
    Desc_3 = NULL,
    Desc_4 = NULL,
    First_Nutrient_Listed = NULL
  )
  
  # Upload data ---------------------------------------------------------
  observeEvent(input$file, {
    req(input$file)
    print("File uploaded, reading data...")
    nutrData$raw <- read_csv(input$file$datapath, 
                             na = "not requested")
    print("Data read successfully!")
    
    # Render Raw Data Table
    output$rawTable <- renderDT({
      req(nutrData$raw)  
      datatable(
        nutrData$raw,
        options = list(
          scrollX = TRUE,
          columnDefs = list(list(
            targets = 7,
            render = JS(
              "function(data, type, row, meta) {",
              "return type === 'display' && data.length > 20 ?",
              "'<span title=\"' + data + '\">' + data.substr(0, 20) + '...</span>' : data;",
              "}")))
        ))
    })
    
  })
  
  # Save Uploaded Data
  observeEvent(input$save_upload, {
    req(nutrData$raw)
    updateTabItems(session, "sidebar", selected = "meta_data")
  })
  
  # Assign Metadata ------------------------------------------------------
  
  # DROPDOWN MENUS
  output$metadata_dropdowns <- renderUI({
    req(nutrData$raw)
    
    # Create dropdowns for each specific title
    dropdowns <- lapply(c(metaDataHeaders, "First Nutrient Listed"), function(title) {
      normalized_title <- gsub(" ", "_", title)
      
      selectInput(inputId = paste0("dropdown_", normalized_title),
                  label = title,
                  choices = colnames(nutrData$raw), 
                  selected = saved_metadata_values[[normalized_title]])  
    })
    
    # Return the list of dropdowns as UI elements
    do.call(tagList, dropdowns)
  })
  
  
  
  # METADATA TABLE
  observeEvent(input$check_metadata, {
    req(nutrData$raw) 
    
    selected_columns <- c(
      input$dropdown_Sample_Date,
      input$dropdown_External_LabID,
      input$dropdown_Internal_LabID,
      input$dropdown_Desc_1,
      input$dropdown_Desc_2,
      input$dropdown_Desc_3,
      input$dropdown_Desc_4
    )
    print(selected_columns)
    
    # Store the selected columns in reactiveValues
    saved_metadata_values$Sample_Date <- input$dropdown_Sample_Date
    saved_metadata_values$External_LabID <- input$dropdown_External_LabID
    saved_metadata_values$Internal_LabID <- input$dropdown_Internal_LabID
    saved_metadata_values$Desc_1 <- input$dropdown_Desc_1
    saved_metadata_values$Desc_2 <- input$dropdown_Desc_2
    saved_metadata_values$Desc_3 <- input$dropdown_Desc_3
    saved_metadata_values$Desc_4 <- input$dropdown_Desc_4
    saved_metadata_values$First_Nutrient_Listed <- input$dropdown_First_Nutrient_Listed
    
    # Save Selected Columns
    nutrData$selected_columns <- c(selected_columns, input$dropdown_First_Nutrient_Listed)
    
    selected_metadata <- nutrData$raw[, selected_columns, drop = FALSE]
    
    # Rename the selected columns to match the titles
    colnames(selected_metadata) <- metaDataHeaders
    
    
    # Pull out Nutrient Only Data: Find the starting index of `1st Nutrient Listed`
    nutrient_start <- which(colnames(nutrData$raw) == input$dropdown_First_Nutrient_Listed)
    # Get the data from the `1st Nutrient Listed` column to the last column
    nutrient_data_raw <- nutrData$raw[, nutrient_start:ncol(nutrData$raw)]
    
    # Save reordered data
    nutrData$metadata <- cbind(selected_metadata, nutrient_data_raw)
    
    # Display the updated data table
    output$metadataTable <- renderDT({
      datatable(nutrData$metadata, options = list(
        scrollX = TRUE,
        pageLength = 5, 
        columnDefs = list(list(
          targets = c(3,4,5,6),
          render = JS(
            "function(data, type, row, meta) {",
            "return type === 'display' && data.length > 20 ?",
            "'<span title=\"' + data + '\">' + data.substr(0, 20) + '...</span>' : data;",
            "}")))))
    })
  })
  
  
  # NAVIGATE TO NUTRIENT NAMES
  observeEvent(input$save_metadata, {
    req(nutrData$metadata)
    nutrData$pivot_data <- nutrData$metadata %>%
      pivot_longer(cols = saved_metadata_values$First_Nutrient_Listed:last_col(), names_to = "Nutrient", values_to = "value") %>%
      mutate(
        Value = as.numeric(value),
        `Sample Date` = parse_date(`Sample Date`, "%m/%d/%Y")
      ) %>%
      drop_na(value)
    updateTabItems(session, "sidebar", selected = "nutrient_names")
  })
  
  # Nutrient Names ---------------------------------------------------
  
  # NUTRIENT DROPDOWN MENU
  output$nutrientTable <- renderDT({
    req(nutrData$pivot_data)
    nutrient_names <-
      nutrData$pivot_data %>%
      distinct(Nutrient)
    
    
    if (!is.null(saved_settings$nutrient_key)) {
      nutrient_names <- saved_settings$nutrient_key %>%
        mutate(Nutrient = Selected_nutrient)
      }

    nutrData$nutrient_names <- nutrient_names

    # Create Dropdown menu and add it to each row
    nutrient_names$Nutrient_selector <- vapply(1:nrow(nutrient_names), function(i) {
      # Check if there is a match for the nutrient in constants$Nutrient
      selected_value <- ifelse(nutrient_names$Nutrient[i] %in% constants$Nutrient, 
                               nutrient_names$Nutrient[i], 
                               constants$Nutrient[1])  # Default to the first option if no match
      
      # Create the dropdown with the selected value
      as.character(
        selectInput(
          paste0("sel", i), 
          label = NULL, 
          choices = constants$Nutrient, 
          selected = selected_value,  # Set the default selected value
          selectize = FALSE
        )
      )
    }, character(1))


    datatable(
      nutrient_names,
      escape = FALSE,
      select = "none",
      options = list(
        pageLength = 50,
        columnDefs = list(
          list(targets = "_all", className = "dt-center"),
          list(targets = ncol(nutrient_names), width = "100px")
        ),
        preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
        drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
      )
    )
  })


 

   # SAVE NUTRIENT NAMES AND NAV TO UNIT CONVERSIONS
   observeEvent(input$save_nutrients, {
     req(nutrData$nutrient_names)
     # Retrieve selected values
     selected_nutrients <- sapply(1:nrow(nutrData$nutrient_names), function(i) {
       input[[paste0("sel", i)]]
     })
     
     # Consolidate values into a nutrient name key
     saved_settings$nutrient_key <- nutrData$nutrient_names %>%
       mutate(Selected_nutrient = selected_nutrients)


     # Apply selected values to raw data
     nutrData$preconversion_data <- as.data.frame(nutrData$pivot_data) %>%
       left_join(saved_settings$nutrient_key, by = "Nutrient") %>%
       mutate(Selected_nutrient = ifelse(is.na(Selected_nutrient), "No match", Selected_nutrient)) %>%
       filter(Selected_nutrient != "IGNORE")
     
     # Setup conversion table data
     nutrData$conversion_table_data <- nutrData$preconversion_data %>%
       distinct(Nutrient, .keep_all = TRUE) %>%
       select(Nutrient, `Desc 4`, Value) %>%
       left_join(constants, by = "Nutrient") %>%
       mutate(
         `Expected Unit` = Units,
         `Conversion Multiplier` = NA,    
         `Converted Value` = NA                  
       ) %>%
       select(Nutrient, `Desc 4`, Value, `Expected Unit`, 
              `Conversion Multiplier`, `Converted Value`)
     
     
     updateTabItems(session, "sidebar", selected = "unit_conversions")
   })
   
   # Unit Conversion ---------------------------------------------------
   
   output$conversionTable <- renderDT({
     req(nutrData$conversion_table_data)
     
     datatable(
       nutrData$conversion_table_data,
       editable = list(target = "cell", columns = c(5)), 
       options = list(pageLength = 10),
     )
   })
   
   observeEvent(input$conversionTable_cell_edit, {
     convertData <- input$conversionTable_cell_edit
     
     if (convertData$col == 4) {  
       
       row <- convertData$row + 1   
       new_multiplier <- as.numeric(convertData$value)
       
       # Update the conversion_table_data
       nutrData$conversion_table_data[row, "Conversion Multiplier"] <<- new_multiplier
       nutrData$conversion_table_data[row, "Converted Value"] <<- 
         nutrData$conversion_table_data[row, "Value"] * new_multiplier
       
       # Replace data in the table
       replaceData(
         proxy = dataTableProxy('conversionTable'),
         data = nutrData$conversion_table_data,
         resetPaging = FALSE
       )
     }
   })
  
  
  # Download -------------------------------------------------------
  output$download <- downloadHandler(
    filename = function() {
      paste0(tools::file_path_sans_ext(input$file$name), "-ready4ZDN", ".xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(as_fed(), file)
    }
  )
}