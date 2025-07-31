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
options(shiny.launch.browser = TRUE)


server <- function(input, output, session) {
  metaDataHeaders <- c("Sample Date", "External LabID", "Internal LabID", "Desc 1", "Desc 2", "Desc 3", "Desc 4")
  
  nutrData <- reactiveValues()
  
  saved_settings <- reactiveValues(
    nutrient_key = NULL,
    metadata_key = NULL,
    conversion_key = NULL,
    conversion_method = "No Conversion (Already As-Fed)"  # Default to no conversion
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
  
  step_complete <- reactiveValues(
    file_upload = FALSE,
    upload_data = FALSE,
    meta_data = FALSE,
    check_metadata = FALSE,
    nutrient_names = FALSE,
    unit_conversions = FALSE    )
  
  # Control Tab Navigation Access
  observe({

    disabled <- c()
    if (!step_complete$upload_data) disabled <- c(disabled, "Meta Data")
    if (!step_complete$meta_data) disabled <- c(disabled, "Nutrient Names")
    if (!step_complete$nutrient_names) disabled <- c(disabled, "Unit Conversions")
    if (!step_complete$unit_conversions) disabled <- c(disabled, "Review and Download")
    
    
    session$sendCustomMessage("disableTabs", disabled %||% character(0))
  })
  
  #Control Save Button Access
  observe({
    ## file must be uploaded first
    if (step_complete$file_upload) {
      shinyjs::enable("save_upload")
    } else {
      shinyjs::disable("save_upload")
    }
    ## Check metaData must be clicked first
    if (step_complete$check_metadata) {
      shinyjs::enable("save_metadata")
    } else {
      shinyjs::disable("save_metadata")
    }
    
  })

  
  # Upload data ---------------------------------------------------------
  
  # Settings file upload handler
  observeEvent(input$settings_file, {
    req(input$settings_file)
    
    tryCatch({
      # Read the settings file
      settings_data <- readRDS(input$settings_file$datapath)
      
      # Validate that it contains the expected structure
      if (!is.list(settings_data) || 
          !all(c("saved_settings", "saved_metadata_values") %in% names(settings_data))) {
        showNotification("Invalid settings file format.", type = "error")
        return()
      }
      
      # Load the settings
      saved_settings$nutrient_key <- settings_data$saved_settings$nutrient_key
      saved_settings$metadata_key <- settings_data$saved_settings$metadata_key
      saved_settings$conversion_key <- settings_data$saved_settings$conversion_key
      saved_settings$conversion_method <- settings_data$saved_settings$conversion_method %||% "No Conversion (Already As-Fed)"
      
      # Load metadata values
      saved_metadata_values$Sample_Date <- settings_data$saved_metadata_values$Sample_Date
      saved_metadata_values$External_LabID <- settings_data$saved_metadata_values$External_LabID
      saved_metadata_values$Internal_LabID <- settings_data$saved_metadata_values$Internal_LabID
      saved_metadata_values$Desc_1 <- settings_data$saved_metadata_values$Desc_1
      saved_metadata_values$Desc_2 <- settings_data$saved_metadata_values$Desc_2
      saved_metadata_values$Desc_3 <- settings_data$saved_metadata_values$Desc_3
      saved_metadata_values$Desc_4 <- settings_data$saved_metadata_values$Desc_4
      saved_metadata_values$First_Nutrient_Listed <- settings_data$saved_metadata_values$First_Nutrient_Listed
      
      showNotification("Settings loaded successfully! The app will use your saved configuration for metadata mapping, nutrient names, and conversion settings.", 
                      type = "message", duration = 5)
    }, error = function(e) {
      showNotification(paste("Error loading settings file:", e$message), type = "error")
    })
  })
  
  observeEvent(input$file, {
    req(input$file)
    print("File uploaded, reading data...")
    
    # Only surround the file reading and parsing with tryCatch
    tryCatch({
      # Check file extension
      file_ext <- tools::file_ext(input$file$name)
      if (tolower(file_ext) != "csv") {
        stop("Only CSV files are supported. Please upload a .csv file.")
      }
      
      nutrData$raw <- read_csv(input$file$datapath, 
                               col_types = cols(.default = "c")) 
      print("Data read successfully!")
      step_complete$file_upload <- TRUE
    }, error = function(e) {
      print(paste("Error reading file:", e$message))
      showNotification(paste("Error reading file:", e$message), type = "error")
      nutrData$raw <- NULL
      step_complete$file_upload <- FALSE
      return()
    })
    
    # Render Raw Data Table 
    output$rawTable <- renderDT({
      req(nutrData$raw)
      tryCatch({
        datatable(
          nutrData$raw,
          options = list(
            scrollX = TRUE,
            processing = TRUE,
            deferRender = TRUE,
            pageLength = 10,
            lengthMenu = c(5, 10, 25, 50),
            columnDefs = list(
              list(
                targets = "_all",
                render = JS(
                  "function(data, type, row, meta) {",
                  "if (type === 'display' && data && data.length > 20) {",
                  "return '<span title=\"' + data + '\">' + data.substr(0, 20) + '...</span>';",
                  "} else if (data === null || data === '') {",
                  "return '<span style=\"color: #999;\">-</span>';",
                  "} else {",
                  "return data;",
                  "}",
                  "}"
                )
              )
            )
          )
        )
      }, error = function(e) {
        showNotification(paste("Error rendering data table:", e$message), type = "error")
        NULL
      })
    })
  })
    

  
  # Save Uploaded Data
  observeEvent(input$save_upload, {
    req(nutrData$raw)
    step_complete$upload_data <- TRUE
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
    
    step_complete$check_metadata <- TRUE
    
    selected_columns <- c(
      input$dropdown_Sample_Date,
      input$dropdown_External_LabID,
      input$dropdown_Internal_LabID,
      input$dropdown_Desc_1,
      input$dropdown_Desc_2,
      input$dropdown_Desc_3,
      input$dropdown_Desc_4
    )

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
    saved_settings$metadate_key <- saved_metadata_values
    nutrData$pivot_data <- nutrData$metadata %>%
      pivot_longer(cols = saved_metadata_values$First_Nutrient_Listed:last_col(), names_to = "Nutrient", values_to = "value") %>%
      mutate(
        Value = parse_number(value),
        `Sample Date` = parse_date(`Sample Date`, "%m/%d/%Y")
      ) %>%
      drop_na(Value) %>%
      select(-value)
    
    step_complete$meta_data <- TRUE
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
      # keep all current nutrients, use saved mapping if available
      merged_key <- nutrient_names %>%
        left_join(saved_settings$nutrient_key, by = "Nutrient")
      # If Selected_nutrient is missing, default to Nutrient
      na_mask <- is.na(merged_key$Selected_nutrient)
      merged_key$Selected_nutrient[na_mask] <- merged_key$Nutrient[na_mask]
      selected_nutrients <- merged_key$Selected_nutrient
      # Update saved_settings$nutrient_key to include any new nutrients
      saved_settings$nutrient_key <- merged_key
    } else {
      selected_nutrients <- nutrient_names$Nutrient
      saved_settings$nutrient_key <- nutrient_names %>% mutate(Selected_nutrient = Nutrient)
    }

    nutrData$nutrient_names <- nutrient_names
    

    # Create Dropdown menu and add it to each row
    nutrient_names$Nutrient_selector <- vapply(seq_len(nrow(nutrient_names)), function(i) {
      current_nutrient <- nutrient_names$Nutrient[i]
      
      selected_value <- if (!is.null(selected_nutrients[i]) && selected_nutrients[i] != "") {
        selected_nutrients[i]
      } else if (nutrient_names$Nutrient[i] %in% constants$Nutrient) {
        nutrient_names$Nutrient[i]
      } else {
        constants$Nutrient[1]  # Fallback
      }
      
      as.character(
        selectInput(
          inputId = paste0("sel", i),
          label = NULL,
          choices = constants$Nutrient,
          selected = selected_value,
          selectize = FALSE
        )
      )
    }, character(1))
    
    table_nutrients <- if ("Selected_nutrient" %in% names(nutrient_names)) {
      nutrient_names %>%
        select(-Selected_nutrient)
    } else {
      nutrient_names
    }

    datatable(
      table_nutrients,
      escape = FALSE,
      select = "none",
      options = list(
        destroy = TRUE,
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
       mutate(Nutrient = ifelse(is.na(Selected_nutrient), "No match", Selected_nutrient)) %>%
       select(-Selected_nutrient) %>%
       filter(Nutrient != "IGNORE")
     
     

     # Setup conversion table data
     nutrData$conversion_table_data <- nutrData$preconversion_data %>%
       distinct(Nutrient, .keep_all = TRUE) %>%
       select(Nutrient, `Desc 4`, Value) %>%
       left_join(constants, by = "Nutrient") 
     

     #Set default conversion key or merge with existing
     if (is.null(saved_settings$conversion_key)) {
       data <- nutrData$conversion_table_data
       saved_settings$conversion_key <- data.frame(
         Nutrient = as.character(data$Nutrient),
         Multiplier = rep(1, nrow(data)),
         Unit = as.character(data$Unit),
         stringsAsFactors = FALSE
       )
     } else {
       # If we have a saved conversion_key, make sure it covers all current nutrients
       data <- nutrData$conversion_table_data
       current_nutrients <- as.character(data$Nutrient)
       saved_nutrients <- as.character(saved_settings$conversion_key$Nutrient)
       
       # Add any new nutrients that weren't in the saved settings
       new_nutrients <- setdiff(current_nutrients, saved_nutrients)
       if (length(new_nutrients) > 0) {
         new_data <- data[data$Nutrient %in% new_nutrients, ]
         new_key_rows <- data.frame(
           Nutrient = as.character(new_data$Nutrient),
           Multiplier = rep(1, nrow(new_data)),
           Unit = as.character(new_data$Unit),
           stringsAsFactors = FALSE
         )
         saved_settings$conversion_key <- rbind(saved_settings$conversion_key, new_key_rows)
       }
       
       # Remove any nutrients that are no longer present
       saved_settings$conversion_key <- saved_settings$conversion_key[
         saved_settings$conversion_key$Nutrient %in% current_nutrients, 
       ]
     }
     nutrData$conversion_table_data <- nutrData$conversion_table_data %>%
       left_join(saved_settings$conversion_key, by = "Nutrient") %>%
       mutate(
         Unit = ifelse(is.na(Unit.y), Unit.x, Unit.y),
         Multiplier = ifelse(is.na(Multiplier), 1, Multiplier),
         `Converted Value` = Value * Multiplier
       ) %>%
       select(-Unit.y, -Unit.x)
     
     step_complete$nutrient_names <- TRUE
     

     updateTabItems(session, "sidebar", selected = "unit_conversions")
   })
   
   # Unit Conversion ---------------------------------------------------
   
   # Global conversion method selector
   output$conversion_method_selector <- renderUI({
     div(
       tags$label("Data Format Conversion:", style = "font-weight: bold; margin-bottom: 15px; display: block;"),
       div(
         style = "display: flex; align-items: center; gap: 15px;",
         span("No Conversion (Already As-Fed)", style = "font-weight: 500;"),
         tags$label(
           class = "switch",
           tags$input(
             type = "checkbox",
             id = "global_conversion_method",
             checked = if(saved_settings$conversion_method == "Convert from Dry Matter to As-Fed") "checked" else NULL
           ),
           span(class = "slider")
         ),
         span("Convert from Dry Matter to As-Fed", style = "font-weight: 500;")
       )
     )
   })
     
     # Observer for global conversion method changes
   observeEvent(input$global_conversion_method, {
     if (is.null(input$global_conversion_method)) {
       saved_settings$conversion_method <- "No Conversion (Already As-Fed)"
     } else {
       saved_settings$conversion_method <- if(input$global_conversion_method) "Convert from Dry Matter to As-Fed" else "No Conversion (Already As-Fed)"
     }
     
     # Update conversion table with preview values when method changes
     if (!is.null(nutrData$conversion_table_data)) {
       updateConversionTablePreview()
     }
   })
   
   # Function to update conversion table with preview values
   updateConversionTablePreview <- function() {
     if (saved_settings$conversion_method == "Convert from Dry Matter to As-Fed") {
       # Show preview of as-fed values
       preview_data <- nutrData$preconversion_data %>%
         left_join(saved_settings$conversion_key, by = "Nutrient") %>%
         mutate(Value = as.numeric(Value) * Multiplier) %>%
         group_by(`Sample Date`, `External LabID`, `Internal LabID`, `Desc 1`, `Desc 2`, `Desc 3`, `Desc 4`) %>%
         mutate(
           dry_matter_value = ifelse(any(str_detect(tolower(Nutrient), "dry matter")), 
                                   Value[str_detect(tolower(Nutrient), "dry matter")][1], 
                                   100)
         ) %>%
         ungroup() %>%
         mutate(
           af_value = as.double(Value * dry_matter_value/100),
           preview_value = case_when(
             str_detect(tolower(Nutrient), "dry matter") ~ dry_matter_value,
             str_detect(tolower(Nutrient), "gross energy") ~ af_value/10,
             TRUE ~ af_value
           )
         ) %>%
         select(-Multiplier, -dry_matter_value, -af_value) %>%
         distinct(Nutrient, .keep_all = TRUE) %>%
         select(Nutrient, `Desc 4`, preview_value) %>%
         rename(Value = preview_value)
       
       # Update the conversion table data with preview values
       nutrData$conversion_table_data <- preview_data %>%
         left_join(constants, by = "Nutrient") %>%
         left_join(saved_settings$conversion_key, by = "Nutrient") %>%
         mutate(
           Unit = Unit.x,
           `Converted Value` = Value * Multiplier
         ) %>%
         select(-Unit.y, -Unit.x)
     } else {
       # Reset to original values (no conversion)
       original_data <- nutrData$preconversion_data %>%
         distinct(Nutrient, .keep_all = TRUE) %>%
         select(Nutrient, `Desc 4`, Value)
       
       nutrData$conversion_table_data <- original_data %>%
         left_join(constants, by = "Nutrient") %>%
         left_join(saved_settings$conversion_key, by = "Nutrient") %>%
         mutate(
           Unit = Unit.x,
           `Converted Value` = Value * Multiplier
         ) %>%
         select(-Unit.y, -Unit.x)
     }
   }
   
     # Generate form fields dynamically
     output$nutrient_forms <- renderUI({
       req(nutrData$conversion_table_data)
       # Also react to conversion method changes
       req(saved_settings$conversion_method)
       
       data <- nutrData$conversion_table_data
       
       lapply(seq_len(nrow(data)), function(i) {
         # Get the nutrient name for this row
         nutrient_name <- as.character(data$Nutrient[i])
         
         # Find the corresponding multiplier from the conversion_key
         conversion_row <- saved_settings$conversion_key[saved_settings$conversion_key$Nutrient == nutrient_name, ]
         current_multiplier <- if (nrow(conversion_row) > 0) conversion_row$Multiplier[1] else 1
         
         # Add label to show what the sample value represents
         sample_label <- if(saved_settings$conversion_method == "Convert from Dry Matter to As-Fed") {
           "Sample (As-Fed Preview)"
         } else {
           "Sample (Original)"
         }
         
         fluidRow(
           column(2, strong(data$Nutrient[i])),
           column(2, data$`Desc 4`[i]),
           column(2, div(
             style = "font-size: 12px; color: #666; margin-bottom: 2px;", 
             sample_label,
             br(),
             strong(sprintf("%.2f", data$Value[i]))
           )),
           column(2, data$Unit[i]),
           column(2, numericInput(
             inputId = paste0("mult_", i),
             label = NULL,
             value = current_multiplier,
             min = 0,
             step = 0.1
           )),
           column(2, verbatimTextOutput(paste0("converted_", i)))
         )
       })
     })
   

     
     # Compute converted values
     observe({
       req(nutrData$conversion_table_data)
       # Also react to conversion method changes
       req(saved_settings$conversion_method)
       
       data <- nutrData$conversion_table_data
       lapply(seq_len(nrow(data)), function(i) {
         output[[paste0("converted_", i)]] <- renderText({
           mult <- input[[paste0("mult_", i)]] %||% 0
           result <- data$Value[i] * mult
           sprintf("%.2f %s", result, data$Unit[i])
         })
       })
     })
   
   
   output$conversionTable <- renderDT({
     req(nutrData$conversion_table_data)
     
     datatable(
       nutrData$conversion_table_data,
       editable = list(target = "cell", columns = c(5)), 
       options = list(
         destroy = TRUE,
         pageLength = 10,
         columnDefs = list(
         list(targets = 5, className = "editable")  # Apply your CSS class to column index 5 (0-based)
          )
       ),
     )
   })

   
   observeEvent(input$conversionTable_cell_edit, {
     convertData <- input$conversionTable_cell_edit
     
     if (convertData$col == 4) {  # Conversion Multiplier column (index 4 = 5th column)
       row <- convertData$row + 1
       new_multiplier <- as.numeric(convertData$value)
       
       # Extract as characters
       nutrient <- as.character(nutrData$conversion_table_data[row, "Nutrient"])
       unit <- as.character(nutrData$conversion_table_data[row, "Unit"])
       
       # Update the table data
       nutrData$conversion_table_data[row, "Conversion Multiplier"] <<- new_multiplier
       nutrData$conversion_table_data[row, "Converted Value"] <<- 
         nutrData$conversion_table_data[row, "Value"] * new_multiplier
       
       # Update the saved_settings$conversion_key
       existing_index <- which(saved_settings$conversion_key$Nutrient == nutrient)
       
       if (length(existing_index) == 1) {
         saved_settings$conversion_key[existing_index, "Multiplier"] <- new_multiplier
         saved_settings$conversion_key[existing_index, "Unit"] <- unit
       } else {
         saved_settings$conversion_key <- rbind(
           saved_settings$conversion_key,
           data.frame(
             Nutrient = nutrient,
             Multiplier = new_multiplier,
             Unit = unit,
             stringsAsFactors = FALSE
           )
         )
       }
       
       # Refresh table view
       replaceData(
         proxy = dataTableProxy('conversionTable'),
         data = nutrData$conversion_table_data,
         resetPaging = FALSE
       )
     }
   })

   # SAVE CONVERSION MULTIPLIERS AND NAV TO REVIEW AND DOWNLOAD
   observeEvent(input$save_conversions, {
     req(nutrData$conversion_table_data)
     
     # Update conversion_key from the current table data multipliers
     for (i in seq_len(nrow(nutrData$conversion_table_data))) {
       input_id <- paste0("mult_", i)
       if (!is.null(input[[input_id]])) {
         # Get the nutrient name for this row
         nutrient_name <- as.character(nutrData$conversion_table_data$Nutrient[i])
         
         # Find and update the corresponding row in conversion_key
         conversion_index <- which(saved_settings$conversion_key$Nutrient == nutrient_name)
         if (length(conversion_index) > 0) {
           saved_settings$conversion_key$Multiplier[conversion_index[1]] <- input[[input_id]]
         }
       }
     }
     
     # Apply conversion based on selected method
     if (saved_settings$conversion_method == "Convert from Dry Matter to As-Fed") {
       # Convert from dry matter to as-fed format
       nutrData$converted_data <- nutrData$preconversion_data %>%
         left_join(saved_settings$conversion_key, by = "Nutrient") %>%
         mutate(
           # Apply unit conversion multipliers first
           Value = as.numeric(Value) * Multiplier
         ) %>%
         # Add dry matter values to each row for conversion
         group_by(`Sample Date`, `External LabID`, `Internal LabID`, `Desc 1`, `Desc 2`, `Desc 3`, `Desc 4`) %>%
         mutate(
           dry_matter_value = ifelse(any(str_detect(tolower(Nutrient), "dry matter")), 
                                   Value[str_detect(tolower(Nutrient), "dry matter")][1], 
                                   100)  # Default to 100 if no dry matter found
         ) %>%
         ungroup() %>%
         mutate(
           af_value = as.double(Value * dry_matter_value/100),
           final_value = case_when(
             str_detect(tolower(Nutrient), "dry matter") ~ dry_matter_value,
             str_detect(tolower(Nutrient), "gross energy") ~ af_value/10,
             TRUE ~ af_value
           ),
           Value = final_value
         ) %>%
         select(-Multiplier, -dry_matter_value, -af_value, -final_value) 
     } else {
       # No conversion needed - data is already in as-fed format
       nutrData$converted_data <- nutrData$preconversion_data %>%
         left_join(saved_settings$conversion_key, by = "Nutrient") %>%
         mutate(
           Value = as.numeric(Value) * Multiplier,
           Unit = Unit
         ) %>%
         select(-Multiplier) 
     }

      
     step_complete$unit_conversions <- TRUE
     
     # Show notification about saving settings
     showNotification("Data conversion complete! Don't forget to save your settings for future use.", 
                     type = "message", duration = 5)
     
     updateTabItems(session, "sidebar", selected = "review_download")
   })
   
   
   
  # Review --------------------------------------------------------
   output$convertedTable <- renderDT({
     req(nutrData$converted_data)
     
     datatable(
       nutrData$converted_data,
       options = list(
         pageLength = 10,
         scrollX = TRUE
       )
     )
   })
  # Download -------------------------------------------------------
  output$download <- downloadHandler(
    filename = function() {
      paste0(tools::file_path_sans_ext(input$file$name), "-ready4ZDN", ".xlsx")
    },
    content = function(file) {
      req(nutrData$converted_data)
      writexl::write_xlsx(as.data.frame(nutrData$converted_data), file)
    }
  )
  
  # Download settings handler
  output$download_settings <- downloadHandler(
    filename = function() {
      paste0("ZooDietConverter_Settings_", Sys.Date(), ".rds")
    },
    content = function(file) {
      # Ensure we capture the latest multiplier values before saving
      if (!is.null(nutrData$conversion_table_data) && !is.null(saved_settings$conversion_key)) {
        for (i in seq_len(nrow(nutrData$conversion_table_data))) {
          input_id <- paste0("mult_", i)
          if (!is.null(input[[input_id]])) {
            # Get the nutrient name for this row
            nutrient_name <- as.character(nutrData$conversion_table_data$Nutrient[i])
            
            # Find and update the corresponding row in conversion_key
            conversion_index <- which(saved_settings$conversion_key$Nutrient == nutrient_name)
            if (length(conversion_index) > 0) {
              saved_settings$conversion_key$Multiplier[conversion_index[1]] <- input[[input_id]]
            }
          }
        }
      }
      
      # Create a list with all relevant settings
      settings_to_save <- list(
        saved_settings = list(
          nutrient_key = saved_settings$nutrient_key,
          metadata_key = saved_settings$metadata_key,
          conversion_key = saved_settings$conversion_key,
          conversion_method = saved_settings$conversion_method
        ),
        saved_metadata_values = list(
          Sample_Date = saved_metadata_values$Sample_Date,
          External_LabID = saved_metadata_values$External_LabID,
          Internal_LabID = saved_metadata_values$Internal_LabID,
          Desc_1 = saved_metadata_values$Desc_1,
          Desc_2 = saved_metadata_values$Desc_2,
          Desc_3 = saved_metadata_values$Desc_3,
          Desc_4 = saved_metadata_values$Desc_4,
          First_Nutrient_Listed = saved_metadata_values$First_Nutrient_Listed
        ),
        # Add metadata for the settings file
        created_date = Sys.time(),
        app_version = "1.0",
        description = "ZooDiet Data Converter settings including metadata mappings, nutrient name mappings, unit conversion multipliers, and conversion method preferences."
      )
      
      # Save as RDS file
      saveRDS(settings_to_save, file)
    }
  )
   # Restart -------------------------------------------------------
  observeEvent(input$return, {
    step_complete$file_upload <- FALSE
    step_complete$upload_data <- FALSE
    step_complete$meta_data <- FALSE
    step_complete$check_metadata <- FALSE
    step_complete$nutrient_names <- FALSE
    step_complete$unit_conversions <- FALSE
    
    nutrData$raw <- NULL
    nutrData$metadata <- NULL
    nutrData$pivot_data <- NULL
    nutrData$nutrient_names <- NULL
    nutrData$preconversion_data <- NULL
    nutrData$conversion_table_data <- NULL
    nutrData$converted_data <- NULL
    nutrData$selected_columns <- NULL
  
    updateTabItems(session, "sidebar", selected = "upload_data")
  })
}