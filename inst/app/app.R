## app.R ##
library(shiny)
library(shinyAce)
library(DiagrammeR)
library(DiagrammeRsvg)
library(datamodelr)
library(rmdoc)

#source("app_pages_output.R")

#source("R/dm.R")
#source("R/dm_module_graph.R")

default_diagram <- ""

default_database <- "BasicDev"


control_definition <-
  tabPanel(
    title = "Definition",
    aceEditor("ace", mode = "yaml", theme = "clouds_midnight", value="", height = 550, fontSize = 14),
    tag("script", '
        var editor = ace.edit("ace");
        editor.getSession().setTabSize(2)
        editor.getSession().setUseSoftTabs(true);
        editor.setBehavioursEnabled( true );
        '),
    selectInput(
      inputId = "database_id", label = NULL,
      choices = c(
        BasicDev = "BasicDev",
        BasicDevTest = "BasicDevTest",
        BasicStg24 = "BasicStg24",
        Test = "Test"
      ),
      selected = "DevBasic" ),
    verbatimTextOutput(outputId = "yml_result")
    )

control_display <-
  tabPanel(title = "Display",
           inputPanel(

             selectInput("view_type", label = "Show columns",  choices = list(
               "All columns" = "all",
               "Keys only" = "keys_only",
               "No columns" = "title_only"), selected = "RL"),
             selectInput("rankdir", label = "Graph direction",  choices = list(
               "Right-left" = "RL",
               "Bottom-top" = "BT",
               "Left-right" = "LR",
               "Top-bottom" = "TB"), selected = "RL"),
             checkboxInput("outside_ref", label = "Display external references", value = FALSE),
             checkboxInput("show_types", label = "Show types",  value = FALSE),
             checkboxInput("show_descriptions", label = "Show descriptions",  value = FALSE),
             selectInput("max_cols", label = "Maximum columns displayed",  choices = list(
               "30" = 30,
               "60" = 60,
               "No limit" = -1), selected = 30)
           )
  )

control_download <-
  tabPanel(
    title = "Download",
    inputPanel(
      downloadButton(outputId = 'downloadDataHTML', label = 'Download HTML'),
      downloadButton(outputId = 'downloadDataGV', label = 'Download GV')
    )
    # ,
    # inputPanel(h3("Save diagram"),
    #            textInput(inputId = "diagram_save_as", "Diagram name:"),
    #            actionButton(inputId = "save_diagram", label = "Save", icon = icon("archive") ),
    #            textOutput(outputId = "save_out")
    # )
  )


controls <-
  absolutePanel(
    id = "controls", class = "panel panel-default", fixed = TRUE,
    draggable = TRUE, top = 60, left = 1, right = "auto", bottom = "auto",
    width = 400, height = "auto",

    tabsetPanel(width = "100%", selected = "Display",
                control_definition,
                control_display,
                control_download
    )
  )


ui <- shinyUI(
  navbarPage(
    title = "datamodelR",
    id = "nav",
#    tabPanel("List",
#            htmlOutput(outputId = "list")
#    ),
    tabPanel("Diagram",
             div(
               class = "outer",
               tags$head(includeCSS("styles.css")),
               grVizOutput('diagram', height = 700),
               controls
             )
    ),
    tabPanel("Module",
             absolutePanel(width = 200,
                           selectInput(
                             inputId = "ref_dir",
                             label = "Reference tables",
                             choices = c("from", "to", "all")
                           )
             ),
             visNetwork::visNetworkOutput(outputId = "module_graph", height = "700px")
    )
# ,
#     tabPanel("Databases",
#              tags$head(tags$script(src = "message-handler.js")),
#              htmlOutput(outputId = "database_info"),
#              actionButton(inputId = "save_database", "Refresh Database"),
#              textOutput(outputId = "database_ok")
#     )

  )
)



server <- function(input, output, session) {
  if(exists(".rmdoc_db_meta") && !is.null(.rmdoc_db_meta)) {
    db_meta <- get(".rmdoc_db_meta")
  } else {
    db_meta <- get_db_meta(database = default_database)
  }
  # main diagram rendering

  output$diagram <- renderGrViz({
    display_options <- graph_display_options()
    display_options$rankdir <- input$rankdir
    display_options$view_type <- input$view_type
    display_options$outside_ref <- input$outside_ref
    display_options$show_types <- input$show_types
    display_options$show_descriptions <- input$show_descriptions
    display_options$max_cols <- input$max_cols
    dot_gv <-
      get_graph_from_yaml(
        input$ace,
        display_options = display_options,
        db_meta = db_meta
      )
    if(dot_gv != "") {
      grViz(dot_gv)
    }
  })


  # download
  output$downloadDataHTML <- downloadHandler(
    filename = "export.HTML",
    content = function(file) {
      display_options <- graph_display_options()
      display_options$rankdir <- input$rankdir
      display_options$view_type <- input$view_type
      display_options$outside_ref <- input$outside_ref
      display_options$show_types <- input$show_types
      display_options$show_descriptions <- input$show_descriptions
      display_options$max_cols <- input$max_cols
      dot_gv <-
        get_graph_from_yaml(
          input$ace,
          display_options = display_options,
          db_meta = db_meta
        )
      if(nchar(dot_gv) == 0) {
        stop("No data returned by get_graph_from_yaml")
      }
      svg <- DiagrammeRsvg::export_svg(grViz(dot_gv))
      html_s <- paste("<html><head><title>datamodelr</title></head><body>", svg, "</body></html>")
      writeChar(object = html_s, file, eos = NULL)
    },
    contentType = "text/html"
  )

  output$downloadDataGV <- downloadHandler(
    filename = "export.gv",
    content = function(file) {
      display_options <- graph_display_options()
      display_options$rankdir <- input$rankdir
      display_options$view_type <- input$view_type
      display_options$outside_ref <- input$outside_ref
      display_options$show_types <- input$show_types
      display_options$show_descriptions <- input$show_descriptions
      display_options$max_cols <- input$max_cols
      dot_gv <-
        get_graph_from_yaml(
          input$ace,
          display_options = display_options,
          db_meta = db_meta
        )

      writeChar(object = dot_gv, file, eos = NULL)
    },
    contentType = "text/gv"
  )

  # yaml parsing errors
  output$yml_result <- renderPrint({
    tmp_yml <- yaml::yaml.load(input$ace)
    #tab_list <- .get_extract_list(tmp_yml)
    "OK"
  })

  # update editor when url parameter diagram change
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['diagram']])) {
      diagram_id <- query[['diagram']]
      file_name <- paste0(diagram_id, ".yml")
      if(file.exists(file_name)) {
        str_definition <- paste(c(readLines(file_name),""), collapse = "\n")
        updateAceEditor(session, "ace", value = str_definition)
        updateTabsetPanel(session, inputId = "nav", selected = "Diagram")
      } else {
        updateAceEditor(session, "ace", value = default_diagram)
        updateTabsetPanel(session, inputId = "nav", selected = "Diagram")
      }
    }
    # update datamodelr from github
    if(!is.null(query[['install']])) {
      pkg <- query[['install']]
      if(pkg == "datamodelr") {
        devtools::install_github("bergant/datamodelr")
      }
    }
    if(!is.null(query[['module']])) {
      updateTabsetPanel(session, inputId = "nav", selected = "Module")
    }
  })
  output$list <- renderUI({
    get_list_output()
  })

  output$database_info <- renderUI({
    check_if_changed <- input$save_database

    dbinfo <- get_database_output(input$database_id)
    div(
      h3("Database Info"),
      p("Database:", dbinfo[["Database"]]),
      p("Last refresh:", dbinfo[["Modified"]]),
      p("Time from last refresh:", dbinfo[["Age"]])
    )
  })


  output$module_graph <- visNetwork::renderVisNetwork({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['module']])) {
      module <- query[['module']]
      create_module_graph_db(input$database_id, path = paste0("diagrams/", module))
    } else {
      tmp_yml <- yaml::yaml.load(input$ace)
      tab_list <- unlist(.get_extract_list(tmp_yml))
      create_module_graph_db(
        database_name = input$database_id,
        module_tables = tab_list,
        ref_dir = input$ref_dir)
    }
  })

  # textInput(inputId = "diagram_save_as", "Save diagram"),
  # actionButton(inputId = "save_diagram", label = "Save", icon = icon("archive") )
  # textOutput(inputId = "save_out", label = NULL)

  button_save <- eventReactive(input$save_diagram, {
    file_name <- input$diagram_save_as
    if(file_name == "") stop("No diagram name")
    if(!dir.exists("diagrams/drafts")) dir.create("diagrams/drafts")
    file_name <- gsub("\\.\\./", "", file_name)
    file_name <- gsub("\\.\\.\\\\", "", file_name)
    file_name <- paste0("diagrams/drafts/", file_name, ".yml")
    cat(input$ace, file = file_name)
    file_name
  })
  output$save_out <- renderText({
    paste("Saved as:", button_save())
  })

  database_saved <- eventReactive(input$save_database, {
    .save_db_model_reluctant(input$database_id, 60)
  })

  output$database_ok <- renderText({
    ret <- "..."
    if(database_saved()) {
      ret <- "OK"
    } else {
      ret <- "please try again a little bit later."
    }
    ret
  })
}

shinyApp(ui, server)
