#-------------------#
# UI part of MTG-App #
#-------------------#

shinyUI(fluidPage(#theme = "bootstrap.css",
  
  titlePanel("MTG-App: collection comparison and deckbuilding"),
  
  navbarPage(
    "MGT-App",
    tabPanel("Before starting...",mainPanel(
      h1('Few words before starting:'),
      p("Welcome to the MTG-App Shiny application."),
      p("This application aims at providing an interactive way for importing and comparing MTG collections."),
      p("Users can also upload decklists and check what cards are missing from their collections."),
      p("It is possible to load more than one collection file."),
      p("At the moment, only collections exported via the DragonShield MTG app can be imported."),
    )),
    # collection import panel
    tabPanel("Import collection data",mainPanel(
      p('First, provide the path to a collection in a comma-separated format:'),
      tags$p(fileInput(inputId = 'collection_file',label = 'Please select a collection file'), align= "center"),
      DT::dataTableOutput("collectionContent")
      # radioButtons(inputId="plateTypeSelect", label="Which input format?",
      #              choices=c("AN","PM1","PM2A","other")),
      
     )),
    # decklist import panel
    tabPanel("Import Decklist data",mainPanel(
      p('Here, provide the path to a decklist in a MTGO format:'),
      tags$p(fileInput(inputId = 'decklist_file',label = 'Please select a decklist file'), align= "center"),
      DT::dataTableOutput("deckContent")
       # radioButtons(inputId="decklistTypeSelect", label="Which input format?",
       #              choices=c("MTGO","MTGA","MTGTOP8","other")),

    )),
    # overlap panel
    tabPanel("Intersection between collection and decklist",mainPanel(
      p('In this panel, we provide information related to decklist cards available or missing in collections'),

      p('Missing cards:'),

      DT::dataTableOutput("tableMissingCards"),

      downloadButton("downloadMissingCards", "Download a list of missing cards")#,

      # p('Available cards:'),
      # 
      # DT::dataTableOutput("tableAvailableCards"),

      #downloadButton("downloadAvailableCards", "Download a list of available cards")

    ))
  )
))
