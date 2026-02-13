gsa = 1:30

ui <- fluidPage(
  theme = bs_theme(version = 3),
  setBackgroundImage(src = "https://miro.medium.com/max/1400/1*JEDiI4tGpVZYM2Y5kzW5XA.png"),
  ##### CSS #####
  tags$head(
    tags$style("*{font-family: Georgia;}"),
    tags$style(
      HTML(
        "
        h3 {
        color: white; text-shadow: 2px 2px 2px rgba(0, 0, 0, 0.5)
        }
        h4 {
        color: white; text-shadow: 2px 2px 2px rgba(0, 0, 0, 0.5)
        }
        h5 {
        color: white; text-shadow: 2px 2px 2px rgba(0, 0, 0, 0.5)
        }
        h6 {
        color: white; text-shadow: 2px 2px 2px rgba(0, 0, 0, 0.5)
        }
        .navbar-default {
        background-color: rgba(156, 210, 246, 0.3) !important;
        }
        .navbar-default .navbar-brand {
        color: white;
        text-shadow: 2px 2px 2px rgba(0, 0, 0, 0.75);
        }
        .navbar-default .navbar-nav > .active > a:hover {
        color: black;
        background-color: rgba(156, 210, 246, 0.7) !important;
        }
        .navbar-default .navbar-nav > li > a:hover {
        color: black;
        background-color: rgba(156, 210, 246, 0.7) !important;
        text-shadow: 2px 2px 2px rgba(255, 255, 255, 0.75);
        text-decoration: underline;
        }
        .navbar-default .navbar-nav > li > a {
        color: white;
        text-shadow: 2px 2px 2px rgba(0, 0, 0, 0.75);
        }
        .well {
        min-width: 600px;
        padding-top: 0px;
        }
        .btn {
        border-color: transparent;
        border-radius: 4px;
        background-color: #87CEFA;
        color: black;
        text-shadow: 2px 2px 2px rgba(255, 255, 255, 0.75);
        }
        .picker-input {
        border-radius: 4px;
        background-color: #87CEFA;
        color: black !important;
        text-shadow: 2px 2px 2px rgba(255, 255, 255, 0.75);
        }
        .selected {
        background-color: #87CEFA;
        text-shadow: 2px 2px 2px rgba(255, 255, 255, 0.25);
        }
        .js-irs-0 .irs-bar {
        background: transparent;
        border: transparent;
        }
        .irs-handle {
        background: transparent !important;
        border: transparent !important;
        }
        .irs-min {
        background: #69B2E0 !important;
        text-shadow: 2px 2px 2px rgba(255, 255, 255, 0.5) !important
        }
        .irs-max {
        background: #69B2E0 !important;
        text-shadow: 2px 2px 2px rgba(255, 255, 255, 0.5) !important
        }
        .irs-single {
        background: #4682B4 !important;
        top: 20px;
        }
        .tabbable {
        width: calc(100vw - 670px);
        position: fixed;
        right: 20px
        }
        .tabbable > .nav > li[class = active] > a {
        background-color: #87CEFA;
        color: black;
        text-shadow: 2px 2px 2px rgba(255, 255, 255, 0.75);
        }
        li > a {
        color: white;
        }
        .shiny-notification {
        max-height: 100px !important;
        max-width: 800px !important;
        height: 100px;
        width: 800px;
        position: fixed;
        top: calc(50% - 50px);
        left: calc(50% - 400px);
        font-size: 250%;
        text-align: center;
        }
        .tooltip {
        width: max-content;
        }
        #sidebar {
        background-color: rgba(156, 210, 246, 0.3);
        }
        #modalWarning .modal-dialog {
        height: 100px !important;
        width: 800px !important;
        position: fixed;
        top: calc(50% - 100px);
        left: calc(50% - 400px);
        font-size: 250%;
        text-align: center;
        }
        #modalHelp .modal-dialog {
        position: fixed;
        height: 80%;
        width: 40%;
        top: 10%;
        left: 35%;
        font-size: 100%;
        text-align: left;
        overflow: auto;
        max-height: 100vh;
        }
        .modal-content {
        border-radius: 100px !important; 
        }
        .modal-body {
        background-color: #69B2E0;
        font-weight: bold;
        text-shadow: 2px 2px 2px rgba(255, 255, 255, 0.5);
        }
        .modal-title {
        text-shadow: 2px 2px 2px rgba(255, 255, 255, 0.5);
        }
        .modal-header {
        background-color: #69B2E0;
        }
        .modal-footer {
        background-color: #69B2E0;
        }
        .modal-lg {
        max-height: 1500px !important;
        max-width: 2000px !important;
        position: fixed;
        height: 80%;
        width: 80%;
        top: 10%;
        left: 10%;
        font-size: 250%;
        text-align: center;
        }
        .modal-sm {
        max-height: 1200px !important;
        max-width: 1500px !important;
        position: fixed;
        height: 60%;
        width: 60%;
        top: 20%;
        left: 20%;
        font-size: 100%;
        }
        #modalBackground .modal-body {
        height: 0px; !important;
        width: 0px; !important;
        margin: 0px;
        padding: 0px;
        background-color: rgba(0, 0, 0, 0) !important;
        font-size: 0%;
        }
        .shiny-notification {
        background-color: #87CEFA;
        text-shadow: 2px 2px 2px rgba(255, 255, 255, 0.5);
        }
        "
        )
      )
    ),
  ##### NAVBAR #####
  navbarPage("Maelstrom",
             inverse = TRUE,
             ##### LOADING #####
             tabPanel("Data Loading",
                      sidebarLayout(
                        sidebarPanel(
                          id = "sidebar",
                          fluidRow(
                            column(8,
                                   h4("Number of Stock Objects:")
                            ),
                            column(4,
                                   h4("Show Help:")
                            )
                          ),
                          fluidRow(
                            column(4,
                                   pickerInput(
                                     inputId = "nstocks",
                                     choices = c(1:10),
                                     options = list(style = "picker-input")
                                   )
                            ),
                            column(4,
                                   
                            ),
                            column(4,
                                   actionButton("generalHelp", "Instructions", width = "100%")
                            )
                          ),
                          fluidRow(
                            column(6,
                                   h4("Input Stock Objects"), align = "left"
                                   ),
                            column(2,
                                   h4("Stock Info"), align = "left"
                                   ),
                            column(2,
                                   h4("Max Cohort"), align = "left"
                                   ),
                            column(2,
                                   h4("Reset Stock"), align = "left"
                                   ),
                            align = "center"
                          ),
                          ##### STOCK OBJECT 1 #####
                          fluidRow(
                            column(6,
                                   uiOutput("sobj1")
                            ),
                            column(1,
                                   htmlOutput("triHelp1")
                            ),
                            column(1,
                                   htmlOutput("gsaHelp1")
                            ),
                            column(2,
                                   pickerInput(
                                     inputId = "baseline1",
                                     choices = 0,
                                     multiple = FALSE,
                                     options = list(style = "picker-input", title = "Age")
                                   )
                            ),
                            column(2,
                                   actionButton("reset1", "RESET", options = list(style = "btn"))
                            )
                          ),
                          ##### STOCK OBJECT 2 #####
                          fluidRow(
                            column(12,
                                   conditionalPanel(
                                     condition = "input.nstocks >= 2",
                                     fluidRow(
                                       column(6,
                                              uiOutput("sobj2")
                                       ),
                                       column(1,
                                              htmlOutput("triHelp2")
                                       ),
                                       column(1,
                                              htmlOutput("gsaHelp2")
                                       ),
                                       column(2,
                                              pickerInput(
                                                inputId = "baseline2",
                                                choices = 0,
                                                multiple = FALSE,
                                                options = list(style = "picker-input", title = "Age")
                                              )
                                       ),
                                       column(2,
                                              actionButton("reset2", "RESET", options = list(style = "btn"))
                                       )
                                     )
                                   )
                            )
                          ),
                          ##### STOCK OBJECT 3 #####
                          fluidRow(
                            column(12,
                                   conditionalPanel(
                                     condition = "input.nstocks >= 3",
                                     fluidRow(
                                       column(6,
                                              uiOutput("sobj3")
                                       ),
                                       column(1,
                                              htmlOutput("triHelp3")
                                       ),
                                       column(1,
                                              htmlOutput("gsaHelp3")
                                       ),
                                       column(2,
                                              pickerInput(
                                                inputId = "baseline3",
                                                choices = 0,
                                                multiple = FALSE,
                                                options = list(style = "picker-input", title = "Age")
                                              )
                                       ),
                                       column(2,
                                              actionButton("reset3", "RESET", options = list(style = "btn"))
                                       )
                                     )
                                   )
                            )
                          ),
                          ##### STOCK OBJECT 4 #####
                          fluidRow(
                            column(12,
                                   conditionalPanel(
                                     condition = "input.nstocks >= 4",
                                     fluidRow(
                                       column(6,
                                              uiOutput("sobj4")
                                       ),
                                       column(1,
                                              htmlOutput("triHelp4")
                                       ),
                                       column(1,
                                              htmlOutput("gsaHelp4")
                                       ),
                                       column(2,
                                              pickerInput(
                                                inputId = "baseline4",
                                                choices = 0,
                                                multiple = FALSE,
                                                options = list(style = "picker-input", title = "Age")
                                              )
                                       ),
                                       column(2,
                                              actionButton("reset4", "RESET", options = list(style = "btn"))
                                       )
                                     )
                                   )
                            )
                          ),
                          ##### STOCK OBJECT 5 #####
                          fluidRow(
                            column(12,
                                   conditionalPanel(
                                     condition = "input.nstocks >= 5",
                                     fluidRow(
                                       column(6,
                                              uiOutput("sobj5")
                                       ),
                                       column(1,
                                              htmlOutput("triHelp5")
                                       ),
                                       column(1,
                                              htmlOutput("gsaHelp5")
                                       ),
                                       column(2,
                                              pickerInput(
                                                inputId = "baseline5",
                                                choices = 0,
                                                multiple = FALSE,
                                                options = list(style = "picker-input", title = "Age")
                                              )
                                       ),
                                       column(2,
                                              actionButton("reset5", "RESET", options = list(style = "btn"))
                                       )
                                     )
                                   )
                            )
                          ),
                          ##### STOCK OBJECT 6 #####
                          fluidRow(
                            column(12,
                                   conditionalPanel(
                                     condition = "input.nstocks >= 6",
                                     fluidRow(
                                       column(6,
                                              uiOutput("sobj6")
                                       ),
                                       column(1,
                                              htmlOutput("triHelp6")
                                       ),
                                       column(1,
                                              htmlOutput("gsaHelp6")
                                       ),
                                       column(2,
                                              pickerInput(
                                                inputId = "baseline6",
                                                choices = 0,
                                                multiple = FALSE,
                                                options = list(style = "picker-input", title = "Age")
                                              )
                                       ),
                                       column(2,
                                              actionButton("reset6", "RESET", options = list(style = "btn"))
                                       )
                                     )
                                   )
                            )
                          ),
                          ##### STOCK OBJECT 7 #####
                          fluidRow(
                            column(12,
                                   conditionalPanel(
                                     condition = "input.nstocks >= 7",
                                     fluidRow(
                                       column(6,
                                              uiOutput("sobj7")
                                       ),
                                       column(1,
                                              htmlOutput("triHelp7")
                                       ),
                                       column(1,
                                              htmlOutput("gsaHelp7")
                                       ),
                                       column(2,
                                              pickerInput(
                                                inputId = "baseline7",
                                                choices = 0,
                                                multiple = FALSE,
                                                options = list(style = "picker-input", title = "Age")
                                              )
                                       ),
                                       column(2,
                                              actionButton("reset7", "RESET", options = list(style = "btn"))
                                       )
                                     )
                                   )
                            )
                          ),
                          ##### STOCK OBJECT 8 #####
                          fluidRow(
                            column(12,
                                   conditionalPanel(
                                     condition = "input.nstocks >= 8",
                                     fluidRow(
                                       column(6,
                                              uiOutput("sobj8")
                                       ),
                                       column(1,
                                              htmlOutput("triHelp8")
                                       ),
                                       column(1,
                                              htmlOutput("gsaHelp8")
                                       ),
                                       column(2,
                                              pickerInput(
                                                inputId = "baseline8",
                                                choices = 0,
                                                multiple = FALSE,
                                                options = list(style = "picker-input", title = "Age")
                                              )
                                       ),
                                       column(2,
                                              actionButton("reset8", "RESET", options = list(style = "btn"))
                                       )
                                     )
                                   )
                            )
                          ),
                          ##### STOCK OBJECT 9 #####
                          fluidRow(
                            column(12,
                                   conditionalPanel(
                                     condition = "input.nstocks >= 9",
                                     fluidRow(
                                       column(6,
                                              uiOutput("sobj9")
                                       ),
                                       column(1,
                                              htmlOutput("triHelp9")
                                       ),
                                       column(1,
                                              htmlOutput("gsaHelp9")
                                       ),
                                       column(2,
                                              pickerInput(
                                                inputId = "baseline9",
                                                choices = 0,
                                                multiple = FALSE,
                                                options = list(style = "picker-input", title = "Age")
                                              )
                                       ),
                                       column(2,
                                              actionButton("reset9", "RESET", options = list(style = "btn"))
                                       )
                                     )
                                   )
                            )
                          ),
                          ##### STOCK OBJECT 10 #####
                          fluidRow(
                            column(12,
                                   conditionalPanel(
                                     condition = "input.nstocks >= 10",
                                     fluidRow(
                                       column(6,
                                              uiOutput("sobj10")
                                       ),
                                       column(1,
                                              htmlOutput("triHelp10")
                                       ),
                                       column(1,
                                              htmlOutput("gsaHelp10")
                                       ),
                                       column(2,
                                              pickerInput(
                                                inputId = "baseline10",
                                                choices = 0,
                                                multiple = FALSE,
                                                options = list(style = "picker-input", title = "Age")
                                              )
                                       ),
                                       column(2,
                                              actionButton("reset10", "RESET", options = list(style = "btn"))
                                       )
                                     )
                                   )
                            )
                          ),
                          ##### LOADING (cont.) #####
                          fluidRow(
                            h3("Process DataFrames"),
                            align = "center"
                          ),
                          fluidRow(
                            column(4,
                            ),
                            column(4,
                                   actionButton("loadButton", "Load", width = "75%"),
                                   align = "center"
                            ),
                            column(4,
                            )
                          ),
                          fluidRow(
                            h3("Zoom Plots"),
                            align = "center"
                          ),
                          fluidRow(
                            column(4,
                                   actionButton("zoomPopButton", "Population", width = "75%"),
                                   bsModal("modalplotPop",
                                           "Data structure - Population",
                                           "zoomPopButton",
                                           size = "large",
                                           plotOutput("zoomPop"),
                                           downloadButton("downloadPop", "Download")
                                   ),
                                   align = "left"
                            ),
                            column(4,
                                   actionButton("zoomCatchButton", "Catches", width = "75%"),
                                   bsModal("modalplotCatch",
                                           "Data structure - Catches",
                                           "zoomCatchButton",
                                           size = "large",
                                           plotOutput("zoomCatch"),
                                           downloadButton("downloadCatch", "Download")
                                   ),
                                   align = "center"
                            ),
                            column(4,
                                   actionButton("zoomWaaButton", "SSB", width = "75%"),
                                   bsModal("modalplotWaa",
                                           "Data structure - SSB",
                                           "zoomWaaButton",
                                           size = "large",
                                           plotOutput("zoomWaa"),
                                           downloadButton("downloadWaa", "Download")
                                   ),
                                   align = "right"
                            )
                          )
                        ),
                        ##### DATAFRAMES PLOT #####
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Population",
                                     uiOutput("uiPop")
                            ),
                            tabPanel("Catches",
                                     uiOutput("uiCatch")
                            ),
                            tabPanel("Weight at Age",
                                     uiOutput("uiWaa")
                            )
                          )
                        )
                      )
             ),
             ##### NET BUILDING #####
             tabPanel("Net Building",
                      sidebarLayout(
                        sidebarPanel(
                          id = "sidebar",
                          fluidRow(
                            column(4,
                                   h4("Show Help:"),
                                   align = "left"
                            ),
                            column(4,
                                   h4("N° of Layers:"),
                                   align = "left"
                            ),
                            column(4,
                                   h4("N° of Epochs:"),
                                   align = "left"
                            )
                          ),
                          fluidRow(
                            column(4,
                                   actionButton("netHelp", "Instructions", width = "75%"),
                                   align = "left"
                            ),
                            column(4,
                                   pickerInput(
                                     inputId = "nLayers",
                                     choices = c(2:5),
                                     selected = 3,
                                     options = list(style = "picker-input", title = "Layers", width = "75%")
                                   ),
                                   align = "left"
                            ),
                            column(4,
                                   pickerInput(
                                     inputId = "nEpochs",
                                     choices = c(25, 50, 100, 200, 300),
                                     selected = 50,
                                     options = list(style = "picker-input", title = "Epochs", width = "75%")
                                   ),
                                   align = "left"
                            )
                          ),
                          fluidRow(
                            column(4,
                                   h4("Activation Function:"),
                                   align = "left"
                            ),
                            column(4,
                                   h4("Recurrent Layers Activation Function:"),
                                   align = "left"
                            ),
                            column(4,
                                   h4("Learning Rate:"),
                                   align = "left"
                            )
                          ),
                          fluidRow(
                            column(3,
                                   pickerInput(
                                     inputId = "activation",
                                     choices = c("relu", "sigmoid", "softmax", "tanh"),
                                     selected = "tanh",
                                     multiple = FALSE,
                                     options = list(style = "picker-input", title = "Activation")
                                   ),
                                   align = "left"
                            ),
                            column(1,
                                   htmlOutput("actHelp"),
                                   align = "left"
                            ),
                            column(3,
                                   pickerInput(
                                     inputId = "recactivation",
                                     choices = c("relu", "sigmoid", "softmax", "tanh"),
                                     selected = "tanh",
                                     multiple = FALSE,
                                     options = list(style = "picker-input", title = "Recurrent Activation")
                                     ),
                                   align = "left"
                                   ),
                            column(1,
                                   htmlOutput("recactHelp"),
                                   align = "left"
                                   ),
                            column(4,
                                   sliderInput("learnParam",
                                               label = NULL,
                                               min = 0.005,
                                               max = 0.1,
                                               step = 0.005,
                                               value = 0.025,
                                               round = -3,
                                               ticks = TRUE),
                                   align = "right"
                                   )
                            ),
                          ##### LAYER 1 #####
                          fluidRow(
                            column(12, 
                                   h4("Layer 1")
                                   )
                            ),
                          fluidRow(
                            column(4,
                                   h5("Layer Type")
                            ),
                            column(4,
                                   h5("N° of Neurons")
                            ),
                            column(4,
                                   conditionalPanel(
                                     condition = "input.layerType1 == 'LSTM' || input.layerType1 == 'SimpleRNN'",
                                     h5("Return Sequence")
                                   )
                            )
                          ),
                          fluidRow(
                            column(4,
                                   pickerInput(
                                     inputId = "layerType1",
                                     choices = c("Dense", "LSTM", "SimpleRNN"),
                                     selected = "SimpleRNN",
                                     multiple = FALSE,
                                     options = list(style = "picker-input", title = "Type")
                                   )
                            ),
                            column(4,
                                   pickerInput(
                                     inputId = "neurons1",
                                     choices = NA,
                                     selected = NA,
                                     multiple = FALSE,
                                     options = list(style = "picker-input", title = "Neurons")
                                   )
                            ),
                            column(4,
                                   conditionalPanel(
                                     condition = "input.layerType1 == 'LSTM' || input.layerType1 == 'SimpleRNN'",
                                     pickerInput(
                                       inputId = "returnSeq1",
                                       choices = c(TRUE, FALSE),
                                       selected = FALSE,
                                       multiple = FALSE,
                                       options = list(style = "picker-input", title = "Return Seq.")
                                     )
                                   )
                            )
                          ),
                          conditionalPanel(
                            condition = "input.layerType1 == 'LSTM' || input.layerType1 == 'SimpleRNN'",
                            fluidRow(
                              column(4,
                                       h5("Dropout")
                                     ),
                              column(2,
                                       h5("Help")
                                     ),
                              column(4,
                                     h5("Recurr. Dropout")
                                     ),
                              column(2,
                                     h5("Help")
                                     )
                              )
                            ),
                          conditionalPanel(
                            condition = "input.layerType1 == 'LSTM' || input.layerType1 == 'SimpleRNN'",
                            fluidRow(
                              column(4,
                                     sliderInput("dropout1",
                                                 label = NULL,
                                                 min = 0,
                                                 max = 1,
                                                 step = 0.05,
                                                 value = 0.2,
                                                 round = -2,
                                                 ticks = TRUE)
                                     ),
                              column(2,
                                     htmlOutput("dropHelp1")
                                     ),
                              column(4,
                                     sliderInput("recdropout1",
                                                 label = NULL,
                                                 min = 0,
                                                 max = 1,
                                                 step = 0.05,
                                                 value = 0.2,
                                                 round = -2,
                                                 ticks = TRUE)
                                     ),
                              column(2,
                                     htmlOutput("recdropHelp1")
                                     )
                              )
                            ),
                          ##### LAYER 2 #####
                          conditionalPanel(
                            condition = "input.nLayers >= 2",
                            fluidRow(
                              column(12,
                                     h4("Layer 2")
                                     )
                              ),
                            fluidRow(
                              column(4,
                                     h5("Layer Type")
                                     ),
                              column(4,
                                     conditionalPanel(
                                       condition = "input.layerType2 == 'Dense' || input.layerType2 == 'LSTM' || input.layerType2 == 'SimpleRNN'",
                                       h5("N° of Neurons")
                                       )
                                     ),
                              column(4,
                                     conditionalPanel(
                                       condition = "input.layerType2 == 'LSTM' || input.layerType2 == 'SimpleRNN'",
                                       h5("Return Sequence")
                                       )
                                     )
                              ),
                            fluidRow(
                              column(4,
                                     pickerInput(
                                       inputId = "layerType2",
                                       choices = c("Dense", "Dropout", "LSTM", "SimpleRNN"),
                                       selected = "Dropout",
                                       multiple = FALSE,
                                       options = list(style = "picker-input", title = "Type")
                                       )
                                     ),
                              column(4,
                                     conditionalPanel(
                                       condition = "input.layerType2 == 'Dense' || input.layerType2 == 'LSTM' || input.layerType2 == 'SimpleRNN'",
                                       pickerInput(
                                         inputId = "neurons2",
                                         choices = NA,
                                         multiple = FALSE,
                                         options = list(style = "picker-input", title = "Neurons")
                                         )
                                       )
                                     ),
                              column(4,
                                     conditionalPanel(
                                       condition = "input.layerType2 == 'LSTM' || input.layerType2 == 'SimpleRNN'",
                                       pickerInput(
                                         inputId = "returnSeq2",
                                         choices = c(TRUE, FALSE),
                                         multiple = FALSE,
                                         options = list(style = "picker-input", title = "Return Seq.")
                                         )
                                       )
                                     )
                              ),
                            conditionalPanel(
                              condition = "input.layerType2 == 'Dropout' || input.layerType2 == 'LSTM' || input.layerType2 == 'SimpleRNN'",
                              fluidRow(
                                column(4,
                                       h5("Dropout")
                                       ),
                                column(2,
                                       h5("Help")
                                       ),
                                column(4,
                                       h5("Recurr. Dropout")
                                       ),
                                column(2,
                                       h5("Help")
                                       )
                                )
                              ),
                            conditionalPanel(
                              condition = "input.layerType2 == 'Dropout' || input.layerType2 == 'LSTM' || input.layerType2 == 'SimpleRNN'",
                              fluidRow(
                                column(4,
                                       sliderInput("dropout2",
                                                   label = NULL,
                                                   min = 0,
                                                   max = 1,
                                                   step = 0.05,
                                                   value = 0.2,
                                                   round = -2,
                                                   ticks = TRUE)
                                       ),
                                column(2,
                                       htmlOutput("dropHelp2")
                                       ),
                                column(4,
                                       sliderInput("recdropout2",
                                                   label = NULL,
                                                   min = 0,
                                                   max = 1,
                                                   step = 0.05,
                                                   value = 0.2,
                                                   round = -2,
                                                   ticks = TRUE)
                                       ),
                                column(2,
                                       htmlOutput("recdropHelp2")
                                       )
                                )
                              )
                          ),
                          ##### LAYER 3 #####
                          conditionalPanel(
                            condition = "input.nLayers >= 3",
                            fluidRow(
                              column(12, 
                                     h4("Layer 3")
                              )
                            ),
                            fluidRow(
                              column(4,
                                     h5("Layer Type")
                              ),
                              column(4,
                                     conditionalPanel(
                                       condition = "input.layerType3 == 'Dense' || input.layerType3 == 'LSTM' || input.layerType3 == 'SimpleRNN'",
                                       h5("N° of Neurons")
                                     )
                              ),
                              column(4,
                                     conditionalPanel(
                                       condition = "input.layerType3 == 'LSTM' || input.layerType3 == 'SimpleRNN'",
                                       h5("Return Sequence")
                                     )
                              )
                            ),
                            fluidRow(
                              column(4,
                                     pickerInput(
                                       inputId = "layerType3",
                                       choices = c("Dense", "Dropout", "LSTM", "SimpleRNN"),
                                       selected = "Dense",
                                       multiple = FALSE,
                                       options = list(style = "picker-input", title = "Type")
                                     )
                              ),
                              column(4,
                                     conditionalPanel(
                                       condition = "input.layerType3 == 'Dense' || input.layerType3 == 'LSTM' || input.layerType3 == 'SimpleRNN'",
                                       pickerInput(
                                         inputId = "neurons3",
                                         choices = NA,
                                         selected = NA,
                                         multiple = FALSE,
                                         options = list(style = "picker-input", title = "Neurons")
                                       )
                                     )
                              ),
                              column(4,
                                     conditionalPanel(
                                       condition = "input.layerType3 == 'LSTM' || input.layerType3 == 'SimpleRNN'",
                                       pickerInput(
                                         inputId = "returnSeq3",
                                         choices = c(TRUE, FALSE),
                                         multiple = FALSE,
                                         options = list(style = "picker-input", title = "Return Seq.")
                                       )
                                     )
                              )
                            ),
                            conditionalPanel(
                              condition = "input.layerType3 == 'Dropout' || input.layerType3 == 'LSTM' || input.layerType3 == 'SimpleRNN'",
                              fluidRow(
                                column(4,
                                       h5("Dropout")
                                ),
                                column(2,
                                       h5("Help")
                                ),
                                column(4,
                                       h5("Recurr. Dropout")
                                ),
                                column(2,
                                       h5("Help")
                                )
                              )
                            ),
                            conditionalPanel(
                              condition = "input.layerType3 == 'Dropout' || input.layerType3 == 'LSTM' || input.layerType3 == 'SimpleRNN'",
                              fluidRow(
                                column(4,
                                       sliderInput("dropout3",
                                                   label = NULL,
                                                   min = 0,
                                                   max = 1,
                                                   step = 0.05,
                                                   value = 0.2,
                                                   round = -2,
                                                   ticks = TRUE)
                                ),
                                column(2,
                                       htmlOutput("dropHelp3")
                                ),
                                column(4,
                                       sliderInput("recdropout3",
                                                   label = NULL,
                                                   min = 0,
                                                   max = 1,
                                                   step = 0.05,
                                                   value = 0.2,
                                                   round = -2,
                                                   ticks = TRUE)
                                ),
                                column(2,
                                       htmlOutput("recdropHelp3")
                                )
                              )
                            )
                          ),
                          ##### LAYER 4 #####
                          conditionalPanel(
                            condition = "input.nLayers >= 4",
                            fluidRow(
                              column(12, 
                                     h4("Layer 4")
                              )
                            ),
                            fluidRow(
                              column(4,
                                     h5("Layer Type")
                              ),
                              column(4,
                                     conditionalPanel(
                                       condition = "input.layerType4 == 'Dense' || input.layerType4 == 'LSTM' || input.layerType4 == 'SimpleRNN'",
                                       h5("N° of Neurons")
                                     )
                              ),
                              column(4,
                                     conditionalPanel(
                                       condition = "input.layerType4 == 'LSTM' || input.layerType4 == 'SimpleRNN'",
                                       h5("Return Sequence")
                                     )
                              )
                            ),
                            fluidRow(
                              column(4,
                                     pickerInput(
                                       inputId = "layerType4",
                                       choices = c("Dense", "Dropout", "LSTM", "SimpleRNN"),
                                       selected = "SimpleRNN",
                                       multiple = FALSE,
                                       options = list(style = "picker-input", title = "Type")
                                     )
                              ),
                              column(4,
                                     conditionalPanel(
                                       condition = "input.layerType4 == 'Dense' || input.layerType4 == 'LSTM' || input.layerType4 == 'SimpleRNN'",
                                       pickerInput(
                                         inputId = "neurons4",
                                         choices = NA,
                                         selected = NA,
                                         multiple = FALSE,
                                         options = list(style = "picker-input", title = "Neurons")
                                       )
                                     )
                              ),
                              column(4,
                                     conditionalPanel(
                                       condition = "input.layerType4 == 'LSTM' || input.layerType4 == 'SimpleRNN'",
                                       pickerInput(
                                         inputId = "returnSeq4",
                                         choices = c(TRUE, FALSE),
                                         multiple = FALSE,
                                         options = list(style = "picker-input", title = "Return Seq.")
                                       )
                                     )
                              )
                            ),
                            conditionalPanel(
                              condition = "input.layerType4 == 'Dropout' || input.layerType4 == 'LSTM' || input.layerType4 == 'SimpleRNN'",
                              fluidRow(
                                column(4,
                                       h5("Dropout")
                                ),
                                column(2,
                                       h5("Help")
                                ),
                                column(4,
                                       h5("Recurr. Dropout")
                                ),
                                column(2,
                                       h5("Help")
                                )
                              )
                            ),
                            conditionalPanel(
                              condition = "input.layerType4 == 'Dropout' || input.layerType4 == 'LSTM' || input.layerType4 == 'SimpleRNN'",
                              fluidRow(
                                column(4,
                                       sliderInput("dropout4",
                                                   label = NULL,
                                                   min = 0,
                                                   max = 1,
                                                   step = 0.05,
                                                   value = 0.2,
                                                   round = -2,
                                                   ticks = TRUE)
                                ),
                                column(2,
                                       htmlOutput("dropHelp4")
                                ),
                                column(4,
                                       sliderInput("recdropout4",
                                                   label = NULL,
                                                   min = 0,
                                                   max = 1,
                                                   step = 0.05,
                                                   value = 0.2,
                                                   round = -2,
                                                   ticks = TRUE)
                                ),
                                column(2,
                                       htmlOutput("recdropHelp4")
                                )
                              )
                            )
                          ),
                          ##### LAYER 5 #####
                          conditionalPanel(
                            condition = "input.nLayers >= 5",
                            fluidRow(
                              column(12, 
                                     h4("Layer 5")
                              )
                            ),
                            fluidRow(
                              column(4,
                                     h5("Layer Type")
                              ),
                              column(4,
                                     conditionalPanel(
                                       condition = "input.layerType5 == 'Dense' || input.layerType5 == 'LSTM' || input.layerType5 == 'SimpleRNN'",
                                       h5("N° of Neurons")
                                     )
                              ),
                              column(4,
                                     conditionalPanel(
                                       condition = "input.layerType5 == 'LSTM' || input.layerType5 == 'SimpleRNN'",
                                       h5("Return Sequence")
                                     )
                              )
                            ),
                            fluidRow(
                              column(4,
                                     pickerInput(
                                       inputId = "layerType5",
                                       choices = c("Dense", "Dropout", "LSTM", "SimpleRNN"),
                                       selected = "SimpleRNN",
                                       multiple = FALSE,
                                       options = list(style = "picker-input", title = "Type")
                                     )
                              ),
                              column(4,
                                     conditionalPanel(
                                       condition = "input.layerType5 == 'Dense' || input.layerType5 == 'LSTM' || input.layerType5 == 'SimpleRNN'",
                                       pickerInput(
                                         inputId = "neurons5",
                                         choices = NA,
                                         multiple = FALSE,
                                         options = list(style = "picker-input", title = "Neurons")
                                       )
                                     )
                              ),
                              column(4,
                                     conditionalPanel(
                                       condition = "input.layerType5 == 'LSTM' || input.layerType5 == 'SimpleRNN'",
                                       pickerInput(
                                         inputId = "returnSeq5",
                                         choices = c(TRUE, FALSE),
                                         multiple = FALSE,
                                         options = list(style = "picker-input", title = "Return Seq.")
                                       )
                                     )
                              )
                            ),
                            conditionalPanel(
                              condition = "input.layerType5 == 'Dropout' || input.layerType5 == 'LSTM' || input.layerType5 == 'SimpleRNN'",
                              fluidRow(
                                column(4,
                                       h5("Dropout")
                                ),
                                column(2,
                                       h5("Help")
                                ),
                                column(4,
                                       h5("Recurr. Dropout")
                                ),
                                column(2,
                                       h5("Help")
                                )
                              )
                            ),
                            conditionalPanel(
                              condition = "input.layerType5 == 'Dropout' || input.layerType5 == 'LSTM' || input.layerType5 == 'SimpleRNN'",
                              fluidRow(
                                column(4,
                                       sliderInput("dropout5",
                                                   label = NULL,
                                                   min = 0,
                                                   max = 1,
                                                   step = 0.05,
                                                   value = 0.2,
                                                   round = -2,
                                                   ticks = TRUE)
                                ),
                                column(2,
                                       htmlOutput("dropHelp5")
                                ),
                                column(4,
                                       sliderInput("recdropout5",
                                                   label = NULL,
                                                   min = 0,
                                                   max = 1,
                                                   step = 0.05,
                                                   value = 0.2,
                                                   round = -2,
                                                   ticks = TRUE)
                                ),
                                column(2,
                                       htmlOutput("recdropHelp5")
                                )
                              )
                            )
                          ),
                          ##### NET BUILDING (cont.) #####
                          fluidRow(
                            column(4,
                                   actionButton("plotNetButton", "Plot Net", width = "100%"),
                                   align = "left"
                            ),
                            column(8,
                            )
                          ),
                        ),
                        ##### NET PLOT #####
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Net",
                                     plotOutput("plotNet")
                            )
                          )
                        )
                      )
             ),
             ##### TEST #####
             tabPanel("Test",
                      sidebarLayout(
                        sidebarPanel(
                          id = "sidebar",
                          fluidRow(
                            h3("Test Model Fit"),
                            align = "center"
                          ),
                          fluidRow(
                            column(3,
                            ),
                            column(3,
                                   actionButton("testFitButton", "Fit", width = "100%"),
                                   align = "left"
                            ),
                            column(3,
                                   actionButton("plotFitButton", "Results", width = "100%"),
                                   align = "left"
                            ),
                            column(3,
                            )
                          ),
                          fluidRow(
                            h3("Test on Observed Data"),
                            align = "center"
                          ),
                          fluidRow(
                            column(3,
                                   pickerInput(
                                     inputId = "depthTest",
                                     choices = c(2:10),
                                     options = list(style = "picker-input", title = "Depth", width = "75%")
                                   ),
                                   align = "right"
                            ),
                            column(3,
                                   actionButton("testTrainTestButton", "Test", width = "100%"),
                                   align = "center"
                            ),
                            column(3,
                                   actionButton("plotTrainTestButton", "Plot", width = "100%"),
                                   align = "center"
                            ),
                            column(3,
                                   actionButton("zoomPlotFitButton", "Zoom", width = "75%"),
                                   bsModal("modalZoomPlotFitButton",
                                           "Prediction Plot",
                                           "zoomPlotFitButton",
                                           size = "large",
                                           plotOutput("zoomPlotFit"),
                                           downloadButton("downloadFit", "Download")
                                   ),
                                   align = "left"
                            )
                          ),
                          fluidRow(
                            column(3,
                                   ),
                            column(3,
                                   actionButton("plotTestBack", icon("angle-left"), width = "100%"),
                                   align = "center"
                                   ),
                            column(3,
                                   actionButton("plotTestForward", icon("angle-right"), width = "100%"),
                                   align = "center"
                                   ),
                            column(3,
                                   verbatimTextOutput("showSpeciesTest"),
                                   align = "left"
                                   )
                            ),
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Fit Results",
                                     plotlyOutput("plotFit")
                            ),
                            tabPanel("Train/Test Plot",
                                     plotlyOutput("plotTrainTest"),
                                     checkboxInput("plotLogTrainTest", "log10", value = F)
                            ),
                            tabPanel("Recruitment",
                                     plotOutput("plotRecruitmentTraintest")
                            ),
                            tabPanel("Taylor Diagram",
                                     plotOutput("taylorDiagram"))
                          )
                        )
                      )
             ),
             ##### FORECAST #####
             tabPanel("Forecast",
                      sidebarLayout(
                        sidebarPanel(
                          id = "sidebar",
                          fluidRow(
                            h3("Fishing Mortality"),
                            align = "center"
                          ),
                          fluidRow(
                            column(12,
                                   actionButton("fModalButton", "Adjust Fishing Mortality"),
                                   bsModal("modalFishingMortality",
                                           "Set Fishing Mortality",
                                           "fModalButton",
                                           size = "small",
                                           fluidRow(
                                             column(3,
                                                    pickerInput(
                                                      inputId = "baseline",
                                                      choices = c(1:5), 
                                                      options = list(style = "picker-input", title = "Baseline")
                                                    )
                                             ),
                                             column(3,
                                                    actionButton("calcFmortButton", "Calc", width = "75%")
                                             ),
                                             column(3,
                                             ),
                                             column(3,
                                                    actionButton("mortalityHelp", "Instructions", width = "100%")
                                             )
                                           ),
                                           fluidRow(
                                             column(12,
                                                    div(style = "overflow-x: scroll",
                                                        h4("Status Quo"),
                                                        tableOutput("statquoFmort"),
                                                        h4("New Exploitation Pattern"),
                                                        tableOutput("adjustedFmort")
                                                    ),
                                                    fluidRow(
                                                      column(3,
                                                             h6("Choose a species or column to adjust"),
                                                             uiOutput("pickFmort")
                                                      ),
                                                      column(3,
                                                             h6("Apply to All Columns"),
                                                             actionButton("calcMultAdjustFmort", "All", width = "75%")
                                                      ),
                                                      column(6,
                                                             h6("Adjust Fishing Mortality"),
                                                             sliderInput("adjustFmort",
                                                                         label = NULL,
                                                                         min = 0,
                                                                         max = 2,
                                                                         step = 0.01,
                                                                         value = 1,
                                                                         round = -2,
                                                                         ticks = TRUE)
                                                      )
                                                    )
                                             )
                                           ),
                                           fluidRow(
                                             column(4,
                                                    downloadButton("downloadAdjFmort",
                                                                   "Download Adjusted Fishing Mortality")
                                             ),
                                             column(4,
                                                    shinyFilesButton("fileAdjFmort",
                                                                     "Choose a file",
                                                                     title = "Please select a file:",
                                                                     multiple = FALSE),
                                             ),
                                             column(4,
                                                    actionButton("loadAdjFmort",
                                                                 "Load Adjusted Fishing Mortality")
                                             )
                                           )
                                   ),
                                   align = "center"
                            )
                          ),
                          fluidRow(
                            h3("Forecast Phase"),
                            align = "center"
                          ),
                          fluidRow(
                            column(3,
                                   pickerInput(
                                     inputId = "depthPred",
                                     choices = c(1:15),
                                     options = list(style = "picker-input", title = "Depth", width = "75%")
                                   ),
                                   align = "right"
                            ),
                            column(3,
                                   actionButton("calcPredButton", "Predict", width = "100%"),
                                   align = "center"
                            ),
                            column(3,
                                   actionButton("plotPredButton", "Plot", width = "100%"),
                                   align = "center"
                            ),
                            column(3,
                                   actionButton("zoomPredButton", "Zoom", width = "75%"),
                                   bsModal("modalZoomPred",
                                           "Prediction Plot",
                                           "zoomPredButton",
                                           size = "large",
                                           plotOutput("zoomPred"),
                                           downloadButton("downloadPred", "Download")
                                   ),
                                   align = "left"
                            )
                          ),
                          fluidRow(
                            column(3,
                                   ),
                            column(3,
                                   actionButton("plotPredBack", icon("angle-left"), width = "100%"),
                                   align = "center"
                                   ),
                            column(3,
                                   actionButton("plotPredForward", icon("angle-right"), width = "100%"),
                                   align = "center"
                                   ),
                            column(3,
                                   verbatimTextOutput("showSpeciesPred"),
                                   align = "left"
                                   )
                          ),
                          fluidRow(
                            h3("Sensitivity Analysis"),
                            align = "center"
                          ),
                          fluidRow(
                            column(4,
                                   actionButton("calcSensButton", "Sens. Analysis", width = "75%"),
                                   align = "left"
                            ),
                            column(4,
                                   actionButton("plotSensButton", "Plot", width = "75%"),
                                   align = "center"
                            ),
                            column(4,
                                   actionButton("zoomSensButton", "Zoom", width = "75%"),
                                   bsModal("modalZoomSens",
                                           "Sensitivity Analysis",
                                           "zoomSensButton",
                                           size = "large",
                                           plotOutput("zoomSens"),
                                           downloadButton("downloadSens", "Download")
                                   ),
                                   align = "right"
                            )
                          )
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Forecast",
                                     plotlyOutput("plotPred"),
                                     checkboxInput("plotLogPred", "log10", value = F)
                            ),
                            tabPanel("Recruitment",
                                     plotOutput("plotRecruitmentForecast")
                            ),
                            tabPanel("Sensitivity",
                                     plotOutput("plotSens")
                            )
                          )
                        )
                      )
             ),
             ##### EXPORT AND LOAD #####
             tabPanel("Export and Load",
                      sidebarLayout(
                        sidebarPanel(
                          id = "sidebar",
                          fluidRow(
                            column(12,
                                   h3("Save Workspace"),
                                   align = "center"
                            )
                          ),
                          fluidRow(
                            column(4,
                                   shinyDirButton("dir", "Choose directory", "Upload", width = "75%"),
                                   align = "left"
                            ),
                            column(4,
                                   verbatimTextOutput("dir"),
                                   align = "center"
                            ),
                            column(4,
                                   actionButton("saveWS", "Save RData", width = "75%"),
                                   align = "right"
                            )
                          ),
                          fluidRow(
                            column(12,
                                   h3("Load Workspace"),
                                   align = "center"
                            )
                          ),
                          fluidRow(
                            column(4,
                                   shinyFilesButton("file",
                                                    "Choose a file",
                                                    title = "Please select a file:",
                                                    multiple = FALSE,
                                                    width = "75%"
                                   ),
                                   align = "left"
                            ),
                            column(4,
                                   conditionalPanel(
                                     condition = "input.file != 0",
                                     verbatimTextOutput("file")
                                   ),
                                   align = "center"
                            ),
                            column(4,
                                   actionButton("loadWS", "Load RData", width = "75%"),
                                   align = "right"
                            )
                          ),
                          fluidRow(
                            h3("Generate Report"),
                            align = "center"
                          ),
                          fluidRow(
                            downloadButton("report", "Report", style = "width: 25%;"),
                            align = "center"
                          )
                        ),
                        mainPanel(
                        )
                      )
             )
  )
)

##### END #####

return(ui)