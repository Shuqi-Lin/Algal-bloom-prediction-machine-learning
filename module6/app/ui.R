# Load required libraries
suppressPackageStartupMessages(library(shinyjs, quietly = TRUE))
suppressPackageStartupMessages(library(shinyBS, quietly = TRUE))
suppressPackageStartupMessages(library(shinydashboard, quietly = TRUE))
suppressPackageStartupMessages(library(rintrojs, quietly = TRUE))
suppressPackageStartupMessages(library(slickR, quietly = TRUE))
suppressPackageStartupMessages(library(sortable, quietly = TRUE))
suppressPackageStartupMessages(library(ncdf4, quietly = TRUE))
suppressPackageStartupMessages(library(ggplot2, quietly = TRUE))
suppressPackageStartupMessages(library(stringr, quietly = TRUE))
suppressPackageStartupMessages(library(hover, quietly = TRUE))
# suppressPackageStartupMessages(library(ggforce, quietly = TRUE)) # Only for geom_ellipse (doesn't work in plotly!)

# Help documentation
help_text <- read.csv("data/help_text.csv", row.names = 1)

# Load text input
module_text <- read.csv("data/module_text.csv", row.names = 1, header = FALSE)

# colors for theme
obj_bg <- "#D4ECE1"
ques_bg <- "#B8E0CD"
nav_bg <- "#DDE4E1"
nav_butt <- "#31ED92"
nav_txt <- "#000000" # white = #fff; black = #000000
slider_col <- "#2CB572"

ui <- function(req) {

  tagList( # Added functionality for not losing your settings
    # shinythemes::themeSelector(), # user-defined theme
    # Java to prompt the students to click a button
    # Java script https://community.rstudio.com/t/keeping-track-of-idle-time-during-app-usage/1735
    tags$script("
              (function() {
  var timeoutWarningMsecs = 12 * 60 * 1000;
  var idleTimer;

  function onTimeout() {
    alert('Warning: Session is about to time out! Please click a button to prevent losing progress.');
  }

  function startIdleTimer() {
    if (idleTimer) clearTimeout(idleTimer);
    idleTimer = setTimeout(onTimeout, timeoutWarningMsecs);
  }

  $(document).on('shiny:message shiny:inputchanged', startIdleTimer);

})();"),
    tags$style(type = "text/css", "text-align: justify"),
    tags$head(tags$link(rel = "shortcut icon", href = "macroeddi_ico_green.ico")), # Add icon for web bookmarks
    tags$head(includeHTML(("google-analytics.html"))),
    fluidPage(
      column(1, offset = 11, align = "right",
             introBox(
               actionButton("help", label = "", icon = icon("question-circle")), data.step = 7, data.intro = help_text["help", 1]
             )
      )
    ),
    navbarPage(title = "Module 6: Understanding Uncertainty in Ecological Forecasts",
               position = "static-top", id = "maintab",

               # 1. Module Overview ----
               tabPanel(introBox("Overview",
                                 data.step = 2,
                                 data.intro = help_text["tab_nav1", 1]
               ),
               value = "mtab1",
               introjsUI(), # must include in UI
               introBox(
                 img(src = "project-eddie-banner-2020_green.png", height = 100,
                     width = 1544, top = 5),
                 data.step = 1,
                 data.intro = help_text["welcome", 1]
               ),
               withMathJax(), # NEEDS to be here for rendering eqn's in data.table

               tags$style(".btn-file {
             background-color:#98CAB2;
             border-color: #579277;
             }

             .progress-bar {
             background-color: #579277;
             }"),
               # Change progress bar color
               tags$style(paste0("
                                   .irs-bar,
.irs-bar-edge,
.irs-single,
.irs-grid-pol {
  background: ", slider_col, ";
  border-color: ", slider_col, ";
}")),
               includeCSS("www/slider_cols.css"),
               tags$style(HTML("
               .irs-bar {
                        border-color: transparent;
                        background-color: transparent;
                        }
                        #first {
                        border: 4px double red;
                        }
                        #13a_graz {
                        margin-bottom: 10px;
                        }
                        #bla_border {
                        border: 2px solid black;
                        }
                        #bla_border2 {
                        border: 1px solid black;
                        box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);
                        }
                        #txt_j {
                        text-align: justify;
                        }
                        #txt_c {
                        text-align: center;
                        }
                        #txt_l {
                        text-align: left;
                        }
                        #ackn {
                        color: gray;
                        font-size: 12px
                        }
                        #pheno img {
                        transition:transform 0.25s ease;
                        max-width: 100%; width: 100%; height: auto
                        }
                        #nextBtn1:hover {
                        background-color: yellow;
                        }
                        #dl_btn {
                        width:290px
                        }
                        #pheno:hover img{
    -webkit-transform:scale(1.5);
    transform:scale(1.5);
}
                        #wh_link a {
                        color: #FFFFFF
                        }
                        #q6_tab {
                        'border':'1px solid #ddd'
                        }
                        .box.box-solid.box-primary>.box-header {

                }

                .box.box-solid.box-primary{

                background:#B8E0CD
                }
                .box.box-solid.box-success{

                background: #DDE4E1;
                }
                .box.box-solid.box-warning>.box-header {

                }

                .box.box-solid.box-warning{

                background:#FFBE85
                }
                        ")),
               introBox(
                 fluidRow(
                   column(6,
                          #* Module text ====
                          h2("Understanding Uncertainty in Ecological Forecasts"),
                          h3("Summary"),
                          p(id = "txt_j", module_text["eco_forecast", ]),
                          p(id = "txt_j", module_text["this_module", ]),
                          h3("Learning Outcomes"),
                          tags$line(),
                          tags$ul(
                            tags$li(id = "txt_j", module_text["LO1", ]),
                            tags$li(id = "txt_j", module_text["LO2", ]),
                            tags$li(id = "txt_j", module_text["LO3", ]),
                            tags$li(id = "txt_j", module_text["LO4", ]),
                            tags$li(id = "txt_j", module_text["LO5", ]),
                            tags$li(id = "txt_j", module_text["LO6", ])
                          )
                   ),
                   column(5, offset = 1,
                          br(), br(), br(),
                          img(src = "Schematic_Draft_v3.png", height = "80%",
                              width = "80%", align = "left")
                          )
                   ), data.step = 8, data.intro = help_text["start", 1]
                 ),
               ),

               # 2. Presentation recap ----
               tabPanel(title = "Presentation", value = "mtab2",
                        img(src = "project-eddie-banner-2020_green.png", height = 100,
                            width = 1544, top = 5),
                        fluidRow(
                          hr(),
                          column(4,
                                 h3("Presentation"),
                                 p("The presentation accompanying this module covers the introduction to forecast uncertainty, sources of forecast uncertainty and the importance and relevance of quantifying uncertainty within ecological forecasts."),
                                 p("What is ecological forecast uncertainty?"),
                                 tags$ul(
                                   tags$li(module_text["uncertainty", ])
                                 ),
                                 p("Where does ecological forecast uncertainty come from?"),
                                 tags$ul(
                                   tags$li(module_text["uncert1", ])
                                 ),
                                 p("Why is uncertainty important to quantify for an ecological forecast?"),
                                 tags$ul(
                                   tags$li(module_text["why_important", ])
                                 ),
                                 p("Click through the slides to recap some of the main points from the lecture.")
                          ),
                          column(8, offset = 0, align = "center",
                                 h3("Key Slides",
                                    align = "center"),
                                 h5("Click the arrows to navigate through the slides", align = "center"),
                                 wellPanel(
                                   slickROutput("slides", width = "600px", height = "450px")
                                 )
                          )
                        )
               ),

               # 3. Introduction ----
               tabPanel(title = "Introduction", value = "mtab3",
                        # tags$style(type="text/css", "body {padding-top: 65px;}"),
                        img(src = "project-eddie-banner-2020_green.png", height = 100,
                            width = 1544, top = 5),
                        fluidRow(
                          column(5,
                                 h3("Workflow for this module"),
                                 tags$ol(
                                   tags$li(id = "txt_j", module_text["workflow1", ]),
                                   tags$li(id = "txt_j", module_text["workflow2", ]),
                                   tags$li(id = "txt_j", module_text["workflow3", ]),
                                   tags$li(id = "txt_j", module_text["workflow4", ])
                                   # tags$li(id = "txt_j", module_text["workflow5", ]),
                                   # tags$li(id = "txt_j", module_text["workflow6", ])
                                 )
                          ),
                          column(6, align = "center", offset = 1,
                                 br(), br(),
                                 img(src = "activity_outline.png", height = "80%", id = "bla_border",
                                     width = "80%", tags$style("border: solid 2px black;"))

                          )
                        ), hr(),
                        fluidRow(
                          column(7, offset = 1,
                                 h3("Student Activities"),
                                 p("Within the Introduction, Exploration, and Activities A, B and C tabs, there are questions for students to complete as part of this module. These can be completed by writing your answers into the text boxes within the green boxes. If you do not complete the module in one continuous sitting, you can download a file with your responses saved, which you can then upload when you return. When you finish the module, you can generate a report which will embed your answers and saved plots into a Word (.docx) file which you can download and make further edits to before submitting to your instructor."),
                                 box(width = 12, status = "warning",
                                     solidHeader = TRUE,
                                     p(tags$b("WARNING:"), " The Shiny app will disconnect from the server if it is left idle for 15 minutes. If this happens you will lose all your inputs into the app. It is recommended to download the user input at the end of the class, but you can also download throughout the class."),
                                 ),
                                 p("Alternatively, you can download the questions as a Word (.docx) file  and record your answers there. If you opt for this option, you can hide the green question boxes by unchecking the box below."),
                                 checkboxInput("show_q1", "Show questions", value = TRUE),
                                 tags$style(type="text/css", "#stud_dl {background-color:#579277;color: white}"),
                                 conditionalPanel("output.handoutbuilt",
                                                  downloadButton(outputId = "stud_dl", label = "Download Student Handout"),
                                 )
                          ),
                        ), hr(),
                        #* Generate report buttons ====
                        fluidRow(
                          column(4,offset = 1,
                                 h3("Save your progress"),
                                 p(id = "txt_j", "If you run out of time to finish all the activities you can save your progress and return to it at a later date. Click the 'Download user input' button at the bottom of the page and a file 'module5_answers_ID_number.eddie' will download. Store this file in a safe place locally on your computer."),
                                 br(),
                                 h3("Resume your progress"),
                                 p(id = "txt_j", "To reload the app input you can upload the downloaded '.eddie' file below and it will populate your answers into the Shiny app."),
                                 fileInput("upload_answers", "Upload data", accept = c(".eddie", ".rds")), # B77C2C
                                 p(id = "txt_j", HTML(paste0(tags$b("Note:"), " You will need to navigate to tabs Objective 1 in Activity A after uploading your file for the site selection to load there."))),
                                 p(id = "txt_j", "Currently the plots do not save to the file.  If you generated plots during your last session, you will need to reload the data and reproduce the plots before generating your report.")
                          ),
                          column(4, offset = 1,
                                 introBox(
                                   h3("Generate Report"),
                                   p("This will take the answers you have input into this app and generate a Microsoft Word document (.docx) document with your answers which you can download and make further edits before submitting. Return here when you have completed the module."),
                                   actionButton("generate", "Generate Report (.docx)", icon = icon("file"), width = "190px", class = "btn-primary"
                                                # id = "dl_btn", # This is the only button that shows up when the app is loaded
                                                # style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                   ), br(), br(),
                                   data.step = 6, data.intro = help_text["finish", 1]
                                 ),
                                 tags$style(type="text/css", "#download {background-color:#579277;color: white}"),
                                 conditionalPanel(condition = "output.reportbuilt", # This button appears after the report has been generated and is ready for download.
                                                  downloadButton("download", "Download Report", width = "60px", style = "width:190px;"
                                                                 # style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                                  )), br(),
                                 h5(tags$b("Questions still to be completed:")),
                                 # verbatimTextOutput("check_list"),
                                 wellPanel(
                                   htmlOutput("check_list")
                                 )
                          )
                        ),
                        fluidRow(
                          hr(),
                          column(10, align = "left",
                                 box(id = "box1", width = 10, status = "primary",
                                     solidHeader = TRUE,
                                     fluidRow(
                                       column(8, offset = 1,
                                              h3("Before you start..."),
                                              p("Input your name and Student ID and this will be added to your final report."),
                                              textInput("name", "Name:"),
                                              textInput("id_number", "ID number:"),
                                              introBox(
                                                h3(tags$b("Think about it!")),
                                                p("Note: The size of these text boxes can be adjusted by clicking and dragging the bottom right of the text box."),
                                                textAreaInput2(inputId = qid[1], label = quest[qid[1], 1]),
                                                textAreaInput2(inputId = qid[2], label = quest[qid[2], 1]),
                                                data.step = 5, data.intro = help_text["questions", 1]
                                              )
                                       )
                                     ),

                                 ),
                          )
                        ),
                        hr(),
                        fluidRow(
                          column(6,
                                 h3("Data sources"),
                                 p(HTML(paste0('This module will introduce key concepts within Ecological forecasting through exploration of ', a(href = "https://www.neonscience.org/", "NEON (National Ecological Observation Network) data", target = "_blank"), ", building a model, and then generating a short-term ecological forecast.")))
                          ),
                          column(6, align = "center",
                                 a(
                                   href = "https://www.neonscience.org/",
                                   img(src = "NSF-NEON-logo.png", title = "NEON - NSF logo"), target = "_blank"
                                   )
                                 )
                          )
                        ),

               # 4. Site Selection ----
               tabPanel(title = "Site Selection", value = "mtab4",
                        tags$style(".nav-tabs {
  background-color: #DDE4E1;
  border-color: #FFF;

}

.nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs-custom .nav-tabs li.active a {
background-color: #4D6A5C;
border-color: #FFF;
}

.nav-tabs-custom .nav-tabs li.active {
    border-top-color: #FFF;
    background-color: #4D6A5C;
}"),
                        # tags$style(type="text/css", "body {padding-top: 65px;}"),
                        img(src = "project-eddie-banner-2020_green.png", height = 100,
                            width = 1544, top = 5),
                        fluidRow(
                          column(12,
                                 h3("Site Selection: Select a NEON site and visualize the data"),
                                 p("Complete objectives 1-2 to familiarize yourself with the data from your selected site and learn about the data you will be using.")
                          )
                        ),

                        #* Objective 1 - Select and view site ====
                        tabsetPanel(id = "tabseries1",
                                    tabPanel(title = "Objective 1 - Select and view a NEON site",

                                             value = "obj1", id = "wh_link",
                                             tags$style("outline: 5px dotted green;"),
                                             #* Objective 1 ====
                                             # introBox(
                                               fluidRow(
                                                 column(12,
                                                        wellPanel(style = paste0("background: ", obj_bg),
                                                                  h3("Objective 1 - Select a Site"),
                                                                  p(module_text["obj_01", ])
                                                        )
                                                 )
                                               ),
                                               # data.step = 4, data.intro = help_text["objectives", 1], data.position = "top"),
                                             #** NEON Map ====
                                             fluidRow(
                                               #** NEON Intro ----
                                               column(4,
                                                      h2("Site Description"),
                                                      p("Select a site in the table to highlight on the map"),
                                                      conditionalPanel("input.row_num > 25",
                                                                       selectizeInput("row_num", "Select row",
                                                                                      choices = 1:nrow(neon_sites_df),
                                                                                      options = list(
                                                                                        placeholder = 'Please select a row',
                                                                                        onInitialize = I('function() { this.setValue(""); }')),
                                                                       )
                                                      ),
                                                      DTOutput("table01"),
                                                      p(tags$b("Click 'View latest photo' to see the latest image from the webcam on site (this may take 10-30 seconds).")),
                                                      actionButton("view_webcam", label = "View latest photo", icon = icon("eye"))
                                               ),
                                               #** Site map ----
                                               column(4,
                                                      h2("Map of NEON sites"),
                                                      wellPanel(
                                                        leafletOutput("neonmap")
                                                      )
                                               )

                                               ,
                                               #** Site photo ----
                                               column(4,
                                                      h2("Phenocam"),
                                                      # textOutput("prompt1"),
                                                      wellPanel(
                                                        imageOutput("pheno"),
                                                        p(id = "txt_j", module_text["phenocam", ])
                                                      )
                                               )
                                             ), br(),
                                        #      span(textOutput("site_name1"), style = "font-size: 22px;
                                        # font-style: bold;"),
                                             fluidRow(
                                               wellPanel(
                                                 h4(tags$b("About Site")),
                                                 uiOutput("site_html"),
                                                 textOutput("prompt2"),
                                                 htmlOutput("site_link")
                                               ),
                                             ),
                                             fluidRow(
                                               column(10, align = "left",
                                                      box(id = "box3", width = 10, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(7, offset = 1,
                                                                   h3("Questions"),
                                                                   h4(quest[qid[3], 1]),
                                                                   p("If the information for your lake is not on the NEON website then you can input NA (Not Available) into the text box.")
                                                            )
                                                          ),
                                                          fluidRow(
                                                            column(4, offset = 1, align = "left", style = paste0("background: ", ques_bg),
                                                                   textInput(inputId = qid[4], label = quest[qid[4], 1] , width = "90%"),
                                                                   textInput(inputId = qid[5], label = quest[qid[5], 1], width = "90%"),
                                                                   textInput(inputId = qid[6], label = quest[qid[6], 1], width = "90%")
                                                            ),
                                                            column(4, offset = 1, align = "left", style = paste0("background: ", ques_bg),
                                                                   textInput(inputId = qid[7], label = quest[qid[7], 1] , width = "90%"),
                                                                   textInput(inputId = qid[8], label = quest[qid[8], 1], width = "90%"),
                                                                   textInput(inputId = qid[9], label = quest[qid[9], 1], width = "90%")
                                                            )
                                                          )
                                                      )
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(5, offset = 1,
                                                      h3("Next step"),
                                                      p("We will explore the data which has been measured at this site by NEON."))
                                             )
                                    ),
                                    #* Objective 2 - Explore data
                                    tabPanel(title = "Objective 2 - Explore data",  value = "obj2",
                                             #* Objective 2 - Explore the Data ====
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 2 - Explore the Data"),
                                                                # p(id = "txt_j", module_text["obj_02", ]),
                                                                p("If there are some variables which you are not familiar with, visit the ", a(href = "https://data.neonscience.org/home", "NEON Data Portal", target = "_blank"), "and click 'Explore Data Products' to learn more about how the data are collected.")
                                                      )
                                               )
                                             ),
                                             fluidRow(
                                               column(8, offset = 2,
                                                      h3("Variable descriptions"),
                                                      DT::DTOutput("var_desc")
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               #** Data Table ----
                                               column(4,
                                                      h3("Data Table"),
                                                      p("This is a Shiny data table. It is interactive and allows you to navigate through the data table by searching or clicking through the different pages."),
                                                      DT::DTOutput("neon_datatable")
                                               ),
                                               #** Plot of data ----
                                               column(8,
                                                      h3("Data Plot"),
                                                      p("All plots in this Shiny app are generated using Plotly. This allows you to hover your mouse over the plot to get information from each of the plots. You can inspect the data closely by clicking and zooming into particular areas. There is a tool box at the top of the plot which has the selection function required for Q6."),
                                                      selectizeInput("view_var", "Select variable",
                                                                     choices = unique(neon_vars$Short_name),
                                                                     options = list(
                                                                       placeholder = 'Please select a variable',
                                                                       onInitialize = I('function() { this.setValue(""); }')),
                                                      ),
                                                      plotlyOutput("var_plot"),
                                                      useShinyjs(),  # Set up shinyjs
                                                      wellPanel(
                                                        h4("Variable Description"),
                                                        textOutput("txt_out")
                                                      )
                                               )
                                             ), hr(),
                                             fluidRow(
                                               column(10, offset = 1,
                                                      box(id = "box4", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   textAreaInput2(inputId = qid[10], label = quest[qid[10], ], width = "90%"),
                                                                   textAreaInput2(inputId = qid[11], label = quest[qid[11], ], width = "90%")
                                                                   )
                                                            )
                                                          )
                                                      )
                                               ),
                                             hr(),
                                             fluidRow(
                                               column(5, offset = 1,
                                                      h3("Next step"),
                                                      p("We will build models that will allow us to predict water temperature."))
                                               )
                                             )
                                    ),
                        ),

               # 5. Activity A ----
               tabPanel(title = "Activity A", value = "mtab5",
                        img(src = "project-eddie-banner-2020_green.png", height = 100,
                            width = 1544, top = 5),
                        fluidRow(
                          column(12,
                                 wellPanel(style = paste0("background: ", obj_bg),
                                           h2("Activity A - Build A Model With Uncertainty"),
                                           p(module_text["act_A", ])
                                 )
                          ),
                        ),
                        tabsetPanel(id = "tabseries2",
                                    #* Objective 3 - Build a water temperature model ====
                                    tabPanel(title = "Objective 3 - Build a water temperature model", value = "obj3",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 3 - Build a water temperature model"),
                                                                p(id = "txt_j", module_text["obj_03", ])
                                                      ))
                                             ),
                                             fluidRow(
                                               column(4,
                                                      h3("Water Temperature"),
                                                      p(id = "txt_j", "Water temperature exerts a major influence on biological activity and growth, has an effect on water chemistry, can influence water quantity measurements, and governs the kinds of organisms that live in water bodies."),
                                                      p(id = "txt_j", "Freshwater ecosystems are currently experiencing a multitude of stressors such as landuse change and climate change."),
                                                      p(id = "txt_j", "Being able to predict how such systems can change in the short-term (up to 7-days into the future) will provide natural resource managers with critical information to take pro-active actions to prevent degradation of water quality.")
                                                      ),
                                               column(8,
                                                     img(src = "lake_image2.jpg", height = "100%", id = "bla_border",
                                                         width = "100%", tags$style("border: solid 2px black;"))
                                                     )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(8, offset = 1,
                                                      box(id = "box4", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   textAreaInput2(inputId = qid[12], label = quest[qid[12], ], width = "90%"),
                                                                   )
                                                            )
                                                          )
                                                      )
                                               ),
                                             hr(),
                                             fluidRow(
                                               column(5,
                                                      h3("Key terms & Definitions"),
                                                      p("Before we begin generating an ecological forecast with uncertainty we will define some key terms, definitions and concepts relevant to ecology and ecological forecasting.")
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(5, offset = 1,
                                                      p("What is an ecological model?"),
                                                      tags$ul(
                                                        tags$li(module_text["model1", ])
                                                      ),
                                                      p("What is a parameter"),
                                                      tags$ul(
                                                        tags$li(module_text["parameter", ])
                                                      ),
                                                      p("What is a parameter distribution?"),
                                                      tags$ul(
                                                        tags$li(module_text["distribution", ])
                                                      )
                                               ),
                                               column(5, offset = 1,
                                                      p("What is a linear relationship?"),
                                                      tags$ul(
                                                        tags$li(module_text["linear_relationship", ])
                                                      ),
                                                      p("In our example, water temperature is the dependent variable and air temperature is the independent variable."),
                                                      p("What is model error"),
                                                      tags$ul(
                                                        tags$li(module_text["mod_error", ])
                                                      ),
                                                      p("What is a parameter distribution?"),
                                                      tags$ul(
                                                        tags$li(module_text["distribution", ])
                                                        )
                                                      ) #,
                                               # column(4, offset = 0, align = "center",
                                               #        h3("Key Figures",
                                               #           align = "center"),
                                               #        h5("Click the arrows to navigate through the slides", align = "center"),
                                               #        wellPanel(
                                               #          # slickROutput("slides", width = "600px", height = "450px")
                                               #        )
                                               # )
                                             ),
                                             hr(),
                                             #* Linear regression ----
                                             fluidRow(
                                               column(6,
                                                      h3("Investigate variable relationships"),
                                                      p("We will explore the relationship between air temperature and surface water temperature for a lake site."),
                                                      p("First, we will look at a time series of the real air and water temperature data measured at the lake you chose in the “Site selection” tab."),
                                                      actionButton("plot_airt_swt", "Plot"),
                                                      radioButtons(qid[13], quest[qid[13], ], choices = c("Yes", "No"), selected = character(0), inline = TRUE),
                                                      conditionalPanel("input.q7 == 'Yes'",
                                                                       p(tags$b("Good job!")),
                                                                       p("When there is a linear relationship we can use ", tags$b("linear regression"), " to model the variable."),
                                                                       br(),
                                                                       h4("Linear Regression"),
                                                                       div("The formula for a linear regression is: $$y = m \\times x + b$$"),
                                                                       p("In our case we will be using ", tags$b("air temperature"), " (atemp) to model ", tags$b("water temperature"), " (wtemp) using the following model:"),
                                                                       div("$$wtemp = m \\times atemp + b$$"),
                                                                       p("where the ", tags$b("parameters"), "of the model are ", tags$em("m"), "(the slope) and ", tags$em("b"), " (the intercept)."),
                                                                       # p(tags$b("R-squared"), module_text["r_squared", ]),
                                                                       # div("$$R^2 = \\frac{Explained\\ Variation}{Total\\ Variation} $$")
                                                                       p(tags$b("Root mean square error"), module_text["rmse", ]),
                                                                       p("The lower the RMSE, the better a given model is able to “fit” a dataset."),
                                                                       div("$$RMSE = \\sqrt{\\sum_{n}^{i=1}\\frac{(P_{i} - O_{i})^2 }n}$$"),
                                                                       p("where P is equal to the predicted value and O is equal to the observed value andn is equal to the total number of data points.")
                                                      ),
                                                      conditionalPanel("input.q7 == 'No'",
                                                                       p(tags$em("Are you sure?"))
                                                      )
                                               ),
                                               column(6,
                                                      h4("Time series of air temperature and water temperature"),
                                                      p("Click on “Plot” to graph the time series of the air temperature and the surface water temperature. Compare the seasonal cycles of both the dependent variable (air temperature) and independent variable (water temperature)."),
                                                      wellPanel(
                                                        plotOutput("airt_swt_plot")
                                                      )
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(3,
                                                      h3("Investigate how collecting more data affects your model"),
                                                      p("When monitoring a lake site, deciding on the sampling frequency is an important decision. Here you will investigate how four different frequencies of data collection can affect model performance (assessed by RMSE) and parameter estimates (m and b)."),
                                                      p("Fit a linear model with data which has been collected at different frequencies. Each model’s parameters will be added to the parameter table."),
                                                      p("Toggle the buttons below to select a frequency of data collection which you would use to build your linear regression model."),
                                                      # " Use the time series plot above to guide your selection of dates. If there are values that look like a sensor malfunction then you can omit them from the selection."),
                                                      p(tags$b("Data collection frequency")),
                                                      radioButtons("samp_freq", "One observation per:", choices = samp_freq2),
                                                      # uiOutput("date_slider1"),
                                                      actionButton("plot_airt_swt2", "Plot"),
                                                      actionButton("add_lm", "Get model parameters")
                                                      # p("Use the date slider to choose how much data is used to go into your model.")
                                               ),
                                               column(4,
                                                      p("For the linear regression model, ", tags$em("m"), " and ", tags$em("b"), "are ", tags$b("parameters.")),
                                                      p(tags$b("N"), "represents the number of data points used to estimate the linear regression."),
                                                      DTOutput("lr_DT", width = "100%"),
                                                      br(),
                                                      # p("You do not need to get the percentages exactly right, but close enough will work fine."),
                                                      br(),
                                                      wellPanel(
                                                        uiOutput("lm_mod")
                                                        )
                                                      ),
                                               column(5,
                                                      h3("Air temperature vs. water temperature"),
                                                      wellPanel(
                                                        plotlyOutput("airt_swt_plot_lines")#,
                                                      )
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(10, offset = 1,
                                                      box(id = "box2", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   textAreaInput2(inputId = qid[14], label = quest[qid[14], ], width = "90%")
                                                                   )
                                                            )
                                                          )
                                                      )
                                             ),
                                             fluidRow(
                                               column(8,
                                                      h3("Water temperature time series"),
                                                      p("When you add your models to the table, they will appear here as lines with colors corresponding to the plot above."),
                                                      p("Use the interactivity of the plots to zoom in at different times of the year to inspect closer."),
                                                      wellPanel(
                                                        plotlyOutput("lm_ts_plot")
                                                        )
                                                      ),
                                               column(4,
                                                      h3("Compare model performance"),
                                                      # h4("ADD TABLE WITH R2 VALUES"),
                                                      p(tags$b("Model performance measured with the R-squared value.")),
                                                      DTOutput("r2_tab"),
                                                      br(),
                                                      box(id = "box2", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   textAreaInput2(inputId = qid[15], label = quest[qid[15], ], width = "90%"),
                                                                   textAreaInput2(inputId = qid[16], label = quest[qid[16], ], width = "90%")
                                                                   )
                                                            )
                                                          )
                                                      )
                                               ),
                                             hr(),
                                             fluidRow(
                                               column(5, offset = 1,
                                                      h3("Next step"),
                                                      p("We will explore the parameters of each of these models and examine how including more data in our model affects parameter estimation."))
                                               )
                                             ),
                                    #* Objective 4 - Explore Parameter ====
                                    tabPanel(title = "Objective 4 - Explore Parameters", value = "obj4",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 4 - Explore Parameters"),
                                                                p(id = "txt_j", module_text["obj_04", ])
                                                                )
                                                      )
                                               ),
                                             #** Generate distributions for intercept & slope ----
                                             fluidRow(
                                               column(12,
                                                      h3("Generate distributions for intercept & slope")
                                               ),
                                               column(4,
                                                      p("When fitting the models to the data in the previous objective, a standard error of the estimated parameters is calculated."),
                                                      p(module_text["std_error", ]),
                                                      DTOutput("lr_DT2", width = "100%"),
                                                      br(),
                                                      # p("You must select rows in the data table before you can calculate the statistics."),
                                                      # actionButton("calc_stats", "Calculate!"),
                                                      # div(DTOutput("lr_stats"), style = "font-size: 50%; width: 50%"),
                                                      p("Generate plots of the normal distribution of the parameters (m and b) using the mean and standard deviation from the lines you created by selecting a row in the table and clicking 'Generate plot'."),
                                                      module_text["density_plots", ],
                                                      p("Density plots are a variation of histograms and they are better at determining the distribution shape.")
                                               ),
                                               column(8,
                                                      fluidRow(
                                                        column(12,
                                                               wellPanel(
                                                                 plotOutput("lr_param_dist_plot")
                                                                 )
                                                               ),
                                                        column(6, align = "center",
                                                                sliderInput("m_std", "Slope (m) - Std. Dev.", min = 0, max = 0.5, value = 0.25, step = 0.01),
                                                                actionButton("gen_lr_dist_plot", "Generate distributions")
                                                               # p("Note: When generating the plots for Q14-15, make sure to deselect the row in the model table. This will add the distributions drawn as 'User input'")
                                                                ),
                                                        column(6, align = "center",
                                                               sliderInput("b_std", "Intercept (b) - Std. Dev.", min = 0, max = 1, value = 0.5, step = 0.1)
                                                               ),
                                                        column(12,
                                                               br(), br(),
                                                               box(id = "box2", width = 12, status = "primary",
                                                                   solidHeader = TRUE,
                                                                   fluidRow(
                                                                     column(10, offset = 1,
                                                                            h3("Questions"),
                                                                            textAreaInput2(inputId = qid[17], label = quest[qid[17], ], width = "90%"),
                                                                            )
                                                                     )
                                                                   )
                                                               )
                                                        )
                                                      )
                                               ),
                                             # hr(),
                                             # #* Adding multiple lines ----
                                             # fluidRow(
                                             #   column(3,
                                             #          h3("Create multiple models"),
                                             #          p("Using the distributions you have created above, you are going to randomly create models by sampling values for the slope (m) and the intercept (b) from the distributions you have defined above for each parameter."),
                                             #          radioButtons("n_samp", "No. of samples", choices = c(10, 20, 50, 75, 100), selected = character(0)),
                                             #          actionButton("gen_lin_mods", "Add models"),
                                             #          p("Every time you click 'Add models' it will generate models randomly from the sample distributions.")
                                             #          # checkboxInput("add_dist", "Distribution plot"))
                                             #   ),
                                             #   column(3,
                                             #          DTOutput("lr_stats", width = "100%"),
                                             #          DTOutput("mb_samps", width = "100%")
                                             #   ),
                                             #   column(6,
                                             #          plotlyOutput("add_lin_mods"),
                                             #          conditionalPanel("input.gen_lin_mods >= 1",
                                             #                           p("Using the properties of a normal distribution, we can calculate the confidence intervals of these samples and add this to our plot."),
                                             #                           radioButtons("plot_type1", "Plot type", c("Line", "Distribution"),
                                             #                                        inline = TRUE)
                                             #          )
                                             #   )
                                             # ),
                                             # fluidRow(
                                             #   column(12,
                                             #          box(id = "box2", width = 12, status = "primary",
                                             #              solidHeader = TRUE,
                                             #              fluidRow(
                                             #                column(12,
                                             #                       h4("Questions")
                                             #                ),
                                             #                column(4,
                                             #                       textAreaInput2(inputId = "q13", label = quest["q13", ], width = "90%")
                                             #                ),
                                             #                column(4,
                                             #                       textAreaInput2(inputId = "q14", label = quest["q14", ], width = "90%")
                                             #                ),
                                             #                column(4,
                                             #                       textAreaInput2(inputId = "q15", label = quest["q15", ], width = "90%")
                                             #                       )
                                             #                )
                                             #              )
                                             #          )
                                             #   ),
                                             hr(),
                                             fluidRow(
                                               column(5, offset = 1,
                                                      h3("Next step"),
                                                      p("We will build alternative models with different structures."))
                                               )
                                             ),
                                    # #* (DEPRECATED) Objective 5 - Assess Model ====
                                    # tabPanel(title = "Objective 5 - Assess Model", value = "obj5",
                                    #          fluidRow(
                                    #            column(12,
                                    #                   wellPanel(style = paste0("background: ", obj_bg),
                                    #                             h3("Objective 5 - Assess Model"),
                                    #                             p(id = "txt_j", module_text["obj_05", ])
                                    #                             )
                                    #                   )
                                    #            ),
                                    #          #** Calculation model error - deterministic ----
                                    #          fluidRow(
                                    #            column(6,
                                    #                   h3("Calculating model error"),
                                    #                   p("If you look at our linear model, we can see that the points do not fall exactly on our model’s predicted line. We can compare our model results to actual observations to determine how “good” the model is. Select points on the graph and the table below will display how far off the model was from the observations by calculating model error (difference between modelled water temperature and observed temperature)."),
                                    #                   h4("Selected points"),
                                    #                   p("Select a point on the plot by clicking on it or select multiple points by clicking and dragging the selection pane."),
                                    #                   DTOutput("click_dt", width = "90%"),
                                    #                   br(),
                                    #                   actionButton("calc_err", "Calculate"),
                                    #                   wellPanel(
                                    #                     textOutput("mean_err")
                                    #                   )
                                    #            ),
                                    #            column(6,
                                    #                   plotOutput("mod_err_plot",
                                    #                              click = "mod_err_plot_click",
                                    #                              brush = brushOpts(
                                    #                                id = "mod_err_plot_brush"
                                    #                              )),
                                    #            )
                                    #          ),
                                    #          fluidRow(
                                    #            column(12,
                                    #                   box(id = "box2", width = 12, status = "primary",
                                    #                       solidHeader = TRUE,
                                    #                       fluidRow(
                                    #                         column(12,
                                    #                                h4("Questions"),
                                    #                                p("Calculate the model error (difference between modelled water temperature and observed water temperature) at varying intervals (0-10, 10-20, 20-30, 30-40 degC).")
                                    #                         ),
                                    #                         column(6,
                                    #                                textAreaInput2(inputId = "q21", label = quest["q21", ], width = "90%")
                                    #                         ),
                                    #                         column(6,
                                    #                                textAreaInput2(inputId = "q22", label = quest["q22", ], width = "90%")
                                    #                         )
                                    #                       ),
                                    #                       #* Calculation model error - probabilistic ----
                                    #                       fluidRow(
                                    #                         column(6,
                                    #                                h3("Calculating model error with uncertainty"),
                                    #                                p("In Objective X, we noticed that there were multiple models that could potentially fit our data and this allowed us to create a plot with confidence intervals. Now we will assess how many of these observations fall within our confidence intervals, giving us a measure of how 'good' our model is."),
                                    #                                p("Use the 'Lasso select' tool to highlight points outside of the confidence interval and input the number below."),
                                    #                                textOutput("sel_points"),
                                    #                                textOutput("total_points"),
                                    #                                numericInput("points_above", "Number of points above the confidence interval", 0, min = 0, max = 1000, step = 1),
                                    #                                numericInput("points_below", "Number of points below the confidence interval", 0, min = 0, max = 1000, step = 1),
                                    #                                textOutput("pct_inside"),
                                    #                                actionButton("calc_pct", "Calculate percentage points inside the confidence intervals."),
                                    #                                p("Does the percentage of points inside your confidence intervals match your intervals?"),
                                    #                                p("If not, why do you think that is?"),
                                    #                                p("If you were to go back and repeat this exercise, what would you do differently?")
                                    #                         ),
                                    #                         column(6,
                                    #                                plotlyOutput("mod_err_uc_plot"),
                                    #                                actionButton("clear_sel1", "Clear selection")
                                    #                                )
                                    #                         )
                                    #                       )
                                    #                   )
                                    #          )
                                    # ),
                                    #* Objective 5 - Improve Model for Forecasting ====
                                    tabPanel(title = "Objective 5 - Improve Model for Forecasting", value = "obj5",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 5 - Improve Model for Forecasting"),
                                                                p(id = "txt_j", module_text["obj_05", ])
                                                      )
                                               )
                                             ),
                                             fluidRow(
                                               column(6,
                                                      h3("Create a Forecast model"),
                                                      p("The model built in the Objective 3 uses the current air temperature to predict the current water temperature. But, if we want to make a forecast of future water temperature, we would be unable to use this model unless we used forecasted (future) air temperature."),
                                                      p("The simplest forecast model that we can create is to predict that tomorrow's water temperature will be the same as today’s water temperature. This is called a ", tags$b("persistence model.")),
                                                      wellPanel(
                                                        h4("Persistence model (Pers):"),
                                                        div("$$wtemp_{t+1} = wtemp_{t}$$"),
                                                        p("where t+1 = tomorrow and t = today.")
                                                      ),
                                                      p("Let's plot this model versus observations. Adjust the date slider below to choose which period to plot this model for. The R-squared value will be calculated for the plotted data."),
                                                      br(),
                                                      actionButton("plot_persist", "Plot"),
                                                      br(), br(),
                                                      box(id = "box2", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   textAreaInput2(inputId = qid[18], label = quest[qid[18], ], width = "90%"),
                                                                   )
                                                            ),
                                                          )
                                                      ),
                                               column(6,
                                                      wellPanel(
                                                        plotlyOutput("persist_plot"),
                                                        p(tags$b("Model performance:")),
                                                        uiOutput("persist_r2")
                                                      )
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(12,
                                                      h3("Build a ", tags$em("Better"), " model"),
                                               ),
                                               column(3,
                                                      # p("The model we have built depends on the current air temperature. But, if we want to make a forecast of water temperature, we would be unable to use this model unless we used forecasted air temperature."),
                                                      p("We are now going to build two models that we will use to forecast water temperature."),
                                                      tags$ol(
                                                        tags$li("A linear regression using today's water temperature to predict tomorrow's water temperature"),
                                                        tags$li("A multiple linear regression using today's water temperature and tomorrow's air temperature to predict tomorrow's water temperature")
                                                      ),
                                                      br()
                                               ),
                                               column(3,
                                                      p("Build the models below and see how well your model performs at predicting water temperature."),
                                                      wellPanel(
                                                        p("Models are of the form"),
                                                        div("$$y = \\beta _{1}x_{1} + \\beta _{2}x_{2} + ... + \\beta _{n}$$"),
                                                        p("where \\(\\beta_{n}\\) represents the parameters in the model, similarly to the slope in a linear regression model.")
                                                        ),
                                                      selectInput("mult_lin_reg_vars", "Select predictors", choices = lin_reg_vars$Name, multiple = TRUE),
                                                      h4("Fit model to data"),
                                                      p("Use a multiple linear regression model to estimate the parameters (\\(\\beta_{n}\\)) in your model below."),
                                                      wellPanel(
                                                        uiOutput("mult_lin_reg_eqn")
                                                      ),
                                                      br(),
                                                      actionButton("fit_mlr", "Fit model")
                                               ),
                                               column(6,
                                                      h4("Timeseries of Better Models"),
                                                      wellPanel(
                                                        plotlyOutput("mlr_ts_plot"),
                                                        br(),
                                                        uiOutput("mlr_mod")
                                                      ),
                                                      DTOutput("mlr_dt"),
                                                      br()
                                                      # p("To change the models in the table, you can select a row in the table then click 'Fit model' to add the model to that row.")
                                               )
                                             ),
                                             fluidRow(
                                               column(12,
                                                      box(id = "box2", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(12,
                                                                   h4("Questions")
                                                            )
                                                          ),
                                                          fluidRow(
                                                            column(4,
                                                                   textAreaInput2(inputId = qid[19], label = quest[qid[19], ], width = "90%")
                                                            ),
                                                            column(4,
                                                                   textAreaInput2(inputId = qid[20], label = quest[qid[20], ], width = "90%")
                                                            ),
                                                            column(4,
                                                                   textAreaInput2(inputId = qid[21], label = quest[qid[21], ], width = "90%")
                                                                   )
                                                            )
                                                          )
                                                      )
                                               ),
                                             hr(),
                                             fluidRow(
                                               column(5, offset = 1,
                                                      h3("Next step"),
                                                      p("We will use the models we have built to generate forecasts and explore different sources of uncertainty.")
                                                      )
                                               )
                                             )
                                    )
                        ),
               # 6. Activity B ----
               tabPanel(title = "Activity B", value = "mtab6",
                        img(src = "project-eddie-banner-2020_green.png", height = 100,
                            width = 1544, top = 5),
                        fluidRow(
                          column(12,
                                 wellPanel(style = paste0("background: ", obj_bg),
                                           h2("Activity B - Explore Forecast Uncertainty"),
                                           p(module_text["act_B", ])
                                 )
                          ),
                        ),
                        tabsetPanel(id = "tabseries3",
                                    #* Activity B - Overview ====
                                    tabPanel(title = "Overview", value = "obj6",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Overview"),
                                                                p(id = "txt_j", module_text["act_B_overview", ])
                                                                )
                                                      )
                                               ),
                                             fluidRow(
                                               column(6,
                                                      h3("Overview"),
                                                      p("We will now generate forecasts of water temperature."),
                                                      p("We will use the models that you developed in Activity A to explore four different types of uncertainty associated with ecological forecasts."),
                                                      tags$ol(
                                                        tags$li("Process uncertainty"),
                                                        tags$li("Parameter uncertainty"),
                                                        tags$li("Initial conditions uncertainty"),
                                                        tags$li("Driver uncertainty")
                                                      )
                                               ),
                                               column(6, align = "center",
                                                      img(src = "model_UC_draft_v2.png", height = "60%", id = "bla_border",
                                                          width = "60%", tags$style("border: solid 2px black;"))
                                                      )
                                               ),
                                             hr(),
                                             #** View Weather Forecast Data ----
                                             fluidRow(
                                               column(6,
                                                      h3("Weather forecast data"),
                                                      p("We just received a weather forecast data from NOAA. It is a 7-day forecast of air temperature at our site. We will need this data for our forecasts as some of our models depend on forecasted air temperature. Click the button below to view it."),
                                                      actionButton("view_at_fc", "View forecast"),
                                                      p("With this air temperature forecast we can use the models that we built in Activity A that require air temperature, to forecast water temperature:")
                                               ),
                                               column(6,
                                                      h4("Air temperature forecast"),
                                                      wellPanel(
                                                        plotlyOutput("airt1_fc_plot")
                                                        )
                                               ),
                                             ),
                                             fluidRow(
                                               #** Deterministic Forecast ----
                                               column(6,
                                                      h3("Deterministic Forecasts"),
                                                      p("Now we will generate ", tags$b("deterministic"), " forecasts with each of our models. We will use the use the forecasted driver data (air temperature) for the models that use it as a driver."),
                                                      p("Select a model from the table below and then load the driver data and run the forecast."),
                                                      actionButton("load_mods", "Load models"),
                                                      p("Note: If there are '$' in the table below, click on one of the rows and this will re-render the table."),
                                                      DTOutput("mod_selec_tab1a"),
                                                      br(),
                                                      actionButton("run_wtemp_fc1a", "Run forecast"),
                                                      br(), br(),
                                                      box(id = "box2", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   textAreaInput2(inputId = qid[22], label = quest[qid[22], ], width = "90%"),
                                                            )
                                                          ),
                                                      )
                                               ),
                                               column(6,
                                                      h4("Water temperature forecast"),
                                                      wellPanel(
                                                        plotlyOutput("wtemp_fc1a")
                                                      ),
                                                      wellPanel(
                                                        uiOutput("sel_mod1a"),
                                                        textOutput("txt_fc_out1a")
                                                        )
                                                      )
                                               ),
                                             br(),
                                             fluidRow(
                                               column(6,
                                                      h4("What is wrong with these forecasts?"),
                                                      p("Using a deterministic forecast (e.g. a forecast which is one single line) is guranteed to be wrong and ignores the uncertainty that is inherently associated with the future."),
                                                      p("There are many things which contribute to uncertainty when generating the forecast."),
                                                      p("A forecast should represent the range of potential outcomes and the ", tags$b("likelihood"), " of such outcomes occurring."),
                                                      p("Therefore, we need to generate a ", tags$b("probabilistic"), " forecast which represents both the range of outcomes and also the likelihood of each.")
                                                      )
                                               )
                                             ),
                                    #* Objective 6 - Process Uncertainty ====
                                    tabPanel(title = "Objective 6 - Process Uncertainty", value = "obj7",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 6 - Process Uncertainty"),
                                                                p(id = "txt_j", module_text["obj_06", ])
                                                                )
                                                      )
                                             ),
                                             #** Process Uncertainty ----
                                             fluidRow(
                                               column(12,
                                                      h3("Process Uncertainty")
                                                      )
                                             ),
                                             fluidRow(
                                               column(4,
                                                      p(module_text["proc_uc", ]),
                                                      p("Across is a description of our \"simple\" water temperature model (1). But we know that water temperature is variable and that our model has simplified these."),
                                                      p("To account for the uncertainty these simplifications introduce to our model we can add in process noise (", tags$em("W"), ") to our model at each time step (2)."),
                                                      p("Water temperature tomorrow is equal to water temperature today ", tags$b("plus"), " some noise ", tags$em("(W)"), "."),
                                                      p("where process noise is equal to a random number with a mean of zero and a standard deviation."),
                                               ),
                                               column(8,
                                                      h5("Click the arrows to navigate through the slides", align = "center"),
                                                      wellPanel(
                                                        slickROutput("proc_uc_slides", width = "640px", height = "360px")
                                                        )
                                                      ),
                                               # column(6,
                                               #        h4("Model Equations"),
                                               #        wellPanel(
                                               #          p(withMathJax("$$ wtemp_{t+1} = model\\ (1) $$")),
                                               #        ),
                                               #        br(),
                                               #        wellPanel(
                                               #          p(withMathJax("$$ wtemp_{t+1} = model + W_t\\ (2) $$"))
                                               #          )
                                               #        )
                                               ),
                                             fluidRow(
                                               column(6,
                                                      h4("Forecast with Process Uncertainty"),
                                                      p("First we will explore how the different models respond to the addition of process uncertainty. Run each of the models with differing numbers of members and observe how the forecast outcome changes. We will generate forecasts from today (Sep 25th), which is represented in the plots as the vertical dashed line, for seven days into the future (Oct 2nd)."),
                                                      p("Select a model from the table below and then load the driver data and run the forecast."),
                                                      p("Note: If there are '$' in the table below, click on one of the rows and this will re-render the table."),
                                                      DTOutput("mod_selec_tab2"),
                                                      br(),
                                                      # actionButton("load_driv2", "Load driver data"),
                                                      actionButton("run_wtemp_fc2", "Run forecast"),
                                                      br(),
                                                      p("To account for uncertainty in the noise, we can run the model multiple times with random noise added to each model run. More noise is associated with high process uncertainty, and vice versa. Using multiple model runs is called an ", tags$b("ensemble."), " Each individual run is referred to as an ensemble ", tags$b("member."), "Forecasters typically run tens to hundreds of ensemble members to build uncertainty in their forecasts."),
                                                      p("Using the slider below, adjust the number of members to see how process uncertainty changes with time into the future (e.g. forecast horizon)."),
                                                      sliderInput("n_mem2", "No. of members", min = 5, max = 100, value = 5, step = 5),
                                                      box(id = "box2", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   textAreaInput2(inputId = qid[23], label = quest[qid[23], ], width = "90%"),
                                                                   )
                                                            )
                                                          )
                                               ),
                                               column(6,
                                                      wellPanel(
                                                        plotlyOutput("wtemp_fc2")
                                                      ),
                                                      wellPanel(
                                                        uiOutput("sel_mod2"),
                                                        textOutput("txt_fc_out2")
                                                      ),
                                                      conditionalPanel("input.run_wtemp_fc2 > 0",
                                                                       p("Using the properties of a normal distribution, we can calculate the confidence intervals of these samples and use this to visualize uncertainty in our forecast."),
                                                                       radioButtons("plot_type2", "Plot type", c("Line", "Distribution"),
                                                                                    inline = TRUE),
                                                                       p("Click on items in the legend to show/hide them from the plot.")
                                                      )
                                               )
                                             ),
                                             hr(),
                                    ),
                                    #* Objective 7 - Parameter Uncertainty ====
                                    tabPanel(title = "Objective 7 - Parameter Uncertainty", value = "obj8",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 7 - Parameter Uncertainty"),
                                                                p(id = "txt_j", module_text["obj_07", ])
                                                      )
                                               )
                                             ),
                                             #** Parameter Uncertainty ----
                                             fluidRow(
                                               column(4,
                                                      h3("Parameter Uncertainty"),
                                                      p(module_text["param_uncert", ]),
                                                      p("With traditional modelling efforts, people general find one set of the 'best fit' parameters and use them to predict with their model. This method does not account for the uncertainty around the estimation of these parameters."),
                                                      p("There is often the possibility that different parameter sets can yield similar metrics of model performance e.g. similar R-squared values."),
                                                      p("Using ", tags$b("parameter distributions"), " allows for a better representation of the potential predicted outcomes e.g. better quantification of the uncertainty.")
                                               ),
                                               column(8,
                                                      h5("Click the arrows to navigate through the slides", align = "center"),
                                                      wellPanel(
                                                        slickROutput("param_uc_slides", width = "640px", height = "360px")
                                                      )
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(4,
                                                      h3("Forecasting with Parameter Uncertainty"),
                                                      p("We know, from Objective 4, that when we used different amounts of data (e.g. ", tags$em("N"), " data points) to build our model, we got slightly different estimations of our model parameters ", tags$em("m"), " and ", tags$em("b"), "."),
                                                      p("To account for this ", tags$b("uncertainty"), " we built distributions for the parameters, from which we then drew samples from.")
                                                      ),
                                               column(8,
                                                      plotOutput("param_fcast3b")
                                                      )
                                               ),
                                             fluidRow(
                                               column(6,
                                                      h4("Generate Parameter Distributions"),
                                                      p("We will generate parameter distributions for each of our models and sample these distributions to create an ", tags$b("ensemble forecast"), " using multiple different potential parameter sets."),
                                                      p("Select a model from the table below and then generate parameter distributions, then load the driver data and then run the forecast."),
                                                      p("Note: If there are '$' in the table below, click on one of the rows and this will re-render the table."),
                                                      DTOutput("mod_selec_tab3"),
                                                      br(),
                                                      actionButton("gen_params3b", "Generate parameters"),
                                                      actionButton("run_wtemp_fc3b", "Run forecast"),
                                                      p("We will use 100 parameter sets in the forecast ensemble. These will be sampled from the distributions generated above.")
                                               ),
                                               column(6,
                                                      h4("Parameter Distributions"),
                                                      wellPanel(
                                                        plotOutput("param_dist3b")
                                                      )
                                               )
                                             ),
                                             fluidRow(
                                               column(6,
                                                      h4("Forecast with Parameter Uncertainty"),
                                                      # p("Select the number of parameters to be used in the forecast ensemble. These will be sampled from the distributions generated above."),
                                                      # sliderInput("n_mem3b", "No. of members", min = 5, max = 100, value = 5, step = 5),
                                                      box(id = "box2", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   textAreaInput2(inputId = qid[24], label = quest[qid[24], ], width = "90%"),
                                                                   textAreaInput2(inputId = qid[25], label = quest[qid[25], ], width = "90%")
                                                                   )
                                                            )
                                                          )
                                                      ),
                                               column(6,
                                                      h4("Water Temperature Forecasts"),
                                                      wellPanel(
                                                        plotlyOutput("wtemp_fc3b")
                                                      ),
                                                      wellPanel(
                                                        uiOutput("sel_mod3b"),
                                                        textOutput("txt_fc_out3b")
                                                      ),
                                                      conditionalPanel("input.run_wtemp_fc3b > 0",
                                                                       p("Using the properties of a normal distribution, we can calculate the confidence intervals of these samples and use this to visualize uncertainty in our forecast."),
                                                                       radioButtons("plot_type3b", "Plot type", c("Line", "Distribution"),
                                                                                    inline = TRUE)
                                                                       )
                                                      )
                                               ),
                                             hr(),
                                             fluidRow(
                                               column(5, offset = 1,
                                                      h3("Next step"),
                                                      p("We will explore what initial conditions uncertainty is and how it can affect our forecasts of water temperature.")
                                                      )
                                               )
                                             ),
                                    #* Objective 8 - Initial Conditions Uncertainty ====
                                    tabPanel(title = "Objective 8 - Initial Conditions Uncertainty", value = "obj9",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 8 - Initial Conditions Uncertainty"),
                                                                p(id = "txt_j", module_text["obj_08", ])
                                                                )
                                                      )
                                               ),
                                             fluidRow(
                                               column(4,
                                                      h3("Initial Condition Uncertainty"),
                                                      p(module_text["init_uncert", ]),
                                                      p("Even though we have measurements of water temperature from our lake, we know that water temperature varies throughout the day so this measurement might not capture exactly the temperature in our lake at this time.")
                                               ),
                                               column(8,
                                                      h5("Click the arrows to navigate through the slides", align = "center"),
                                                      wellPanel(
                                                        slickROutput("ic_uc_slides", width = "640px", height = "360px")
                                                        )
                                                      )
                                               ),
                                             hr(),
                                             fluidRow(
                                               column(4,
                                                      h4("Forecasting with Initial Conditions Uncertainty"),
                                                      p("To account for initial condition uncertainty we can generate a distribution around this value and then run our model with slightly different initial conditions to account for this uncertainty."),
                                                      p("Use the slider below to adjust the standard deviation and then generate a normal distribution around the observation"),
                                                      sliderInput("ic_uc", "Standard deviation", min = 0.05, max = 0.5, value = 0.1, step = 0.05),
                                                      actionButton("gen_ic", "Generate distribution")
                                                      ),
                                               column(4,
                                                      h4("Recent Observations"),
                                                      wellPanel(
                                                        plotlyOutput("ic_obs_plot")
                                                        )
                                                      ),
                                               column(4,
                                                      h4("Distribution of Initial Conditions"),
                                                      wellPanel(
                                                        plotOutput("ic_uc_plot")
                                                        )
                                                      )
                                             ),
                                             fluidRow(
                                               column(6,
                                                      p("Now we will generate forecasts with different initial conditions for each of our models."),
                                                      p("Select a model from the table below and then load the driver data and run the forecast."),
                                                      p("Note: If there are '$' in the table below, click on one of the rows and this will re-render the table."),
                                                      DTOutput("mod_selec_tab4"),
                                                      br(),
                                                      # actionButton("load_driv4", "Load driver data"),
                                                      actionButton("run_wtemp_fc4", "Run forecast"),
                                                      br(),
                                                      p("We will use 100 different initial condtions in the forecast ensemble. These will be sampled from the distribution generated above."),

                                                      # sliderInput("n_mem4", "No. of members", min = 1, max = 100, value = 5, step = 5),
                                                      br(),
                                                      box(id = "box2", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   textAreaInput2(inputId = qid[26], label = quest[qid[26], ], width = "90%"),
                                                                   textAreaInput2(inputId = qid[27], label = quest[qid[27], ], width = "90%"),
                                                                   textAreaInput2(inputId = qid[28], label = quest[qid[28], ], width = "90%")
                                                                   )
                                                            )
                                                          )
                                                      ),
                                               column(6,
                                                      wellPanel(
                                                        plotlyOutput("wtemp_fc4")
                                                      ),
                                                      wellPanel(
                                                        uiOutput("sel_mod4"),
                                                        textOutput("txt_fc_out4"),
                                                        radioButtons("plot_type4", "Plot type", c("Line", "Distribution"),
                                                                     inline = TRUE)
                                                        )
                                                      )
                                               ),
                                             hr(),
                                             fluidRow(
                                               column(5, offset = 1,
                                                      h3("Next step"),
                                                      p("We will examine how driver uncertainty can affect our forecast of water temperature.")
                                                      )
                                               )
                                             ),
                                    #* Objective 9 - Driver Uncertainty ====
                                    tabPanel(title = "Objective 9 - Driver Uncertainty", value = "obj10",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 9 - Driver Uncertainty"),
                                                                p(id = "txt_j", module_text["obj_09", ])
                                                      )
                                               )
                                             ),
                                             fluidRow(
                                               column(4,
                                                      h3("Driver Uncertainty"),
                                                      p(module_text["driver_uncert", ]),
                                                      p("The driver variable for our model is air temperature. To generate a forecast of future water temperature, we need to use forecasted driver data to drive the model."),
                                                      br(),
                                                      p("Luckily for us, the National Oceanic and Atmospheric Administration (NOAA) generate ensemble forecasts"),
                                                      h4("NOAA Forecast data"),
                                                               p(id = "txt_j", "Here we will load in an air temperature forecast which has been generate from the ", a(href = "https://www.ncdc.noaa.gov/data-access/model-data/model-datasets/global-ensemble-forecast-system-gefs", "NOAA Global Ensemble Forecast System", target = "_blank"), " for the NEON site you chose in Activity A."),
                                                               img(src = "noaa_logo.jpg", height = "20%",
                                                                   width = "20%", align = "right")
                                                      ),
                                               column(8,
                                                      h5("Click the arrows to navigate through the slides", align = "center"),
                                                      wellPanel(
                                                        slickROutput("driver_uc_slides", width = "640px", height = "360px")
                                                        )
                                                      )
                                               ),
                                             fluidRow(
                                               column(6,
                                                      p("Load the NOAA ensemble forecast for air temperature below."),
                                                      actionButton("load_noaa_at", "Load forecast"),
                                                      verbatimTextOutput("noaa_at_loaded"),
                                                      p("You can adjust the number of ensemble members plotted below. These are what you will use to drive your model."),
                                                      numericInput("noaa_n_mems", "Number of members (0-30)", 30, 1, 30),
                                                      p("Note: If there are '$' in the table below, click on one of the rows and this will re-render the table."),
                                                      DTOutput("mod_selec_tab5"),
                                                      br(),
                                                      actionButton("run_wtemp_fc5", "Run forecast"),
                                                      br(),
                                                      wellPanel(
                                                        uiOutput("sel_mod5"),
                                                        textOutput("txt_fc_out5"),
                                                        radioButtons("plot_type5", "Plot type", c("Line", "Distribution"),
                                                                     inline = TRUE)
                                                        )
                                                      ),
                                               column(6,
                                                      wellPanel(
                                                        plotlyOutput("airt_fc5")
                                                      ),
                                                      wellPanel(
                                                        plotlyOutput("wtemp_fc5")
                                                        )
                                                      )
                                               ),

                                             fluidRow(
                                               column(10, offset = 1,
                                                      box(id = "box2", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   textAreaInput2(inputId = qid[29], label = quest[qid[29], ], width = "90%"),
                                                                   textAreaInput2(inputId = qid[30], label = quest[qid[30], ], width = "90%")
                                                                   )
                                                            )
                                                          )
                                                      )
                                               ),
                                             hr(),
                                             fluidRow(
                                               column(5, offset = 1,
                                                      h3("Next step"),
                                                      p("We will summarize the key sources of uncertainty in our forecasts and discuss them as a group.")
                                                      )
                                               )
                                             ),
                                    #* Activity B - Summary ====
                                    tabPanel(title = "Summary", value = "obj11",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Summary"),
                                                                p(id = "txt_j", module_text["act_B_summ", ]),
                                                                p("Remember, the Shiny app will disconnect if you leave it idle for 10 minutes, so make sure to download your '.eddie' file at the bottom of the page to checkpoint your progress.")
                                                                )
                                               ),
                                             ),
                                               fluidRow(
                                                 column(5, offset = 1,
                                                      h4("Discussion Questions"),
                                                      p(tags$em("Use the figures below to answer the questions.")),
                                                      tags$line()
                                                 )
                                               ),
                                             fluidRow(
                                               column(4,
                                                      tags$ul(
                                                        tags$li(id = "txt_j", module_text["actB_q1", ]),
                                                        textAreaInput2("disc_q1", "")
                                                      )
                                               ),
                                               column(4,
                                                      tags$ul(
                                                        tags$li(id = "txt_j", module_text["actB_q2", ]),
                                                        textAreaInput2("disc_q2", "")
                                                      )
                                               ),
                                               column(4,
                                                      tags$ul(
                                                        tags$li(id = "txt_j", module_text["actB_q3", ]),
                                                        textAreaInput2("disc_q3", "")
                                                        )
                                                      )
                                               ),
                                             fluidRow(
                                               column(6,
                                                      h3("Process Uncertainty"),
                                                      wellPanel(
                                                        plotOutput("proc_uc_summ")
                                                        )
                                                      ),
                                               column(6,
                                                      h3("Parameter Uncertainty"),
                                                      wellPanel(
                                                        plotOutput("param_uc_summ")
                                                        )
                                                      ),
                                               column(6,
                                                      h3("Initial Conditions Uncertainty"),
                                                      wellPanel(
                                                        plotOutput("ic_uc_summ")
                                                        )
                                                      ),
                                               column(6,
                                                      h3("Driver Uncertainty"),
                                                      wellPanel(
                                                        plotOutput("driver_uc_summ")
                                                        )
                                                      )
                                             ),
                                             fluidRow(
                                               column(12,
                                                      h2("Completed Activity B!"),
                                                      p("This is the end of Activity B. If you have been inputting your answers into the app, it is recommended to return to the 'Introduction' tab and generate the final report before completing Activity C. Otherwise you could lose your progress.")
                                                      )
                                               )
                                             )
                                    )
                        ),

               # 6. Activity C ----
               tabPanel(title = "Activity C", value = "mtab7",
                        img(src = "project-eddie-banner-2020_green.png", height = 100,
                            width = 1544, top = 5),
                        fluidRow(
                          column(12,
                                 wellPanel(style = paste0("background: ", obj_bg),
                                           h2("Activity C - Managing Uncertainty"),
                                           p(module_text["act_C", ])
                                           )
                                 ),
                          ),
                        tabsetPanel(id = "tabseries4",
                                    #* Objective 10 - Quantify Uncertainty ====
                                    tabPanel(title = "Objective 10 - Quantify Uncertainty", value = "obj12",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 10 - Quantify Uncertainty"),
                                                                p(id = "txt_j", module_text["obj_10", ])
                                                      )
                                               )
                                             ),
                                             fluidRow(
                                               column(4,
                                                      h3("Quantifying Uncertainty"),
                                                      p(id = "txt_j", module_text["uc_quant1", 1]),
                                                      p("So far we have explored where uncertainty comes from, now we will quantify the uncertainty at each forecast horizon."),
                                                      p("We will generate forecasts with two of the models including all of the sources of uncertainty. In reality, when you generate an ecological forecast you will want to include and account for all sources of uncertainty."),
                                                      br(),
                                                      h3("Think, Pair, Share!"),
                                                      p(id = "txt_j", module_text["tps1", 1]),
                                                      selectizeInput("mod_selec_tot_fc", "Select two models for the next exercise:",
                                                                     choices = mod_names,
                                                                     options = list(
                                                                       maxItems = 2,
                                                                       placeholder = 'Please select a pair of models',
                                                                       onInitialize = I('function() { this.setValue(""); }'))
                                                      )
                                               ),
                                               column(6, align = "center", offset = 2,
                                                      img(src = "tot_uc2.png", height = "60%",
                                                          width = "60%", align = "center")
                                               )
                                             ),
                                             hr(),
                                             #** Model A - UC partitioning ----
                                             fluidRow(
                                               column(4,
                                                      h4("Model 1"),
                                                      textOutput("modA_txt"),
                                                      uiOutput("modA_eqn"),
                                                      # p("Select sources of uncertainty to include in your forecast below"),
                                                      # checkboxGroupInput("fc_uncert", "Sources of Uncertainty:", uc_sources),
                                                      # radioButtons("fc_uncertA", "Sources of Uncertainty:", uc_sources, selected = character(0)),
                                                      conditionalPanel("input.fc_uncertA == 'Total'",
                                                                       p("Total uncertainty includes all four sources of uncertainty (Process, Parameter, Initial Conditions and Driver).")
                                                      ),
                                                      sliderInput("tot_fc_mem", "Forecast members", min = 10, max = 1000, value = 100, step = 10),
                                                      actionButton("run_tot_fcA", "Run forecast"),
                                                      radioButtons("plot_type_totA", "Plot type", c("Line", "Distribution"),
                                                                   inline = TRUE),
                                                      p("For each forecast, you will need to quantify the different sources of uncertainty in the panel below."),
                                                      br(),
                                                      h3("Quantify Forecast Uncertainty"),
                                                      p("For our forecasts, uncertainty is represented in the spread or the ", tags$em("variation"), " of the forecast ensemble members. From this variation we can calculate the ", tags$em("standard deviation"), " across our ensemble members and use this as a quantification of our uncertainty."),
                                                      actionButton("quant_ucA", "Quantify uncertainty"),
                                                      radioButtons(qid[31], quest[qid[31], ], choices = uc_sources[1:4], selected = character(0))
                                               ),
                                               column(8,
                                                      h4("Water Temperature Forecast with Total Uncertainty"),
                                                      wellPanel(
                                                        plotlyOutput("tot_fc_uncertA")
                                                        # checkboxInput("add_obs1", "Add observations")
                                                      ),
                                                      wellPanel(
                                                        plotlyOutput("fc_quantA")
                                                      )
                                               )
                                             ),
                                             hr(),
                                             #** Model B - UC partitioning ----
                                             fluidRow(
                                               column(4,
                                                      h4("Model 2"),
                                                      textOutput("modB_txt"),
                                                      uiOutput("modB_eqn"),
                                                      actionButton("run_tot_fcB", "Run forecast"),
                                                      radioButtons("plot_type_totB", "Plot type", c("Line", "Distribution"), selected = "Line",
                                                                   inline = TRUE),
                                                      p("For each forecast, you will need to quantify the different sources of uncertainty in the panel below.")
                                               ),
                                               column(8,
                                                      wellPanel(
                                                        plotlyOutput("tot_fc_uncertB")
                                                      )
                                               )
                                             ),
                                             #** Quantify Uncertainty - Part B ----
                                             hr(),
                                             fluidRow(
                                               column(4,
                                                      h3("Quantify Forecast Uncertainty"),
                                                      p("Quantify uncertainty for the other model you have select and compare the two results below and answer questions."),
                                                      actionButton("quant_ucB", "Quantify uncertainty"),
                                                      radioButtons(qid[32], quest[qid[32], ], choices = uc_sources[1:4], selected = character(0))
                                               ),
                                               column(8,
                                                      wellPanel(
                                                        plotlyOutput("fc_quantB")
                                                      )
                                               )

                                             ),
                                             hr(),

                                             fluidRow(
                                               column(6,
                                                      h3("Which source of uncertainty is contributing the most?"),
                                                      p("This is the key question for forecasters. If we can identify which uncertainty is contributing the most then we can take steps to manage this uncertainty and reduce it in our forecasts."),
                                                      # p("Here is a figure from a paper which partitioned out the different contributors of uncertainty to a forecast of water temperature."),
                                                      br(),
                                                      # h4("Q. How do you think you would be able to partition out the uncertainty?")
                                               ),
                                               column(4, offset = 1,
                                                      box(id = "box2", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   textAreaInput2(inputId = qid[33], label = quest[qid[33], ], width = "90%")
                                                            )
                                                          )
                                                      )
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(12,
                                                      h3("Manage Uncertainty")
                                               ),
                                               column(5, offset = 1,
                                                      radioButtons("uc_manage", "Select a source of uncertainty below and learn of ways to reduce it:",
                                                                   choices = uc_sources[1:4], selected = character(0), inline = TRUE),
                                                      conditionalPanel("input.uc_manage == 'Process'",
                                                                       h4("Process Uncertainty"),
                                                                       tags$ul(
                                                                         tags$li(id = "txt_j", "Build a better model"),
                                                                         tags$li(id = "txt_j", "Collect more data")
                                                                       )
                                                      ),
                                                      conditionalPanel("input.uc_manage == 'Parameter'",
                                                                       h4("Parameter Uncertainty"),
                                                                       tags$ul(
                                                                         tags$li(id = "txt_j", "Collect more data"),
                                                                         tags$li(id = "txt_j", "Identify which variables you need to measure")
                                                                       )
                                                      ),
                                                      conditionalPanel("input.uc_manage == 'Driver'",
                                                                       h4("Driver Uncertainty"),
                                                                       tags$ul(
                                                                         tags$li(id = "txt_j", "Use better forecasted data"),
                                                                         tags$li(id = "txt_j", "Increase number of driver ensembles")
                                                                       )
                                                      ),
                                                      conditionalPanel("input.uc_manage == 'Initial Conditions'",
                                                                       h4("Initial Conditions Uncertainty"),
                                                                       tags$ul(
                                                                         tags$li(id = "txt_j", "Collect data at time of forecast (in real-time)"),
                                                                         tags$li(id = "txt_j", "Collect data more frequently")
                                                                         )
                                                                       )
                                                      )
                                               )
                                             ),
                                    #* Objective 11 - Management Scenario ====
                                    tabPanel(title = "Objective 11 - Management Scenario", value = "obj13",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 11 - Management Scenario"),
                                                                p(id = "txt_j", module_text["obj_11", ])
                                                                )
                                                      ),
                                               column(4,
                                                      h3("Management Scenario"),
                                                      p(id = "txt_j", module_text["mgmt_scen1", ]),
                                                      p(id = "txt_j", module_text["mgmt_scen2", ]),
                                                      p(id = "txt_j", module_text["mgmt_scen3", ]),
                                                      p(id = "txt_j", module_text["mgmt_scen4", ]),
                                                      p(id = "txt_j", "Some fish species, such as Chinook salmon ", tags$em("(Oncorhynchus tshawytscha),"), "have eggs which have higher rates of survival at colder temperatures."),
                                                      p(id = "txt_j", module_text["mgmt_scen5", ]),

                                                      p("")
                                                      ),
                                               column(8, align = "center",
                                                      img(src = "salmon_underwater_dam.jpg", height = "60%",
                                                          width = "60%", align = "center"),
                                                      br(),
                                                      a("Image source", href = "http://www.hatchmag.com/sites/default/files/styles/extra-large/public/field/image/pinksalmon-elwha.jpg", target = "_blank")
                                                      # h2("Image of Water Resource manager & SALMON")
                                                     )
                                               ),
                                             hr(),
                                             #** Decision #1 ----
                                             fluidRow(
                                               column(4,
                                                      h3("Decision #1"),
                                                      p("Use the forecast of surface and bottom temperature (across) to make a decision."),
                                                      p("This forecast was generated only including parameter uncertainty."),
                                                      radioButtons("dec_scen1", "Which level should be used to release water from the dam?", choices = dam_lev,
                                                                   selected = character(0)),
                                                      actionButton("scen1_dec", "Decide")
                                                      ),
                                               column(8,
                                                      h4("Forecast of water temperature at the surface and bottom of the reservoir"),
                                                      wellPanel(
                                                        plotOutput("scen1_plot")
                                                        )
                                                      )
                                               ),
                                             hr(),
                                             #** Decision #2 ----
                                             fluidRow(
                                               column(4,
                                                      h3("Decision #2"),
                                                      p("Use the forecast of surface and bottom temperature (across) to make a decision."),
                                                      p("This forecast was generated and includes process, parameter, initial conditions and driver uncertainty."),
                                                      radioButtons("dec_scen2", "Which level should be used to release water from the dam?", choices = dam_lev,
                                                                   selected = character(0)),
                                                      actionButton("scen2_dec", "Decide")
                                               ),
                                               column(8,
                                                      h4("Forecast of water temperature at the surface and bottom of the reservoir"),
                                                      wellPanel(
                                                        plotOutput("scen2_plot")
                                                        )
                                                      )
                                               ),
                                             hr(),
                                             # Think, Pair, Share
                                             fluidRow(
                                               column(12,
                                                      h3("Think, Pair, Share!")
                                               ),
                                               column(4,
                                                      p("With your partner, compare your decisions and discuss how the uncertainty visualization affected your decision."),
                                                      p("Answer the questions across.")
                                                      ),
                                               column(6, offset = 2,

                                                      box(id = "box2", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   textAreaInput2(inputId = qid[34], label = quest[qid[34], ], width = "90%"),
                                                                   textAreaInput2(inputId = qid[35], label = quest[qid[35], ], width = "90%")
                                                                   )
                                                            )
                                                          )
                                                      )
                                               )
                                             ),
                                    # ),
                                    #* Activity C - Summary ====
                                    tabPanel(title = "Summary", value = "obj14",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Summary")
                                                      )
                                               )
                                             ),
                                             fluidRow(
                                               column(4,
                                                      h3("Completed Activity C!"),
                                                      p("This is the end of Activity C. Now you can generate your final report which will input all your answers and figures into a Microsoft Word document which you can download and submit to your instructor.")
                                                      ),
                                               column(4,
                                                      h3("Generate Report"),
                                                      p("This will take the answers you have input into this app and generate a Microsoft Word document (.docx) document with your answers which you can download and make further edits before submitting."),
                                                      actionButton("generate2", "Generate Report (.docx)", icon = icon("file"), width = "190px", class = "btn-primary"
                                                                   # id = "dl_btn", # This is the only button that shows up when the app is loaded
                                                                   # style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                                      ), br(), br(),
                                                      tags$style(type="text/css", "#download2 {background-color:#579277;color: white}"),
                                                      conditionalPanel(condition = "output.reportbuilt2", # This button appears after the report has been generated and is ready for download.
                                                                       downloadButton("download2", "Download Report", width = "60px", style = "width:190px;"
                                                                                      )
                                                                       )

                                                      ),
                                               column(4,
                                                      h3(tags$b("Questions to be completed:")),
                                                      wellPanel(
                                                        htmlOutput("check_list2")
                                                        )
                                                      )
                                               )
                                             )
                                    )
                        )
               ),
    # Tab navigation buttons ----
    br(), hr(),
    introBox(
      # h4("Use the buttons below to navigate through the tabs", align = "center"),
      box(width = 12, status = "success",
          solidHeader = TRUE,
          fluidRow(

            column(5, align = "center",
                   br(),
                   hover_action_button(
                     inputId = "prevBtn1",
                     label = "< Previous",
                     button_animation = "glow",
                     style = paste0("color: ", nav_txt, "; background-color: ", nav_butt, "; border-color: #00664B; padding:15px; font-size:22px;")
                   ),
                   bsTooltip("prevBtn1", title = "Navigate to previous tab", placement = "left", trigger = "hover"),
                   br(), br()

            ),
            column(2, align = "center",
                   br(),
                   tags$style(type="text/css", paste0("#download_answers {background-color:#579277;color: white; padding:15px; font-size:18px;}")),
                   hover_download_button(outputId = "download_answers",
                                         label = "Download user input",
                                         class = "butt1",
                                         button_animation = "glow"),
                   br(), br()
            ),
            column(5, align = "center",
                   br(),
                   use_hover(popback = TRUE),
                   hover_action_button(
                     inputId = "nextBtn1",
                     label = "Next >",
                     button_animation = "glow",
                     style = paste0("color: ", nav_txt, "; background-color: ", nav_butt, "; border-color: #00664B; padding:15px; font-size:22px;")
                   ),
                   bsTooltip("nextBtn1", title = "Navigate to next tab", placement = "right", trigger = "hover"),
                   br(), br()
                   # )
            )
          )
      ), data.step = 3, data.intro = help_text["tab_nav2", 1], data.position = "right"
    ),
    hr(),
    fluidRow(
      column(8, offset = 1,
             br(),
             p(module_text["acknowledgement", ], id = "ackn"),
             p(app_update_txt, id = "ackn")
             )
      )
    )
  }

shinyUI(ui)

# end
