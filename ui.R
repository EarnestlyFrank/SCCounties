library(leaflet)

# Choices for drop-downs

names <- c("Number of self-employed adults",
           "Percent of working adults who are self-employed",
           "Number of children without health insurance",
           "Percent of children without health insurance",
           "Median family income",
           "Percent of adults with a high school education or higher",
           "Percent of adults with a bachelor degree or higher",
           "Number of households living in the same house as last year",
           "Percent of households living in the same house as last year",
           "Median value of owner occupied hosuing units",
           "Number of vacant hosuing units",
           "Percent of housing units vacant",
           "Number of hosuing units without a full kitchen",
           "Percent of housing units without a full kitchen",
           "Percent turnout in 2016 general election",
           "Median age",
           "Male life expectancy",
           "Female life expectancy")

vars <- names

navbarPage("Frank Webb STAT 515-H01 Final", id="nav",
           
           tabPanel("Interactive map",
                    div(class="outer",
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css")
                        ),
                        
                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leafletOutput("map", width="100%", height="100%"),
                        
                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = FALSE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 375, #height = "auto",
                                      
                                      h2("Select Variables"),
                                      selectInput("indep", "Independent Variable", vars, selected = names[5]),
                                      selectInput("dep", "Dependent Variables", vars, selected = names[18]),
                                      numericInput("numIn", "Model Input", value = 50000, min = 1),
                                      plotOutput("histIndep", height = 300),
                                      plotOutput("histDep", height = 300),
                                      plotOutput("scatter", height = 250),
                                      plotOutput("qqPlot", height = 200)
                                      
                                     # ,htmlOutput("analysis")
                        ),
                        
                        absolutePanel(id = "analysisText", class = "panel panel-default", fixed = TRUE,
                                      draggable = FALSE, top = "auto", left = 20, right = "auto", bottom = 60,
                                      width = 400, #height = 300,
                                      
                                      h2("Statistical Analysis"),
                                      htmlOutput("analysis")
                                      ),
                        
                        
                        
                        tags$div(id="cite",
                                 'Design iterated from RStudio\'s and Joe Chen\'s SuperZip example'
                        )
                    )
           ),

           
           conditionalPanel("false", icon("crosshair"))
)