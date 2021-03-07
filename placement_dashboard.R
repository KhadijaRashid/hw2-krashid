library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
#library(plyr)
library(dplyr)
# install.packages("glyphicon")
# library(glyphicon)

# Load and clean data ----------------------------------------------
child.data <- read.csv("placementData.csv")

#checking missing values for ethnic groups/gender and age
Et <- sum(is.na(child.data$ETHNIC2))
gen <- sum(is.na(child.data$GENDER))
age <- sum(is.na(child.data$AGE_20161231))

#dropping age >20
child.data <- subset(child.data, AGE_20161231 <= 20)

#recoding our variables of interest
child.data <- child.data %>%
    rename("Gender" = GENDER,
           "All_ethnicities" = ETHNIC,
           "Race" = ETHNIC2, #white, black, other
           "Age" = AGE_20161231,
           "Start_age" = SPELLAGE, #age of child at the begiing of spell
           #         "Spell_age" = 
           "Placement1_type" = PLACE1,
           "Subsequent_spells" = REENTER,
           "Exit_type" = EXIT,
           "Runs" = RUNS,
           "Year" = INYEAR,
           "Duration_Spells" = DURAT
    )

#converting age to numeric 
child.data$Start_age <- as.numeric(child.data$Start_age)

#class(child.data$Runs)
child.data$Runs <- as.numeric(child.data$Runs)
#count(child.data$Runs)
child.data$Duration_Spells <- as.numeric(child.data$Duration_Spells)

#recoding the exit types into 5 categories
child.data <- mutate(child.data, 
                     Exit_type_recat = recode(Exit_type,
                                              "XOP" = "Permanent",
                                              "XCA" = "Permanent",
                                              "XRF" = "Permanent",
                                              "XLC" = "Permanent", 
                                              "XRM" = "Reach Adulthood", #not adopted cat
                                              "XRY" = "Run Away",        #not adopted cat
                                              "ZTC" = "Still in care",
                                              "XJP" = "Non-permanent",
                                              "XRL" = "Non-permanent",
                                              "XUK" = "Misc",
                                              "XOT" = "Misc"))

#recoding  all ethniticites 
child.data <- mutate(child.data, 
                     All_ethnicities = recode(All_ethnicities,
                                              "BL" = "African American",
                                              "WH" = "White",
                                              "MU" = "Multiple Race",
                                              "AS" = "Asian and Pacific",
                                              "AN" = "Native American",
                                              "OT" = "Other Category",
                                              "UK" = "Unknown"
                     ))
#recoding race
child.data <- mutate(child.data, 
                     Race = recode(Race,
                                   "BL" = "Black",
                                   "WH" = "White",
                                   "OT" = "Other"
                     ))

#recoding Gender
child.data <- mutate(child.data, 
                     Gender = recode(Gender,
                                     "F" = "Female",
                                     "M" = "Male"
                     ))
#recoding subsequent spells
child.data <- mutate(child.data, 
                     Subsequent_spells = recode(Subsequent_spells,
                                                "0" = "No",
                                                "1" = "Yes"
                     ))

#------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------------
# Application header & title ----------------------------------------------
header <- dashboardHeader(title = "Placement of Children in Foster Care",
                          titleWidth = 350)


# Dashboard Sidebar ----------------------------------------------
sidebar <- dashboardSidebar(width = 350,
  sidebarMenu(
    id = "tabs",

    # Menu Items ----------------------------------------------
    menuItem("Plot", icon = icon("bar-chart"), tabName = "plot"),
    menuItem("Table", icon = icon("table"), tabName = "table"),
    
    # Inputs: select variables to plot ----------------------------------------------
    #gender
    checkboxGroupInput("genderSelect",
                       "Gender:",
                       choices = c("Female",
                                   "Male"),
                       selected = c("Female")
                       ),
    # ethnicty
    #check if you want it all to be buttons too
    selectInput("ethnicitySelect",
                label = "Ethnicity:",
                choices = sort(unique(child.data$All_ethnicities)),
                multiple = TRUE,
                selectize = TRUE,
                selected = c("African American")),
                       
    # Year Selection ----------------------------------------------
    sliderInput("YearIn",
                "Year entered into foster care:",
                min = min(child.data$Year, na.rm = T),
                max = max(child.data$Year, na.rm = T),
                value = c(min(child.data$Year, na.rm = T), max(child.data$Year, na.rm = T)),
                step = 1)
    )
)

# Dashboard body ----------------------------------------------
body <- dashboardBody(tabItems(

    # Plot page ----------------------------------------------
  tabItem("plot",
      # Input and Value Boxes ----------------------------------------------
          fluidRow(
             valueBoxOutput("Runs", width = 3), #runs types
             infoBoxOutput("Start_age", width = 4), #average age
             valueBoxOutput("Duration_Spells", width = 4) #duration of spells
          ),
      fluidRow(
          tabBox(title = "Plot",
                 width = 12,
                 tabPanel("Age", plotlyOutput("plot_age")),
                 tabPanel("Duration of stay", plotlyOutput("plot_duration")),
                 tabPanel("Placement type", plotlyOutput("plot_exit")))
      )
),
  
  # Data Table Page ----------------------------------------------
  tabItem("table",
          fluidPage(
              box(title = "Selected Placement Chracteristics",
                  DT::dataTableOutput(outputId ="table"), width = 12))
  )
)
)


ui <- dashboardPage(skin = "purple", header, sidebar, body)

  # Define server function required to create plots and value boxes -----
server <- function(input, output) {
    # Reactive data function -------------------------------------------
    swInput <- reactive({
        child <- child.data %>%
            
        # Slider Filter for year----------------------------------------
        filter(Year >= input$YearIn[1] & Year <= input$YearIn[2])
        
        # Ethnicity Filter ----------------------------------------------
        if (length(input$ethnicitySelect) > 0 ) {
            child <- subset(child, All_ethnicities %in% input$ethnicitySelect)
        }
        
        # Gender Filter ----------------------------------------------
        if (length(input$genderSelect) > 0 ) {
            child <- subset(child, Gender %in% input$genderSelect)
        }
    # Return dataframe ----------------------------------------------
        return(child)
    })
    
    # Reactive melted data ----------------------------------------------
    # mwInput <- reactive({
    #     swInput() %>%
    #         melt(id = "ID")
    # })
    
    # output$plot_age <- renderPlotly({
    #     dat <- subset(mwInput(), variable == "Start_age")
    #     # Generate Plot ----------------------------------------------
    #     ggplotly(ggplot(data = dat, aes_string(x = mw$Input , y = as.numeric(mw$y), fill = child.data$Gender))) + geom_bar(stat = "identity")
    # })
    # #A plot showing the duration of spells -----------------------------------
    # output$plot_duration <- renderPlotly({
    #     dat <- subset(mwInput(),  variable == "Duration_Spells")
    #     # Generate Plot ----------------------------------------------
    #     ggplot(data = dat, aes_string(x = ID, y = value, fill = ID)) + geom_bar(stat = "identity")
    # })
    
    
    # Create a subset of data filtering for selected age types ------
    # Selected type is the checkbox
    # child.age <- child.data %>%
    #     group_by(child.data$Age, child.data$All_ethnicities, child.data$Year, child.data$Year) %>%
    #     summarise(count = n(child.data$Age)) 
    
#     #subsetting and agreegating data for plots
#     child_subset_g <- reactive({
#         req(input$genderSelect) # ensure availablity of value before proceeding
#         filter(child.data, Gender %in% input$genderSelect)
# #        summarise(count = n())
#     })
    
    # child_subset_ge <- reactive({
    #     req(input$ethnicitySelect) # ensure availablity of value before proceeding
    #     filter(child_subset_g(), All_ethnicites %in% input$ethnicitySelect)
    #     #        summarise(count = n())
    # })
    # 
    # child_subset_gey <- reactive({
    #     req(input$YearIn) # ensure availablity of value before proceeding
    #     filter(child_subset_gey(), Year %in% input$YearIn)
    #     #        summarise(count = n())
    # })
    
    #    subsetting and agreegating data for plots
    # child_subset_g <- reactive({
    #   req(input$genderSelect)
    #   child.agg <- child.data %>%
    #     group_by(get(input$genderSelect)) %>%
    #     summarise(count = n())
    #   return(child.agg)
    # })
    
    # Create scatterplot  --
    output$plot_age <- renderPlotly({
      sw <- swInput()
      ggplot(data = child.data, aes_string(x = sw$Age, y = sw$Year)) +
        # color = input$z)) +
        geom_point() +#(aes(x = input$Year, y = input$Age, size = 10)) +
        geom_smooth(se = FALSE, show.legend = FALSE) +
        theme(plot.title = element_text(face = "plain", size = 18)) #+
      labs(x = "Age",
           y = "Number of children")
      #      color = toTitleCase(str_replace_all(input$z, "_", " "))
    })
    
    # # Create barplot object  --
    # output$barplot <- renderPlot({
    #     ggplot(data = sales, aes_string(x = input$x, y = "Global_Sales")) +
    #         stat_summary(fun = sum, geom = "bar") +
    #         geom_bar(fill = "pink", alpha = 1, stat = "identity") +
    #         theme(plot.title = element_text(face = "plain", size = 18)) +
    #         labs(x = toTitleCase(str_replace_all(input$x, "_", " ")),
    #              y = "Total Worldwide Sales (in millions)",
    #              title = "Global Sales across Years/Platforms"
    #         )
    # })
    

    
    # Data table of characters ----------------------------------------------
    output$table <- DT::renderDataTable({
        subset(swInput(), select = c(Gender, All_ethnicities, Age, Subsequent_spells, Exit_type, Runs, Duration_Spells))
    })
    
  # Runs value box ----------------------------------------------
    output$Runs <- renderValueBox({
        sw <- swInput()
        num <- sum(sw$Runs)

    valueBox(subtitle = "Total No. of runaways", value = num, icon = icon("running"), color = "red")
    })
    
    # Age mean info box ----------------------------------------------
    output$Start_age <- renderInfoBox({
        sw <- swInput()
        num1 <- round(mean(sw$Age, na.rm = T), 2)

        infoBox("Avg Age at admission", value = num1, subtitle = "years", icon = icon("code"), color = "teal")
    })
    # duration of spells mean info box ----------------------------------------------
    output$Duration_Spells <- renderInfoBox({
        sw <- swInput()
        num <- round(mean(sw$Duration_Spells, na.rm = T), 2)
        
        infoBox("Avg Duration of stay", value = num, subtitle = "days", icon = icon("calendar"), color = "olive")
        #HTML(paste("This is for", paste(nrow(sw), "observations from this dataset. <br>")))
    })
}
    


# Run the application 
shinyApp(ui = ui, server = server)
