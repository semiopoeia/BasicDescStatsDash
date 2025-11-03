#basic settings
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

#loading census into memory
##set file path where you've saved needed census data##
###########################################################################################
###adjust the file path below for where you've located census###
census<-read.csv(
"E:/<<filepathtodata>>.csv",
stringsAsFactors=T, header=T)
############################################################################################
save(census,file="census.Rdata")

#######################################################
########NO NEED TO ALTER BELOW#######
###############################################

#call in data
load("census.RData")

#set up UI
ui <- fluidPage(
  titlePanel("Descriptive Statistics for Salary by Rank/Campus/Class/School/etc."),
  sidebarLayout(
    sidebarPanel(
      selectInput("outcomeVar", "Select Faculty Contract:",
                  choices = c("Annual.Salary", "Faculty.Contract.Salary")),
      selectInput("groupVars", "Select Grouping Variables:",
                  choices = names(census), multiple = TRUE),
      uiOutput("filterUIs"),             #Grouping variables
      selectInput("nonGroupVars", "Select Subset Filter Variables:",
                  choices = names(census), multiple = TRUE),
      uiOutput("nonGroupFilterUIs"),     #Subset filters
      uiOutput("rangeFilterUI"),         #Continuous variable range filter
      numericInput("value", "Enter Value for Percentile Rank:", value = 0),
      selectInput("plotType", "Choose Plot Type:",
                  choices = c("Boxplot", "Density", "Histogram", "Violin"),
                  selected = "Boxplot"),
      actionButton("update", "Update")
    ),
    mainPanel(
      h4("Descriptive Statistics"),
      tableOutput("descStats"),
      h4("Percentile Rank of Requested Salary"),
      tableOutput("percentileRank"),
      h4("Salary Distribution Plot"),
      plotlyOutput("salaryPlot"),
      h4("Download Summary"),
      downloadButton("downloadStats", "Download Descriptive Statistics")
    )
  )
)


#establish server logic
server <- function(input, output, session) {

  #UI for grouping variables
  output$filterUIs <- renderUI({
    req(input$groupVars)
    lapply(input$groupVars, function(var) {
      selectizeInput(
        paste0("filter_", var),
        paste("Select", var, "Group:"),
        choices = unique(census[[var]]),
        multiple = TRUE
      )
    })
  })

  #UI for subset filters
  output$nonGroupFilterUIs <- renderUI({
    req(input$nonGroupVars)
    lapply(input$nonGroupVars, function(var) {
      selectizeInput(
        paste0("nonGroupFilter_", var),
        paste("Filter", var, ":"),
        choices = unique(census[[var]]),
        multiple = TRUE
      )
    })
  })

  #UI for continuous variable range filter
  output$rangeFilterUI <- renderUI({
    numericVars <- names(census)[sapply(census, is.numeric)]
    rangeVars <- setdiff(numericVars, input$groupVars)
    if (length(rangeVars) == 0) return(NULL)
#complete filtering/grouping listing
    tagList(
      selectInput("rangeVar", "Select Continuous Variable for Range Filter:",
                  choices = rangeVars),
      uiOutput("rangeSliderUI")
    )
  })

  output$rangeSliderUI <- renderUI({
    req(input$rangeVar)
    rng <- range(census[[input$rangeVar]], na.rm = TRUE)
    sliderInput("rangeValues", paste("Select Range for", input$rangeVar),
                min = rng[1], max = rng[2],
                value = rng, step = 1)
  })

  #reactively filtering data
  filteredData_reactive <- eventReactive(input$update, {
    req(input$outcomeVar)
    data <- census

    #filter on grouping variables
    for (var in input$groupVars) {
      vals <- input[[paste0("filter_", var)]]
      if (!is.null(vals) && length(vals) > 0) {
        data <- data %>% filter(!!sym(var) %in% vals)
      }
    }

    #filter on selection/subsetting variables
    for (var in input$nonGroupVars) {
      vals <- input[[paste0("nonGroupFilter_", var)]]
      if (!is.null(vals) && length(vals) > 0) {
        data <- data %>% filter(!!sym(var) %in% vals)
      }
    }

    #filter on continuous range variable
    if (!is.null(input$rangeVar) && !is.null(input$rangeValues)) {
      data <- data %>% filter(
        .data[[input$rangeVar]] >= input$rangeValues[1],
        .data[[input$rangeVar]] <= input$rangeValues[2]
      )
    }
    data
  })

  #produce descriptive statistics
  output$descStats <- renderTable({
    req(filteredData_reactive())
    filteredData <- filteredData_reactive()
    outcomeVar <- input$outcomeVar
    groupVars <- input$groupVars

    descStats <- filteredData %>%
      group_by(across(all_of(groupVars))) %>%
      summarize(
        N = sum(!is.na(.data[[outcomeVar]])),
        Mean = mean(.data[[outcomeVar]], na.rm = TRUE),
        Min = min(.data[[outcomeVar]], na.rm = TRUE),
        `25th Percentile` = quantile(.data[[outcomeVar]], 0.25, na.rm = TRUE),
        Median = median(.data[[outcomeVar]], na.rm = TRUE),
        `75th Percentile` = quantile(.data[[outcomeVar]], 0.75, na.rm = TRUE),
        Max = max(.data[[outcomeVar]], na.rm = TRUE),
        .groups = "drop"
      )
    descStats
  })

  #produce percentile for given outcome value if want
output$percentileRank <- renderTable({
  req(filteredData_reactive())
  filteredData <- filteredData_reactive()
  outcomeVar <- input$outcomeVar
  groupVars <- input$groupVars
  value <- input$value

  percentileRank <- filteredData %>%
    group_by(across(all_of(groupVars))) %>%
    summarize(
      PercentileRank = mean(.data[[outcomeVar]] <= value, na.rm = TRUE) * 100,
      .groups = "drop"
    )
  percentileRank
})

  #produce distribution plots
  output$salaryPlot <- renderPlotly({
    req(filteredData_reactive())
    filteredData <- filteredData_reactive()
    outcomeVar <- input$outcomeVar
    groupVars <- input$groupVars
    plotType <- input$plotType
    plotData <- filteredData %>% filter(!is.na(.data[[outcomeVar]]))
    if (length(groupVars) >= 1) {
      plotData$group_label <- interaction(plotData[, groupVars], drop = TRUE, sep = " / ")
    } else {
      plotData$group_label <- "All Data"
    }
    if (plotType %in% c("Boxplot", "Violin")) {
      p <- ggplot(plotData, aes(x = group_label, y = .data[[outcomeVar]]))
      if (plotType == "Boxplot") {
        p <- p + geom_boxplot()
      } else {
        p <- p + geom_violin(fill = "purple", alpha = 0.5)
      }
      p <- p + labs(x = "Group", y = outcomeVar)
    } else if (plotType == "Density") {
      p <- ggplot(plotData, aes(x = .data[[outcomeVar]],y=after_stat(density))) +
        geom_density(fill = "steelblue", alpha = 0.5) +
        labs(x = outcomeVar, y = "Density")
    } else if (plotType == "Histogram") {
      p <- ggplot(plotData, aes(x = .data[[outcomeVar]],y=after_stat(count))) +
        geom_histogram(fill = "darkorange", bins = 30, alpha = 0.7) +
        labs(x = outcomeVar, y = "Count")
    }
    if (length(groupVars) == 1) {
      p <- p + facet_wrap(as.formula(paste("~", groupVars)))
    } else if (length(groupVars) == 2) {
      p <- p + facet_grid(as.formula(paste(groupVars[1], "~", groupVars[2])))
    } else if (length(groupVars) > 2) {
      p <- p + facet_wrap(~group_label)
    }
    p <- p + theme_minimal()
    ggplotly(p)
  })

  #optional download stats
  output$downloadStats <- downloadHandler(
    filename = function() {
      paste0("Descriptive_Statistics_", Sys.Date(), ".csv")
    },
    content = function(file) {
      filteredData <- filteredData_reactive()
      outcomeVar <- input$outcomeVar
      groupVars <- input$groupVars
      descStats <- filteredData %>%
        group_by(across(all_of(groupVars))) %>%
        summarize(
          N = sum(!is.na(.data[[outcomeVar]])),
          Mean = mean(.data[[outcomeVar]], na.rm = TRUE),
          Min = min(.data[[outcomeVar]], na.rm = TRUE),
          `25th Percentile` = quantile(.data[[outcomeVar]], 0.25, na.rm = TRUE),
          Median = median(.data[[outcomeVar]], na.rm = TRUE),
          `75th Percentile` = quantile(.data[[outcomeVar]], 0.75, na.rm = TRUE),
          Max = max(.data[[outcomeVar]], na.rm = TRUE),
          .groups = "drop"
        )
      write.csv(descStats, file, row.names = FALSE)
    }
  )
}

#run app
shinyApp(ui = ui, server = server)
