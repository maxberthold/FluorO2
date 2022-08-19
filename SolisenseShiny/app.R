library(shiny)
library(tidyverse)

data <- readRDS("../ProcessData/salinity_growthSolFit.Rds")

# Take input from the user, and show the user output
ui <- fluidPage(
    
    # Application title
    titlePanel("Plate Parameter Plots"),
    
    # Layout with a side menu, and a main panel 
    sidebarLayout(
        # What to display in the side menu
        sidebarPanel(
            sliderInput(
                "height",
                label = "Plot Height",
                min = 0, max = 800, value = 500,
                step = 10
            ),
            sliderInput(
                "height_adjust", 
                label = "Y-axis adjustment:",
                min = 0, max = 75, value = 1,
                step = 0.01
            ),
            selectInput(
                "xaxis",
                label = "X axis",
                choices = colnames(data),
                selected = "E_hours"
            ),
            selectInput(
                "yaxis",
                label = "Y axis",
                choices = colnames(data),
                selected = "FvFm"
            ),
            sliderInput(
                "size",
                label = "Marker Size",
                min = 0, max = 5, value = 3.5,
                step = 0.5
            ),
            selectInput(
                "color",
                label = "Choose color",
                choices = as.factor(colnames(data)),
                selected = "strain"
            ),
            selectInput(
                "shape",
                label = "Choose shape",
                choices = as.factor(colnames(data)),
                selected = as.character("strain")
            ),
            selectInput(
                "facetX",
                label = "Choose facet row",
                choices = as.factor(colnames(data)),
                selected = "ExpSalinity"
            ),
            selectInput(
                "facetY",
                label = "Choose facet column",
                choices = as.factor(colnames(data)),
                selected = "par_ue"
            ),
            checkboxGroupInput(
                "Light_steps",
                label = "Choose Light step",
                choices = unique(data$Light_steps),
                selected = "320"
            ),
            # checkboxGroupInput(
            #     "term",
            #     label = "Choose parameter",
            #     choices = unique(data$term),
            #     selected = "mu"
            # ),
            checkboxGroupInput(
                "strain",
                label = "Choose strain",
                choices = unique(data$strain),
                selected = "CZS48M"
            ),
        ),
        
        # What to display in the main panel
        mainPanel(
            plotOutput("plot")
        )
    )
)

# Use the input to compute an outputsomething
server <- function(input, output) {
    
    finalData <- reactive({
        data %>%
            # filter(term %in% input$term)
            filter(Light_steps %in% input$Light_steps & strain %in% input$strain)#  & term %in% input$term)
            # filter(Strain %in% input$Strain)
            # filter(NutrientStatus %in% input$NutrientStatus) 
        
    })
    
    output$plot <- renderPlot({
        
        aesInput <- list(x = input$xaxis,
                         y = input$yaxis,
                         color = input$color,
        shape = input$shape)
        
        aesInputFiltered <- lapply(
            aesInput[aesInput != "None"],
            sym)
        
        facetInput <- c(input$facetX,
                        input$facetY)
        
        facetInputFiltered <- facetInput[facetInput != "None"]
        
        ggplot(finalData()) +
            geom_point(do.call(aes, aesInputFiltered), 
                       size = input$size) +
            theme_bw() +
            coord_cartesian(ylim = c(0, input$height_adjust)) +
            #scale_y_continuous(limits = input$height_adjust) +
            if(length(facetInputFiltered) == 1){
                facet_grid(reformulate(facetInputFiltered))
            } else if(length(facetInputFiltered) == 2){
                facet_grid(reformulate(input$facetX, input$facetY))
            } 
        
    },
    height = function(){input$height},
    width = "auto")
}

# Run the application 
shinyApp(ui = ui, server = server)