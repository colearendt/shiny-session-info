library(shiny)

ui <- fluidPage(
    
    titlePanel("Shiny Session Info"),
    
    sidebarLayout(
        sidebarPanel(
        ),
        
        mainPanel(
            h2("session$clientData"),
            verbatimTextOutput("clientdataText"),
            h2("session"),
            verbatimTextOutput("sessionInfo") 
        )
    )
)

server <- function(input, output, session) {
    
    
    # Store in a convenience variable
    cdata <- session$clientData
    
    
    output$sessionInfo <- renderText({
        calt <- as.list(session)
        
    #browser()
        calt_type <- lapply(calt, typeof)
        calt_clean <- calt[which(!calt_type %in% c("environment", "closure"))]
        calt_class <- lapply(calt_clean, class)
        calt_clean_2 <- calt_clean[which(!calt_class %in% c("reactivevalues", "shinyoutput"))]
        calt_final <- calt_clean_2
        calt_names <- names(calt_final)
        
        all_calt <- lapply(calt_names, function(name){
            paste(name, as.character(calt_final[[name]]), sep = " = ")
        })
        paste(all_calt, collapse = "\n")
    })
    
    # Values from cdata returned as text
    output$clientdataText <- renderText({
        cnames <- names(cdata)
        
        allvalues <- lapply(cnames, function(name) {
            paste(name, cdata[[name]], sep = " = ")
        })
        paste(allvalues, collapse = "\n")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

