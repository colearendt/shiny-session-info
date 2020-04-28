library(shiny)
library(listviewer)

sessionInfoModuleUI <- function(id, req) {
    ns <- NS(id)

    fluidPage(
    titlePanel("Shiny Session Info"),
    
    sidebarLayout(
        sidebarPanel(
        ),
        
        mainPanel(
            h2("session$clientData"),
            jsoneditOutput(ns("clientdataText")),
            h2("session"),
            jsoneditOutput(ns("sessionInfo")),
            h2("UI req object"),
            jsonedit(as.list(req)
                     , mode = 'view'
                     , modes = list('view'))
        )
    )
    )
}

sessionInfoModule <- function(input, output, session, actual_session) {
    ns <- session$ns
    output$test <- renderText("hi")
    
    clean_environ <- function(environ){
        if (is.environment(environ)) {
            lenv <- as.list(environ)
            lenv <- lenv[which(!lapply(lenv, typeof) %in% c("environment"))]
            return(lenv)
        } else {
            return(environ)
        }
    }
    
    # Store in a convenience variable
    cdata <- session$clientData
    
    
    output$sessionInfo <- renderJsonedit({
        #browser()
        #calt <- reactiveValuesToList(session)
        calt <- as.list(actual_session)
        
        
        calt_type <- lapply(calt, typeof)
        calt_clean <- calt[which(!calt_type %in% c("closure"))]
        calt_clean <- lapply(calt_clean, clean_environ)
        calt_class <- lapply(calt_clean, class)
        calt_clean_2 <- calt_clean[which(!calt_class %in% c("reactivevalues", "shinyoutput"))]
        calt_final <- calt_clean_2
        calt_names <- names(calt_final)
        
        print(lapply(calt_final, typeof))
        
        jsonedit(calt_final
                 , mode = 'view'
                 , modes = list('view'))
    })
    
    # Values from cdata returned as text
    output$clientdataText <- renderJsonedit({
        jsonedit(as.list(cdata), mode = 'view', modes = list('view'))
    })
}


ui <- function(req) {
    fluidPage(
  sessionInfoModuleUI("testing", req)
)
}

server <- function(input, output, session) {
  callModule(sessionInfoModule, "testing", session)
}

shinyApp(ui = ui, server = server)
