library(shiny)
library(listviewer)


ui <- function(req) {fluidPage(
    
    titlePanel("Shiny Session Info"),
    
    sidebarLayout(
        sidebarPanel(
        ),
        
        mainPanel(
            h2("session$clientData"),
            jsoneditOutput("clientdataText"),
            h2("session"),
            jsoneditOutput("sessionInfo"),
            h2("UI req object"),
            jsonedit(as.list(req)
                     , mode = 'view'
                     , modes = list('view'))
        )
    )
)}

server <- function(input, output, session) {
    
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
        calt <- as.list(session)
        
        
    #browser()
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

# Run the application 
shinyApp(ui = ui, server = server)

