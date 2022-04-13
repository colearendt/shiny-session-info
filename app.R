library(shiny)
library(listviewer)

safe_list <- function(.list) {
  tryCatch({
    obj <- as.list(.list)
    obj <- lapply(obj, function(x){
      if (is.character(x) && nchar(x) > 300) {
        return(
          paste0(
            substr(x, 1, pmin(nchar(x), 300)),
            "... [[ truncated for space ]]"
            )
        )
      } else {
        return(x)
      }
    })
  }, error = function(e) {
    message(e)
    obj <- list(
      "ERROR",
      e,
      "Please refresh the page to see if the error persists",
      "If so, submit an issue here:",
      "https://github.com/colearendt/shiny-session-info"
    )
  })
  
  return(obj)
}

ui <- function(req) {fluidPage(
    
    titlePanel("System and Shiny info"),
    
    sidebarLayout(
        sidebarPanel(
          h3("An Example App for Exploring Shiny"),
          p("If you encounter any issues with this application, please submit bugs to ", a("GitHub", href = "https://github.com/colearendt/shiny-session-info")),
          p("Use the listviewers to the right for exploring Shiny session state"),
          br(),
          h4("Important Notes"),
          p("This app has shown fragility with a large number of groups. If you see errors and have a large number of groups, please refresh")
        ),
        
        mainPanel(
            h2("Sys.info()"),
            tableOutput("sys_info"),
            h2("Sys.getenv(names = TRUE)"),
            tableOutput("system_env"),
            h2("Shiny: session$clientData"),
            jsoneditOutput("clientdataText"),
            h2("Shiny: session"),
            jsoneditOutput("sessionInfo"),
            h2("Shiny: UI req object"),
            jsonedit(
              safe_list(req)
              , mode = 'view'
              , modes = list('view')
              )
        )
    )
)}

server <- function(input, output, session) {
    
    output$sys_info <- renderTable({
      dat <- as.data.frame(as.list(Sys.info()))
      dat <- as.data.frame(cbind(Name = names(dat), t(dat)))
      dat$Value <- dat$V2
      dat$V2 <- NULL
      dat
    })
    
    output$system_env <- renderTable({ 
      s <- Sys.getenv(names = TRUE)
      data.frame(name = names(s), value = as.character(s))
    })
    
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
        tryCatch({
          calt <- as.list(session)
          
          
          calt_type <- lapply(calt, typeof)
          calt_clean <- calt[which(!calt_type %in% c("closure"))]
          calt_clean <- lapply(calt_clean, clean_environ)
          calt_class <- lapply(calt_clean, class)
          calt_clean_2 <- calt_clean[which(!calt_class %in% c("reactivevalues", "shinyoutput"))]
          calt_final <- calt_clean_2
          calt_names <- names(calt_final)
          
          # print(lapply(calt_final, typeof))
        },
    error = function(e) {
      message(e)
      calt_final <- list("ERROR occurred", e, "Please refresh the page")
    })
        
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
