library(shinydashboard)
library(magrittr)

# read data
dataset <- 
    readr::read_csv("dataset.csv")

# compute counts
counts <- 
    dataset %>% 
    dplyr::group_by(from_book, to_book) %>% 
    dplyr::count()

counts %<>% 
    reshape2::dcast(from_book ~ to_book,
                    value.var = "n")

# convert in matrix
mat_counts <-
    as.matrix(counts[ , -1])

# replace by 0
mat_counts[is.na(mat_counts)] <- 0 

# adjust dimnames
dimnames(mat_counts) <- 
    list(from_book = mat_counts %>% colnames(),
         to_book = mat_counts %>% colnames())

# get group names
groupNames <- mat_counts %>% colnames()

ui <- dashboardPage(skin = "black",
                    
                    header = dashboardHeader(title = "Bible Cross References Analysis"),
                    sidebar = dashboardSidebar(
                        tags$div(
                            tags$p("The bible cross reference query is able by selecting or clicking on chord diagram, 
                            but books with too little area is more efficient selecting in dropdown"), 
                            style = "padding: 15px; text-align: justify"
                        ),
                        
                        selectizeInput("group", 
                                       label = "Select Book", 
                                       choices = c("", groupNames), 
                                       selected = "", 
                                       multiple = FALSE,
                                       options = NULL)
                    ),
                    body = dashboardBody(
                        box(title = "Chord Diagrams (or click on book)",
                            chorddiag::chorddiagOutput("chord"),
                            footer = tags$a(href = "https://www.openbible.info/labs/cross-references/",
                                            "Reference: www.openbible.info/labs/cross-references"), 
                            width = 7
                        ),
                        box(title = "Cross References",
                            DT::dataTableOutput("table"), 
                            width = 5),
                        textOutput("shiny_return"),
                        textOutput("shiny_return2")
                    )
)

server <- function(input, output, session) {
    
    output$chord <- chorddiag::renderChorddiag({
        chorddiag::chorddiag(mat_counts, 
                             showTicks = FALSE,
                             palette = "Set2", 
                             palette2 = "#Set3", 
                             margin = 40,
                             groupnameFontsize = 9,
                             groupnamePadding = 10,
                             clickGroupAction = "Shiny.onInputChange('groupIndex', d.index+1);"
        )
    })
    
    output$table <- DT::renderDataTable({
        
        groupFilter <- c(input$group, groupNames[input$groupIndex])
        
        validate(
            need(groupFilter != "", "Please choose some book")
        )
        
        dataset %>% 
            dplyr::filter(from_book == groupFilter) %>% 
            dplyr::select(from_verse, to_verse) %>% 
            DT::datatable(rownames = FALSE,
                      options = list(dom = "ftp"),
                      colnames = c("Verse", "Reference")
            )
    })
    
}

shinyApp(ui, server)