# UI ####
ui <- fluidPage(# Application title
    titlePanel("Bibliometrics"),
    
    sidebarLayout(
        sidebarPanel(
            helpText(
                "Enter the Google Scholar ID of an author. Search the author on Google Scholar and copy the user argument in the author's page URL:"
            ),
            textInput("AuthorID", "Google Scholar ID", "4iLBmbUAAAAJ"),
            helpText(
                "Tune the author's network plot with the minimum number of co-authored paper to consider and the maximum number of coauthors to display:"
            ),
            sliderInput(
                "MinCopublications",
                "Min number of co-authored papers",
                min = 2,
                max = 20,
                value = 3
            ),
            sliderInput(
                "MaxCoauteurs",
                "Max number of co-authors to show",
                min = 5,
                max = 50,
                value = 10
            ),
            # End of input
            br(),
            # Display author's name and h
            uiOutput("name"),
            uiOutput("h")
        ),
        
        # Show plots in the main panel
        mainPanel(
            plotOutput("network"),
            plotOutput("citations")
        )
    ))

# Server logic ####
server <- function(input, output) {
    
    # Run the get_profile function only once and manage exceptions ####
    # Store the author profile
    AuthorProfile <- reactiveVal()
    # Update it when input$AuthorID is changed
    observeEvent(input$AuthorID, {
        AuthorProfile(tryCatch(
            get_profile(input$AuthorID),
            error = function(e)
                list(name = "Author not found", h_index = 0)
        ))
    })
    
    # Output ####
    output$name <- renderUI({
        tags$h2(AuthorProfile()$name)
    })
    
    output$h <- renderUI({
        if (AuthorProfile()$name != "Author not found") {
            a(
                href = paste0(
                    "https://scholar.google.com/citations?user=",
                    input$AuthorID
                ),
                paste("h index:", AuthorProfile()$h_index),
                target = "_blank"
            )
        }
    })
    
    output$citations <- renderPlot({
        if (AuthorProfile()$name == "Author not found") {
            ggplot() + geom_blank()
        }
        else {
            get_citation_history(input$AuthorID)  %>%
                ggplot(aes(year, cites)) +
                geom_segment(aes(xend = year, yend = 0),
                             size = 1,
                             color =
                                 'darkgrey') +
                geom_point(size = 3, color = 'firebrick') +
                labs(title = "Citations per year",
                     caption = "Source: Google Scholar")
        }
    })
    
    output$network <- renderPlot({
        if (AuthorProfile()$name == "Author not found") {
            ggplot() + geom_blank()
        }
        else {
            # Vecteur des coauteurs de publications, sans accents
            get_publications(input$AuthorID) %>%
                mutate(AuthorsASCII = iconv(author, from = "UTF-8", to = "ASCII//TRANSLIT")) %$%
                AuthorsASCII ->
                AuthorsASCII
            # Auteurs uniques
            AuthorsASCII %>%
                paste(collapse = ", ") %>%
                str_split(pattern = ", ") %>%
                unlist %>%
                unique ->
                UniqueAuthors
            # Elimination de ... (= et al.)
            UniqueAuthors <- UniqueAuthors[UniqueAuthors != "..."]
            # Matrice d'autorat: une ligne par article, auteurs en colonnes, valeurs logiques
            PaperAuthoredBy <-
                sapply(UniqueAuthors, function(Author)
                    str_detect(AuthorsASCII, Author))
            # Filtrage des auteurs
            tibble(Author = UniqueAuthors,
                   NbPapers = colSums(PaperAuthoredBy)) %>%
                filter(NbPapers >= input$MinCopublications) %>%
                arrange(desc(NbPapers)) %>%
                slice(1:input$MaxCoauteurs) ->
                NbPapersPerAuthor
            # Recalcul de la matrice d'autorat réduite
            PaperAuthoredBy <- sapply(NbPapersPerAuthor$Author,
                                      function(Author)
                                          str_detect(AuthorsASCII, Author))
            # Matrice d'adjacence
            adjacencyMatrix <- t(PaperAuthoredBy) %*% PaperAuthoredBy
            # Graphe d'adjacence
            # (https://paulvanderlaken.com/2017/10/31/network-visualization-with-igraph-and-ggraph/)
            g <- graph.adjacency(adjacencyMatrix,
                                 mode = "undirected",
                                 diag = FALSE)
            V(g)$Degree <- degree(g, mode = 'in') # Nombre de liens
            V(g)$Name <- NbPapersPerAuthor$Author # Etiquettes des noeuds
            # Figure
            ggraph(g, layout = "auto") +
                geom_edge_diagonal(alpha = 1, label_colour = "blue") +
                geom_node_label(aes(
                    label = Name,
                    size = log(Degree),
                    fill = Degree
                )) +
                scale_fill_gradient(high = "blue", low = "lightblue") +
                theme(
                    panel.border = element_blank(),
                    panel.grid = element_blank(),
                    legend.position = "none",
                    axis.text = element_blank(),
                    axis.title = element_blank(),
                    axis.ticks = element_blank()
                ) +
                labs(
                    title = paste(
                        "Coauthorship Network of",
                        get_profile(input$AuthorID)$name
                    ),
                    subtitle = "Publications with more than one Google Scholar citation included",
                    caption = paste(
                        "Coauthors with at least",
                        input$MinCopublications,
                        "copublications"
                    )
                )
        }
    })
    
    
}

# Prepare the application ####

# Does the app run locally?
is_local <- (Sys.getenv('SHINY_PORT') == "")

# Install necessary packages ####
InstallPackages <- function(Packages) {
    sapply(Packages, function(Package) 
        if (!Package %in% installed.packages()[, 1]) {install.packages(Package)})
}

# Necessary packages (not run on shyniapps.io)
if(is_local) InstallPackages(c("shiny", "tidyverse", "magrittr", "scholar", "ggraph", "igraph"))
    
# Load packages
library("shiny")
library("tidyverse")
library("magrittr")
library("scholar")
library("ggraph")
library("igraph")


# Run the application ####
shinyApp(ui = ui, server = server)
