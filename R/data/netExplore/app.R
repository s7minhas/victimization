#################
if(Sys.info()['user'] %in% c('Owner','herme','S7M')){
    source(paste0(
        'C:/Users/',Sys.info()['user'],
        '/Research/victimization/R/setup.R')) }
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
    source('~/Research/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('cassydorff')){
    source('~/ProjectsGit/victimization/R/setup.R') }
#################

#################
loadPkg(c(
    'shiny', 'shinyWidgets', 'scales'
))
#################

#################
# load data
load(paste0(pathData, 'actorAdjList.rda')) # yListAll
# load(paste0(pathData, 'netStats.rda')) # netStats
# load(paste0(pathData, 'actorAdjList_GED.rda')) # yListALL_GED
# load(paste0(pathData, 'netStatsGED.rda')) # netStatsGED
#################

#################
# get years
yrs = sort(unique(unlist(lapply(yListAll, names))))

# clean up countrynames in both ucdp and acled
cntries = names(yListAll)
cntries = countrycode(cntries, 'country.name', 'country.name')
names(yListAll) = cntries

# cntries = names(yListAll_GED)
# cntries = countrycode(cntries, 'country.name', 'country.name')
# names(yListAll_GED) = cntries

# label of object for ui/server
netList = yListAll
#################

#################
# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("Conflict Networks"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderTextInput(
                "plotYr", 
                "Year:",
                choices=yrs,
                selected="1997"
            ),
            selectInput(
                "cntry",
                "Country:",
                choices=sort(names(netList))
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("netPlot"),
           tableOutput("netTable")
        )
    )
)
#################

#################
# Define server logic
server <- function(input, output) {

    # prep data
    prepData <- reactive({
        
        # choose mat based on user input
        mat = netList[[input$cntry]][[char(input$plotYr)]]
        
        # make graph if it is not null
        if(!is.null(mat)){
            g = graph_from_adjacency_matrix(
                mat, mode='undirected', diag=FALSE, weighted=TRUE)
        } else {
            g = NULL
        }

        # 
        return(g)
    })
    
    # make net plot
    output$netPlot <- renderPlot({
      
        # get graph
        g = prepData()
        
        # exit if null
        validate(
            need(!is.null(g), 'No events for this country-year')
        )
        
        # rescale
        V(g)$size = degree(g, mode='total')
        V(g)$size = rescale(V(g)$size, 1, 25)
        
        # net plot
        set.seed(6886)
        par(mar=c(0,0,0,0))
        plot(
            g, 
            vertex.size = V(g)$size, 
            vertex.color='grey',
            vertex.label.color='black',
            vertex.label.cex=.75,
            edge.curved=.25,
            edge.color='grey20'
        )
    })
    
    # make netTable
    output$netTable <- renderTable({
        
        # pull out specific country from netList
        cntryList = netList[[input$cntry]]
        yrs = names(cntryList)
        
        # calc some stats
        nActors = unlist(lapply(cntryList, nrow))
        if(is.null(nActors)){ nActors = rep('No events', length(yrs)) }
        dens = suppressWarnings(
            round(
                unlist(lapply(cntryList, mean, na.rm=TRUE)),
                2)
        )
        dens = char(dens)
        dens[is.na(dens)] = 'No events'
        
        # org
        tab = cbind(Year=yrs, nActors, dens)
        rownames(tab) = NULL
        tab = data.frame(tab, stringsAsFactors = FALSE)
        return(tab)
    })
}
#################

#################
# Run the application 
shinyApp(ui = ui, server = server)
#################