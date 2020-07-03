### todos:

# make things pretty

# to get at the star level thing, we can also have another table that
# shows a sorting of actors by degree

#################
if(Sys.info()['user'] %in% c('Owner','herme','S7M')){
    source(paste0(
        'C:/Users/',Sys.info()['user'],
        '/Research/victimization/R/setup.R')) }
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
    source('~/Research/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('dorffc')){
    source('~/ProjectsGit/victimization/R/setup.R') }
#################

#################
loadPkg(c(
    'shiny', 'shinyWidgets',
    'scales', 'visNetwork'
))
#################

#################
# load data
load(paste0(pathData, 'actorAdjList.rda')) # yListAll
load(paste0(pathData, 'netStats.rda')) # netStats
load(paste0(pathData, 'data.rda')) # modeling file
#################

#################
# get years
yrs = sort(unique(unlist(lapply(yListAll, names))))

# clean up countrynames in both acled and ucdp
# for both the adjLists and netStats

cleanListNames = function(x){
  cntries = names(x)
  cntries = countrycode(cntries, 'country.name', 'country.name')
  names(x) = cntries
  return(x) }

# acled adj list
yListAll = cleanListNames(yListAll)

# acled net stats
netStats = cleanListNames(netStats)

# vector of graph stats from net stats
# for use in table output
graphStats = names(netStats[[1]])[
  grepl('graph_',names(netStats[[1]]))]

# when using acled graph_recip will equal one
# change when modifying to choosing between acled/ucdp
graphStats = graphStats[-length(graphStats)]

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
            ),
            selectInput(
              'vicGraphCompare',
              'Graph Measure to Compare Vic Against:',
              choices=sort(graphStats),
              selected='graph_avgDeg',
              multiple = TRUE
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
          visNetworkOutput("netPlot", height='700px', width='100%'),
          plotOutput("vicCompare", width='80%'),
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
    output$netPlot <- renderVisNetwork({

        # get graph
        g = prepData()

        # exit if null
        validate(
            need(!is.null(g), 'No events for this country-year')
        )


        ## create plotly version
        # extract nodes and edges from graph
        edges = data.frame(as_edgelist(g), stringsAsFactors=FALSE)
        names(edges) = c('from', 'to')
        nodes = data.frame(id=V(g)$name, stringsAsFactors = FALSE)

        # set attributes for nodes
        nodes$title = nodes$id
        nodes$label = nodes$id
        nodes$size = rescale(degree(g, mode='total'), to=c(10, 30))
        nodes$color.background = 'slategrey'
        nodes$color.border = 'black'

        # set special node attribute for gov actors
        nodes$gov = grepl(
          'Gov Forces of',
          nodes$id
          )
        nodes$color.background[nodes$gov] = 'gold'

        # set attributes for edges
        edges$width = 3
        edges$color = 'black'
        edges$smooth = TRUE

        # viz
        visNetwork(nodes, edges, width='100%')
    })

    # extract network data from netStats
    prepNetTabData <- reactive({

      # pull in cntrylevel results (at the level of actor)
      actorStats = netStats[[input$cntry]]

      # calculate number of actors for every year
      nVec = tapply(actorStats$actor, actorStats$year, length)

      # subset to relev unique rows and only graph level
      # columns
      cntryStats = unique(
        actorStats[,c('year', graphStats)] )

      # merge nActors and reorg table order
      cntryStats = cbind(cntryStats, nActors=nVec)
      cntryStats = cntryStats[,c('year', 'nActors', graphStats)]

      # cleanup
      cntryStats$year = as.integer(cntryStats$year)

      #
      return(cntryStats)
    })

    # make netTable
    output$netTable <- renderTable({

        # get net stats for table
        cntryStatsForTab = prepNetTabData()

        # cleanup
        cntryStatsForTab[,graphStats] = round(cntryStatsForTab[,graphStats], 2)
        rownames(cntryStatsForTab) = NULL

        #
        return(cntryStatsForTab)
    })

  # make viz comparing net stats to vic
  output$vicCompare <- renderPlot({

    # get vic data from iData_acled result (data)
    vicData = data[data$cname == input$cntry,
    c(
      'cname','year',
      'civVicCount'
      ) ]

    # get net stats for plot
    cntryNetStats = prepNetTabData()

    # merge
    toMerge = setdiff(names(cntryNetStats), 'year')
    for(v in toMerge){
      vicData$tmp = cntryNetStats[match(vicData$year, cntryNetStats$year), v]
      names(vicData)[ncol(vicData)] = v }

    # adj data for plotting
    ggData = pivot_longer(vicData,
      cols=civVicCount:graph_centrz,
      names_to = 'graphMeasure',
      values_to = 'value'
      )

    # bring in user input to limit graph measures
    toPlot = c('civVicCount', input$vicGraphCompare)
    ggData = ggData[which(ggData$graphMeasure %in% toPlot),]

    # plot
    ggplot(ggData, aes(x=year, y=value)) +
      geom_line() + geom_point() +
      facet_wrap(~graphMeasure, ncol=1, scales='free_y') +
      labs(
        x=''
      ) +
      theme(
        axis.ticks=element_blank(),
        panel.border=element_blank()
      )
  })
}
#################

#################
# Run the application
shinyApp(ui = ui, server = server)
#################
