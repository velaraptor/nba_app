library(RPostgreSQL)
library(ggplot2)
library(ggthemes)
library(shiny)
library(plotly)
library(shinythemes)
library(shinysky)

drv = dbDriver("PostgreSQL")
connection<-dbConnect(drv,
  port="port",
  host="host",
  user="user",
  dbname="database",
  password="password")
nba_2017 = dbGetQuery(connection, "SELECT * FROM nba_2017")
full_data=dbGetQuery(connection, "SELECT * FROM mvp_race")
dbDisconnect(connection)
nba_2017=nba_2017[!duplicated(nba_2017$Player),]

player_list = nba_2017$Player

ui <- fluidPage(theme = shinytheme("journal"),tags$head(includeScript("google_analytics.js")),
	    navbarPage(title="NBA Analytics", id="nav",windowTitle="Velaraptor's NBA Analytics",
	    	tabPanel("MVP Race",
	    		div(class="outer",
        				tags$head(
        					tags$link(
        						rel = "icon", type = "image/png", href = "favicon2.png"))), 
	    		 h2(img(src="favicon.png",height=50,width=50),"2017 Day-by-Day MVP Race"),
	    		 plotlyOutput("plot"),
	    		 hr(),
	    		 fluidRow(
	    		 	column(6,offset=1,htmlOutput("image")),
	    		 	column(4,htmlOutput("text"))
	    		 ),
	    		 fluidRow(
	    		 	column(2,plotlyOutput("games", height = "230px",width="auto")),
	    		 	column(2,plotlyOutput("minutesplayed", height = "230px")),
	    		 	column(2,plotlyOutput("fieldgoals", height = "230px")),
	    		 	column(2,plotlyOutput("freethrows", height = "230px")),
	    		 	column(2,plotlyOutput("per", height = "230px")),
	    		 	column(2,plotlyOutput("threepoint", height = "230px"))
	    		 	),
	    		 fluidRow(
	    		 	column(2,plotlyOutput("assists", height = "230px")),
	    		 	column(2,plotlyOutput("rebounds", height = "230px")),
	    		 	column(2,plotlyOutput("steals" ,height = "230px")),
	    		 	column(2,plotlyOutput("blocks", height = "230px")),
	    		 	column(2,plotlyOutput("turnovers", height = "230px")),
	    		 	column(2,plotlyOutput("winshare", height = "230px"))
	    		 	),
	    		 h5("*Black Line represents League Average. Blue lines represent one standard deviation."),
	    		 hr(),
	    		 h4("Click on player on right side legend to remove that player from the graph."),
	    		 h4("This data only shows the top 20 players from each calendar day."),
	    		 h5("This page will be updated daily, data to create this score comes from", a(href="http://www.basketball-reference.com","Basketball-Reference",target="_blank"), "and", a(href="http://www.hoopshype.com","HoopsHype",target="_blank"),"."),
	    		 h5("Player images from ESPN Player Directory."),
	    		 h5("More information on how I created this statistic can be seen here:", a(href = "https://github.com/velaraptor/nba_mvp_metric/blob/master/pdf%20report/adafinalproject%20(1).pdf?raw=true", "Click Here!",target="_blank")),
	    		 h5("Contact me @ ", a(href="mailto:christophvel@gmail.com","christophvel@gmail.com"), " or Twitter: ", a(href="http://twitter.com/velaraptor","@velaraptor",target="_blank"), "for questions, information, or recommendations.")
	    	),
			tabPanel("Similar Players Search", 
	    		h2(img(src="favicon.png",height=50,width=50),"2017 Fantasy Basketball - Similar Players"),
	    		helpText("Select one player and see the top 5 most similar players to that player."),
	    		helpText(paste0("Data current as of: ",format(full_data$Date[nrow(full_data)],format="%b %d,%Y"))),
	    		textInput.typeahead(id="select2Input1",
                                  placeholder="Type players name",
                                  local=data.frame(name=c(player_list)),
                                  valueKey = "name",
                                  tokens=c(1:length(player_list)),
                                  template = HTML("<p class='repo-language'>{{info}}</p> <p class='repo-name'>{{name}}</p>")
              	 ),
	    		 hr(),
	    		 fluidRow(
	    		 	column(6,offset=1,htmlOutput("header1")),
	    		 	column(5,htmlOutput("header2"))  		 		    		 	
	    		 ),
	    		 fluidRow(
	    		 	column(6,offset=1,htmlOutput("text3")),
	    		 	column(2,htmlOutput("text2")),
	    		 	column(2,htmlOutput("text4"))  		 		    		 	
	    		 ),
	    		 hr(),
	    		 fluidRow(
	    		 	column(6,plotlyOutput("winshare2", height = "300px",width="auto")),
	    		 	column(6,plotlyOutput("per2", height = "300px",width="auto"))
	    		 	),
	    		 fluidRow(
	    		 	column(6,plotlyOutput("points2", height = "300px",width="auto")),
	    		 	column(6,plotlyOutput("assists2", height = "300px",width="auto"))
	    		 	),
	    		 fluidRow(
	    		 	column(6,plotlyOutput("rebounds2", height = "300px",width="auto")),
	    		 	column(6,plotlyOutput("fieldgoals2", height = "300px",width="auto"))
	    		 	),	    		 		    		 
	    		 hr()

	    	)
	    )
)