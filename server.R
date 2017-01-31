library(RPostgreSQL)
library(ggplot2)
library(ggthemes)
library(shiny)
library(plotly)
library(shinythemes)
##================================================================================
drv = dbDriver("PostgreSQL")
connection<-dbConnect(drv,
  port="port",
  host="host",
  user="user",
  dbname="database",
  password="password")
full_data=dbGetQuery(connection, "SELECT * FROM mvp_race")
nba_2017 = dbGetQuery(connection, "SELECT * FROM nba_2017")
dbDisconnect(connection)
full_data=full_data[21:nrow(full_data),]
names(full_data)[5]="Score"
##================================================================================
factorlist=levels(factor(full_data$player))
images =read.csv("player_images.csv")
nba_2017=nba_2017[!duplicated(nba_2017$Player),]

viz=ggplot(full_data, aes(Date, Score)) +  geom_line(aes(text=player,colour = factor(player))) +
  xlab("") + ylab("MVP Score") + guides(fill=FALSE) + 
  theme_minimal()+
  scale_color_gdocs()+theme(legend.title=element_blank())+theme(text=element_text(family="Avenir"))


server <- function(input, output) {
  output$plot <- renderPlotly({
	ggplotly(viz, tooltip = c("text","Score"),source="a") %>% config(displaylogo = FALSE, modeBarButtonsToRemove = list('sendDataToCloud'), showLink=FALSE)
  })
  output$image <-renderUI({
  	event.data <- event_data("plotly_click",source="a")
  	if(is.null(event.data)){
  		tags$i("Click on line to see more player information")
  	}
  	else{
  		player_id=event.data$curveNumber+1
  		name=factorlist[player_id]
  		image_url=images[images$player==name,2]
    	tags$img(src=image_url)
  	}
  })     
  output$text <-renderUI({
  	event.data <- event_data("plotly_click",source="a")
  	if(is.null(event.data)){
  		h4("")
  	}
  	else{
  		player_id=event.data$curveNumber+1
  		pointnumber=event.data$pointNumber+1
  		name=factorlist[player_id]
  		image_url=images[images$player==name,2]
  		all_player_data =full_data[full_data$player==name,]
  		player_data=all_player_data[pointnumber,]
    	h3(name, br(),
    		paste0("Position: ", player_data[,2]),
    		br(),
    		paste0("Team: ",player_data[,3]),
    		br(),
    		paste0("Games Played: ",player_data[,4]),
    		br(),
    		paste0("MVP Score: ",prettyNum(player_data[,5],2)),
    		br(),
    		paste0("Date: ", format(player_data[,6],format="%b %d,%Y"))
		)
  	}
  }) 
  observe({
  	event.data <- event_data("plotly_click",source="a")
	if(is.null(event.data) == T){ return(h4(""))}
	else{
		player_id=event.data$curveNumber+1
		name=factorlist[player_id]
		playertest = nba_2017[nba_2017$Player==name,]
		output$games =renderPlotly({
			games=ggplot(playertest,aes(text=paste0("Games Played: ", totals_stats.G),1,y=totals_stats.G))+geom_point(colour="#E67300",size=3)+geom_hline(aes(text="League Average",yintercept=mean(nba_2017$totals_stats.G)))+geom_hline(aes(text="Standard Deviation",colour="#0099C6",yintercept=mean(nba_2017$totals_stats.G)-sd(nba_2017$totals_stats.G)))+geom_hline(aes(text="Standard Deviation",colour="#0099C6",yintercept=mean(nba_2017$totals_stats.G)+sd(nba_2017$totals_stats.G))) +theme_minimal()+scale_color_gdocs()+xlab("")+ylab("")+ggtitle("Games")+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+theme(text=element_text(family="Avenir"))+ theme(legend.position="none")
			ggplotly(games,tooltip="text") %>% config(displayModeBar = F)
	  	})
		output$minutesplayed =renderPlotly({
			minutesplayed=ggplot(playertest,aes(text=paste0("Minutes Played: ", totals_stats.MP),1,y=totals_stats.MP))+geom_point(colour="#E67300",size=3)+geom_hline(aes(text="League Average",yintercept=mean(nba_2017$totals_stats.MP)))+geom_hline(aes(text="Standard Deviation",colour="#0099C6",yintercept=mean(nba_2017$totals_stats.MP)-sd(nba_2017$totals_stats.MP)))+geom_hline(aes(text="Standard Deviation",colour="#0099C6",yintercept=mean(nba_2017$totals_stats.MP)+sd(nba_2017$totals_stats.MP))) +theme_minimal()+scale_color_gdocs()+xlab("")+ylab("")+ggtitle("Minutes")+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+theme(text=element_text(family="Avenir"))+ theme(legend.position="none")
			ggplotly(minutesplayed,tooltip="text") %>% config(displayModeBar = F)  	
	  	})
		output$points =renderPlotly({
			points=ggplot(playertest,aes(text=paste0("Points Made: ", totals_stats.PTS),1,y=totals_stats.PTS))+geom_point(colour="#E67300",size=3)+geom_hline(aes(text="League Average",yintercept=mean(nba_2017$totals_stats.PTS)))+geom_hline(aes(text="Standard Deviation",colour="#0099C6",yintercept=mean(nba_2017$totals_stats.PTS)-sd(nba_2017$totals_stats.PTS)))+geom_hline(aes(text="Standard Deviation",colour="#0099C6",yintercept=mean(nba_2017$totals_stats.PTS)+sd(nba_2017$totals_stats.PTS))) +theme_minimal()+scale_color_gdocs()+xlab("")+ylab("")+ggtitle("Points")+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+theme(text=element_text(family="Avenir"))+ theme(legend.position="none")
			ggplotly(points,tooltip="text") %>% config(displayModeBar = F)
	  	})
		output$fieldgoals =renderPlotly({
			fieldgoals=ggplot(playertest,aes(text=paste0("Field Goals %: ", totals_stats.FG.),1,y=totals_stats.FG.))+geom_point(colour="#E67300",size=3)+geom_hline(aes(text="League Average",yintercept=mean(nba_2017$totals_stats.FG.)))+geom_hline(aes(text="Standard Deviation",colour="#0099C6",yintercept=mean(nba_2017$totals_stats.FG.)-sd(nba_2017$totals_stats.FG.)))+geom_hline(aes(text="Standard Deviation",colour="#0099C6",yintercept=mean(nba_2017$totals_stats.FG.)+sd(nba_2017$totals_stats.FG.))) +theme_minimal()+scale_color_gdocs()+xlab("")+ylab("")+ggtitle("FG%")+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+theme(text=element_text(family="Avenir"))+ theme(legend.position="none")
			ggplotly(fieldgoals,tooltip="text") %>% config(displayModeBar = F)  	
	  	})
		output$freethrows =renderPlotly({
			freethrows=ggplot(playertest,aes(text=paste0("Free Throws %: ", totals_stats.FT.),1,y=totals_stats.FT.))+geom_point(colour="#E67300",size=3)+geom_hline(aes(text="League Average",yintercept=mean(nba_2017$totals_stats.FT.)))+geom_hline(aes(text="Standard Deviation",colour="#0099C6",yintercept=mean(nba_2017$totals_stats.FT.)-sd(nba_2017$totals_stats.FT.)))+geom_hline(aes(text="Standard Deviation",colour="#0099C6",yintercept=mean(nba_2017$totals_stats.FT.)+sd(nba_2017$totals_stats.FT.))) +theme_minimal()+scale_color_gdocs()+xlab("")+ylab("")+ggtitle("FT%")+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+theme(text=element_text(family="Avenir"))+ theme(legend.position="none")
			ggplotly(freethrows,tooltip="text") %>% config(displayModeBar = F)
		})
		output$per =renderPlotly({
			per=ggplot(playertest,aes(text=paste0("PER: ", advanced_stats.PER),1,y=advanced_stats.PER))+geom_point(colour="#E67300",size=3)+geom_hline(aes(text="League Average",yintercept=mean(nba_2017$advanced_stats.PER)))+geom_hline(aes(text="Standard Deviation",colour="#0099C6",yintercept=mean(nba_2017$advanced_stats.PER)-sd(nba_2017$advanced_stats.PER)))+geom_hline(aes(text="Standard Deviation",colour="#0099C6",yintercept=mean(nba_2017$advanced_stats.PER)+sd(nba_2017$advanced_stats.PER))) +theme_minimal()+scale_color_gdocs()+xlab("")+ylab("")+ggtitle("PER")+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+theme(text=element_text(family="Avenir"))+ theme(legend.position="none")
			ggplotly(per,tooltip="text") %>% config(displayModeBar = F)

	  	})
		output$threepoint =renderPlotly({
			threepoint=ggplot(playertest,aes(text=paste0("3P%: ", totals_stats.3P.),1,y=totals_stats.3P.))+geom_point(colour="#E67300",size=3)+geom_hline(aes(text="League Average",yintercept=mean(nba_2017$totals_stats.3P.)))+geom_hline(aes(text="Standard Deviation",colour="#0099C6",yintercept=mean(nba_2017$totals_stats.3P.)-sd(nba_2017$totals_stats.3P.)))+geom_hline(aes(text="Standard Deviation",colour="#0099C6",yintercept=mean(nba_2017$totals_stats.3P.)+sd(nba_2017$totals_stats.3P.))) +theme_minimal()+scale_color_gdocs()+xlab("")+ylab("")+ggtitle("3P%")+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+theme(text=element_text(family="Avenir"))+ theme(legend.position="none")
			ggplotly(threepoint,tooltip="text") %>% config(displayModeBar = F)
	  	})
		output$assists =renderPlotly({
			assists=ggplot(playertest,aes(text=paste0("Assists: ", totals_stats.AST),1,y=totals_stats.AST))+geom_point(colour="#E67300",size=3)+geom_hline(aes(text="League Average",yintercept=mean(nba_2017$totals_stats.AST)))+geom_hline(aes(text="Standard Deviation",colour="#0099C6",yintercept=mean(nba_2017$totals_stats.AST)-sd(nba_2017$totals_stats.AST)))+geom_hline(aes(text="Standard Deviation",colour="#0099C6",yintercept=mean(nba_2017$totals_stats.AST)+sd(nba_2017$totals_stats.AST))) +theme_minimal()+scale_color_gdocs()+xlab("")+ylab("")+ggtitle("Assists")+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+theme(text=element_text(family="Avenir"))+ theme(legend.position="none")
			ggplotly(assists,tooltip="text") %>% config(displayModeBar = F)
		})
		output$rebounds =renderPlotly({
			rebounds=ggplot(playertest,aes(text=paste0("Rebounds: ", totals_stats.TRB),1,y=totals_stats.TRB))+geom_point(colour="#E67300",size=3)+geom_hline(aes(text="League Average",yintercept=mean(nba_2017$totals_stats.TRB)))+geom_hline(aes(text="Standard Deviation",colour="#0099C6",yintercept=mean(nba_2017$totals_stats.TRB)-sd(nba_2017$totals_stats.TRB)))+geom_hline(aes(text="Standard Deviation",colour="#0099C6",yintercept=mean(nba_2017$totals_stats.TRB)+sd(nba_2017$totals_stats.TRB))) +theme_minimal()+scale_color_gdocs()+xlab("")+ylab("")+ggtitle("Total Rebounds")+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+theme(text=element_text(family="Avenir"))+ theme(legend.position="none")
			ggplotly(rebounds,tooltip="text") %>% config(displayModeBar = F)  	
	  	})
		output$steals =renderPlotly({
			steals=ggplot(playertest,aes(text=paste0("Steals: ", totals_stats.STL),1,y=totals_stats.STL))+geom_point(colour="#E67300",size=3)+geom_hline(aes(text="League Average",yintercept=mean(nba_2017$totals_stats.STL)))+geom_hline(aes(text="Standard Deviation",colour="#0099C6",yintercept=mean(nba_2017$totals_stats.STL)-sd(nba_2017$totals_stats.STL)))+geom_hline(aes(text="Standard Deviation",colour="#0099C6",yintercept=mean(nba_2017$totals_stats.STL)+sd(nba_2017$totals_stats.STL))) +theme_minimal()+scale_color_gdocs()+xlab("")+ylab("")+ggtitle("Total Steals")+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+theme(text=element_text(family="Avenir"))+ theme(legend.position="none")
			ggplotly(steals,tooltip="text") %>% config(displayModeBar = F)
	  	})
		output$blocks =renderPlotly({	
			blocks=ggplot(playertest,aes(text=paste0("Blocks: ", totals_stats.BLK),1,y=totals_stats.BLK))+geom_point(colour="#E67300",size=3)+geom_hline(aes(text="League Average",yintercept=mean(nba_2017$totals_stats.BLK)))+geom_hline(aes(text="Standard Deviation",colour="#0099C6",yintercept=mean(nba_2017$totals_stats.BLK)-sd(nba_2017$totals_stats.BLK)))+geom_hline(aes(text="Standard Deviation",colour="#0099C6",yintercept=mean(nba_2017$totals_stats.BLK)+sd(nba_2017$totals_stats.BLK))) +theme_minimal()+scale_color_gdocs()+xlab("")+ylab("")+ggtitle("Total Blocks")+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+theme(text=element_text(family="Avenir"))+ theme(legend.position="none")
			ggplotly(blocks,tooltip="text") %>% config(displayModeBar = F)
	  	})
		output$turnovers =renderPlotly({
			turnovers=ggplot(playertest,aes(text=paste0("Turnovers: ", totals_stats.TOV),1,y=totals_stats.TOV))+geom_point(colour="#E67300",size=3)+geom_hline(aes(text="League Average",yintercept=mean(nba_2017$totals_stats.TOV)))+geom_hline(aes(text="Standard Deviation",colour="#0099C6",yintercept=mean(nba_2017$totals_stats.TOV)-sd(nba_2017$totals_stats.TOV)))+geom_hline(aes(text="Standard Deviation",colour="#0099C6",yintercept=mean(nba_2017$totals_stats.TOV)+sd(nba_2017$totals_stats.TOV))) +theme_minimal()+scale_color_gdocs()+xlab("")+ylab("")+ggtitle("Total Turnovers")+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+theme(text=element_text(family="Avenir"))+ theme(legend.position="none")
			ggplotly(turnovers,tooltip="text") %>% config(displayModeBar = F)
	  	})
		output$winshare =renderPlotly({
			winshare=ggplot(playertest,aes(text=paste0("WS48: ", advanced_stats.WS.48),1,y=advanced_stats.WS.48))+geom_point(colour="#E67300",size=3)+geom_hline(aes(text="League Average",yintercept=mean(nba_2017$advanced_stats.WS.48)))+geom_hline(aes(text="Standard Deviation",colour="#0099C6",yintercept=mean(nba_2017$advanced_stats.WS.48)-sd(nba_2017$advanced_stats.WS.48)))+geom_hline(aes(text="Standard Deviation",colour="#0099C6",yintercept=mean(nba_2017$advanced_stats.WS.48)+sd(nba_2017$advanced_stats.WS.48))) +theme_minimal()+scale_color_gdocs()+xlab("")+ylab("")+ggtitle("WinShare48")+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+theme(text=element_text(family="Avenir"))+ theme(legend.position="none")
			ggplotly(winshare,tooltip="text") %>% config(displayModeBar = F)  	
	  	})
	}

  })

	observe({
		player_index=nba_2017$Player==input$select2Input1
		scaled=scale(nba_2017[,c(5:41,43:46,48:51)])
		player=scaled[player_index,]
		distance=as.matrix(dist(scaled))
		test=distance[player_index,!player_index]

		big_test=nba_2017[!player_index,]
		comparable=big_test[which(test %in% sort(test)[1:5]),]
		final=rbind(nba_2017[player_index,],comparable)
		image_url=images[images$player==input$select2Input1,2]
		output$winshare2= renderPlotly({
			ggplotly(ggplot(final, aes(x=Player,y=advanced_stats.WS.48))+geom_point(aes(colour=factor(Player)))+theme_minimal()+scale_color_gdocs()+xlab("")+ylab("")+ggtitle("WS 48")+theme(text=element_text(family="Avenir"))+ theme(legend.position="none")+ theme(axis.text.x = element_text(angle=45,size=8)),height=300,tooltip=c("Player","advanced_stats.WS.48")) %>% config(displayModeBar = F)
		})
		output$points2= renderPlotly({
			ggplotly(ggplot(final, aes(x=Player,y=totals_stats.PTS))+geom_point(aes(colour=factor(Player)))+theme_minimal()+scale_color_gdocs()+xlab("")+ylab("")+ggtitle("PTS")+theme(text=element_text(family="Avenir"))+ theme(legend.position="none")+ theme(axis.text.x = element_text(angle=45,size=8)),height=300,tooltip=c("Player","totals_stats.PTS")) %>% config(displayModeBar = F)
		})
		output$per2= renderPlotly({
			ggplotly(ggplot(final, aes(x=Player,y=advanced_stats.PER))+geom_point(aes(colour=factor(Player)))+theme_minimal()+scale_color_gdocs()+xlab("")+ylab("")+ggtitle("PER")+theme(text=element_text(family="Avenir"))+ theme(legend.position="none")+ theme(axis.text.x = element_text(angle=45,size=8)),height=300,tooltip=c("Player","advanced_stats.PER")) %>% config(displayModeBar = F)
		})
		output$assists2= renderPlotly({
			ggplotly(ggplot(final, aes(x=Player,y=totals_stats.AST))+geom_point(aes(colour=factor(Player)))+theme_minimal()+scale_color_gdocs()+xlab("")+ylab("")+ggtitle("AST")+theme(text=element_text(family="Avenir"))+ theme(legend.position="none")+ theme(axis.text.x = element_text(angle=45,size=8)),height=300,tooltip=c("Player","totals_stats.AST")) %>% config(displayModeBar = F)
		})
		output$rebounds2= renderPlotly({
			ggplotly(ggplot(final, aes(x=Player,y=totals_stats.TRB))+geom_point(aes(colour=factor(Player)))+theme_minimal()+scale_color_gdocs()+xlab("")+ylab("")+ggtitle("Rebounds")+theme(text=element_text(family="Avenir"))+ theme(legend.position="none")+ theme(axis.text.x = element_text(angle=45,size=8)),height=300,tooltip=c("Player","totals_stats.TRB")) %>% config(displayModeBar = F)
		})
		output$fieldgoals2= renderPlotly({
			ggplotly(ggplot(final, aes(x=Player,y=totals_stats.FG.))+geom_point(aes(colour=factor(Player)))+theme_minimal()+scale_color_gdocs()+xlab("")+ylab("")+ggtitle("FG %")+theme(text=element_text(family="Avenir"))+ theme(legend.position="none")+ theme(axis.text.x = element_text(angle=45,size=8)),height=300,tooltip=c("Player","totals_stats.FG.")) %>% config(displayModeBar = F)
		})

		output$text2=renderUI({
			if(input$select2Input1==""){
				h4("")
			}
			else{
				h5(tags$img(src=images[images$player==comparable[1,1],2],width=110,height=80),br(),comparable[1,1],br(),
	    			paste0("Position: ", comparable[1,2]),
	    			br(),
		    		paste0("Team: ",comparable[1,4]),
		    		br(),
		    		paste0("Games Played: ",comparable[1,5]),br(),
					tags$img(src=images[images$player==comparable[2,1],2],width=110,height=80),br(),comparable[2,1],br(),
	    			paste0("Position: ", comparable[2,2]),
	    			br(),
		    		paste0("Team: ",comparable[2,4]),
		    		br(),
		    		paste0("Games Played: ",comparable[2,5]),br(),tags$img(src=images[images$player==comparable[3,1],2],width=110,height=80),br(),comparable[3,1],br(),
	    			paste0("Position: ", comparable[3,2]),
	    			br(),
		    		paste0("Team: ",comparable[3,4]),
		    		br(),
		    		paste0("Games Played: ",comparable[3,5])
		    	)
			}
		})
		output$text4=renderUI({
			if(input$select2Input1==""){
				h4("")
			}
			else{
				h5(
					tags$img(src=images[images$player==comparable[4,1],2],width=110,height=80),br(),comparable[4,1],br(),
	    			paste0("Position: ", comparable[4,2]),
	    			br(),
		    		paste0("Team: ",comparable[4,4]),
		    		br(),
		    		paste0("Games Played: ",comparable[4,5]),
					br(),tags$img(src=images[images$player==comparable[5,1],2],width=110,height=80),br(),comparable[5,1],br(),
	    			paste0("Position: ", comparable[5,2]),
	    			br(),
		    		paste0("Team: ",comparable[5,4]),
		    		br(),
		    		paste0("Games Played: ",comparable[5,5]))
			}
			})
		output$header1=renderUI({
			if(input$select2Input1==""){
				h4("")
			}
			else{
				h2("Entry")
			}
			})
		output$header2=renderUI({
			if(input$select2Input1==""){
				h4("")
			}
			else{
				h2("Comparable Players")
			}			
			})
		output$text3=renderUI({
			if(input$select2Input1==""){
				h4("")
			}
			else{

				h3(tags$img(src=image_url),br(),nba_2017[player_index,1],br(),
	    			paste0("Position: ", nba_2017[player_index,2]),
	    			br(),
		    		paste0("Team: ",nba_2017[player_index,4]),
		    		br(),
		    		paste0("Games Played: ",nba_2017[player_index,5])
		    	)
			}
		})
	
	})

}