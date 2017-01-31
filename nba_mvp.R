library(XML)
library(MASS)
library(plyr)
library(RPostgreSQL)
## env EDITOR=vim crontab -e
## PATH=/usr/local/sbin:/usr/local/bin:/sbin:/bin:/usr/sbin:/usr/bin
##28 11 * * * Rscript $HOME/Desktop/nba_shinyapp/nba_mvp.R no-save no-restore >> $HOME/Desktop/nba_shinyapp/logs/update.log
## crontab -l
## for ubuntu: 0 16 * * * Rscript /home/ubuntu/scripts/nba_mvp.R no-save -no-restore >> /home/ubuntu/scripts/logs/update.log
##================================================================================
thir<-readHTMLTable("http://www.basketball-reference.com/leagues/NBA_2017_totals.html")
thir.1<-as.data.frame(thir)
thir.adv<-readHTMLTable("http://www.basketball-reference.com/leagues/NBA_2017_advanced.html")
thir.adv.1<-as.data.frame(thir.adv)

change.num.totals<-function(list){
	team<-list;
	for( i in c(4,6:length(team))){
		team[,i]<-as.character(team[,i]);
		team[,i]<-as.numeric(team[,i]);	
	}
	return(team)
}

change.num.advanced<-function(list){
	team<-list;
	for( i in c(4,6:length(team))){
		team[,i]<-as.character(team[,i]);
		team[,i]<-as.numeric(team[,i]);	
	}
	return(team)
}
thir.1.totals<-change.num.totals(thir.1)
thir.1.adv<-change.num.advanced(thir.adv.1)
delete.row<-function(list){
	team<-list;
	team.1<-team[-grep("Player",team[,2]),]
	return(team.1)
}
thir.1.totals<-delete.row(thir.1.totals)
thir.1.adv<-delete.row(thir.1.adv)
has.star<-function(list){
	team<-list;
	team[,2]<-as.character(team[,2]);
	check.star<-grep("\\*$",team[,2]);
	team[check.star,2]<-gsub("\\*$","",team[check.star,2]);
	return(team);
}
has.multiple.entry<-function(list){
	team<-list;
	team<-team[!duplicated(team[,2]),];
	return(team);	
}
thir.1.totals<-has.star(thir.1.totals)
thir.1.totals<-has.multiple.entry(thir.1.totals)
thir.1.adv<-has.star(thir.1.adv)
thir.1.adv<-has.multiple.entry(thir.1.adv)
names(thir.1.adv)[2]="advanced.Player"
names(thir.1.totals)[2]="totals.Player"
nba_2013<-merge(thir.1.totals,thir.1.adv,by.x="totals.Player",by.y="advanced.Player")
fix.columns.ndf<-function(list){
 	x<-list;
 	x<-data.frame(x,0);
 	colnames(x)[59]<-"Pts.Won";
 	colnames(x)[1]<-"Player";
 	x$totals_stats.Rk<-NULL;
 	x$advanced_stats.Rk<-NULL;
 	x$advanced_stats.Pos<-NULL;
 	x$advanced_stats.Age<-NULL;
 	x$advanced_stats.Tm<-NULL;
 	x$advanced_stats.G<-NULL;
 	x$advanced_stats.MP<-NULL;
	return(x);
}
nba_2013<-fix.columns.ndf(nba_2013)
##================================================================================
salary<-file.path("http://hoopshype.com/salaries",c("brooklyn_nets","atlanta_hawks","boston_celtics","charlotte_hornets","chicago_bulls","cleveland_cavaliers","dallas_mavericks","denver_nuggets","detroit_pistons","golden_state_warriors","houston_rockets","indiana_pacers","los_angeles_clippers","los_angeles_lakers","memphis_grizzlies","miami_heat","milwaukee_bucks","minnesota_timberwolves","new_orleans_pelicans","new_york_knicks","oklahoma_city_thunder","orlando_magic","philadelphia_76ers","phoenix_suns","portland_trail_blazers","sacramento_kings","san_antonio_spurs","toronto_raptors","utah_jazz","washington_wizards"))
sal<-lapply(salary,readHTMLTable)
names.extract<-function(list){
	newlist<-list(0)
	for(i in 1:30){
		names(list[[i]])<-c("sal","a","b")
	}
	for(i in 1:30){
		newlist[[i]]<-list[[i]]$sal
	}
	return(newlist)
}
sal.1<-names.extract(sal)
s<-lapply(sal.1,as.data.frame)
df<-ldply(s,data.frame)
df<-df[,1:2]

df[,1]<-as.character(df[,1])
delete.money<-function(data){
	team<-data;
	team[,2]<-as.character(team[,2]);
	check.money<-grep("[[:punct:]]",team[,2]);
	team[check.money,2]<-gsub("[[:punct:]]","",team[check.money,2]);
	return(team)
}
df<-delete.money(df)
df[,2]<-as.numeric(df[,2])
df=df[!is.na(df[,2]),]
sum.total<-sum(df[,2])

names(df)[2]<-"Salary"
df$Salary<-df$Salary/sum.total
##================================================================================

##================================================================================
nba_2017.1<-merge(nba_2013,df,by.x="Player",by.y="Player")
min_games=max(nba_2017.1$totals_stats.G)*.7
    
nba_2017<-nba_2017.1[nba_2017.1$totals_stats.G>=min_games,]   
nba_2017[is.na(nba_2017)]<-0
##================================================================================
pre<-.5647*nba_2017$advanced_stats.WS + 3.14 *nba_2017$Salary -.0002975* nba_2017$advanced_stats.USG  -.0003054 *nba_2017$totals_stats.FT + .006238 * nba_2017$advanced_stats.TOV - 1.028 * nba_2017$advanced_stats.TS -.0003448 * nba_2017$totals_stats.PF -.001252 * nba_2017$advanced_stats.AST- .00009456 * nba_2017$totals_stats.FG -.0001489 * nba_2017$totals_stats.MP -.004903 * nba_2017$advanced_stats.PER + .0005163 * nba_2017$totals_stats.TOV- .01492 * nba_2017$advanced_stats.STL +.0001946 * nba_2017$totals_stats.PTS 
m<-data.frame(nba_2017$Player,nba_2017$totals_stats.Pos,nba_2017$totals_stats.Tm,nba_2017$totals_stats.G,pre)
##================================================================================
top.order<-order(m[,5],decreasing=T)
toporder=m[top.order,]
toporder$Date=Sys.Date() 
top=toporder[1:20,]
##================================================================================
drv = dbDriver("PostgreSQL")
connection<-dbConnect(drv,
  port="port",
  host="host",
  user="user",
  dbname="database",
  password="password")
current_data=dbGetQuery(connection, "SELECT * FROM mvp_race")
print("===============")
print(Sys.time())
if(current_data[nrow(current_data),6]==format(Sys.time(),tz="US/Central",usetz=TRUE)){
	print("Already Done!")
}else{
	print("MVP_race Table Wrote:")
	dbWriteTable(connection, "mvp_race", value=top,append=TRUE,row.names=FALSE)
	print("Full Data Table Wrote:")
	dbWriteTable(connection, "nba_2017",value=nba_2017,overwrite=TRUE,row.names=FALSE)
	print("Updated Data!")
}
print("DB Disconnect:")
dbDisconnect(connection)
print("===============")

