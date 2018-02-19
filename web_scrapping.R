library(RCurl)

#dataset
df <- read.csv("train.csv",stringsAsFactors = F)

#currency history
curr <- tolower(unique(df$currency))[-1]
for(i in 1:length(curr)){
  thepage <- getURL(paste("https://stooq.com/q/d/?s=usd",curr[i],"&c=0&d1=20090424&d2=20170323&i=q",sep=""))
  tables <- readHTMLTable(thepage)
  tables <- list.clean(tables, fun = is.null, recursive = FALSE)
  n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
  t <- tables[[which.max(n.rows)]]
  t <- as.data.frame(t[5:36,c(2,6)])
  t$curr <- toupper(curr[i])
  if(i==1){
    curr_tab <- t 
  }else{
    curr_tab <- rbind(curr_tab,t)
  }
}

colnames(curr_tab) <- c("date","rate","curr")
curr_tab$date <- as.Date(curr_tab$date,"%d %b %Y")
curr_tab$date <- as.Date(curr_tab$date,"%d %b %Y")
df$lastdaylaunched <- as.Date(as.yearqtr(as.Date(df$launched_at)))-1
curr_tab$date2 <- as.Date(as.yearqtr(as.Date(curr_tab$date))+0.4)-1
df$goalUS <- NA
for(i in 1:nrow(df)){
  if(df[i,"currency"]=="USD"){
    df[i,"goalUS"] <- df[i,"goal"]
  }else{
  cur <- df[i,"currency"]
  dat <- df[i,"lastdaylaunched"]
  rate <- curr_tab[which(curr_tab$curr==cur&curr_tab$date2==dat),"rate"]
  rate <- as.numeric(as.character(rate))
  df[i,"goalUS"] <- df[i,"goal"]*rate
  }
}
