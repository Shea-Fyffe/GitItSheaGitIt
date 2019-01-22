###Functions used to produce Grad Cafe analytics###
###for questions please email Shea Fyffe
###this script should be sourced into the "Mining_Grad_Cafe.r" script by passing the correct directory of this file as the "function_dir" object


###Function that finds maximum page number given search string and number of cases per page
get_pages <- function(p_url, n_page) { 
  
  require(rvest)
  
  p_url <- paste(p_url,"&t=a&pp=", n_page,"&o=&p=1", sep = "")
  
  pages <- read_html(p_url) %>%
    
    html_node('.pagination') %>%
    
    html_text()
  
  pages <-  as.numeric(sub(".*over *(.*?) * pages.*","\\1", pages))
  
  return(pages)}


###create function that lists URLS of all pages returned by the search criteria then returns a date.frame that combines tables from each search page
get_data <-  function(base_url, per_page, pn) { 
  require(rvest)
  
  df_list <-  lapply(paste0(base_url,"&t=a&pp=", per_page,"&o=&p=", 1:pn), 
    
    #embedded function that returns table
    function(df_url){
      
      read_html(df_url) %>%
        
        html_nodes(xpath='//*[@id="my-table"]') %>%
        
        .[[1]] %>%  
        
        html_table(header = TRUE, trim = TRUE)
      
    })
  
  #combine list of data frames into one
  df <- do.call("rbind", df_list)
  #rename columns
  names(df) <- c("Institution", "Program_AND_Season","Decision_AND_Date", "St1", "Date_Added", "Notes")
  
  
  rm(df_list)
  
  return(df)
}


#### static function to extract and consolidate decision values
add_new_columns <- function(data = df){

#extract and consolidate decision values
data$Decision <- ifelse( nchar (gsub(" via.*","",data$Decision_AND_Date)) > 11, "Other", gsub(" via.*","", data$Decision_AND_Date))

#extract and consolidate decision date values
data$Decision_Date <- ifelse( grepl(".*on ",data$Decision_AND_Date) == TRUE, gsub(".*on | Undergrad.*", "",data$Decision_AND_Date), data$Date_Added)
data$Decision_Date <- as.Date(data$Decision_Date, format = "%d %b %Y")

#extract day of the week from Decision_Date column
data$Decision_Day <- weekdays(as.Date(data$Decision_Date))

#clean Program (Season) column
data$Program <- gsub("*$\\s*\\([^\\)]+\\)", "", data$Program_AND_Season) #remove data within parenthsis
#Validate Masters or Phd Find, otherwise OTHER
data$Degree <- ifelse( gsub(".*, *(.*?) * [(].*$", "\\1" , data$Program_AND_Season) %in% c("PhD","Masters"), gsub(".*, *(.*?) * [(].*$", "\\1" , data$Program_AND_Season), "Other")

data$Program <- gsub("(.*),.*","\\1", data$Program) #Remove Data after the last Comma

data$GPA <- sapply(strsplit(data$Decision_AND_Date, ": "), "[", 2)
data$GPA <- sub('G.*$','', data$GPA)
data$GPA[(is.na(data$GPA)|data$GPA=="n/a")] <- "0"
data$GPA <- as.numeric(data$GPA)

data$GRE <- sapply(strsplit(data$Decision_AND_Date, ": "), "[", 3)

data$GRE_Verbal <- sapply(strsplit(data$GRE, "/"), "[", 1)
data$GRE_Verbal[(is.na(data$GRE_Verbal)|data$GRE_Verbal=="n/a")] <- "0"
data$GRE_Verbal <- as.numeric(data$GRE_Verbal)

data$GRE_Quant <- sapply(strsplit(data$GRE, "/"), "[", 2)
data$GRE_Quant[(is.na(data$GRE_Quant)|data$GRE_Quant=="n/a")] <- "0"
data$GRE_Quant <- as.numeric(data$GRE_Quant)

data$GRE_AWA <- sapply(strsplit(data$GRE, "/"), "[", 3)
data$GRE_AWA <- sub('G.*$','', data$GRE_AWA)
data$GRE_AWA[(is.na(data$GRE_AWA)|data$GRE_AWA=="n/a")] <- "0"
data$GRE_AWA <- as.numeric(data$GRE_AWA)

data$GRE <- NULL
return(data)}
####

clean_encoding <- function(data = df){
  data <- as.data.frame(sapply(data, function(x) iconv(x, "UTF-8", "ASCII", sub = "")), stringsAsFactors = FALSE)
  df[grep("^[^[:alnum:](]", df[,1]),]
  return(data)
}

clean_strings <- function(data = df){
  data<- as.data.frame(lapply(data, function(x) if(class(x)=="character"){ 
    x <- ifelse( grepl("^[^[:alnum:](]", x)==TRUE, gsub("^[[:punct:]]", "", x), x)
    x <- trimws(x, "both")
    x <- gsub(' +',' ', x)
    x <- gsub('?','', x)
    x <- gsub("the ", "", x, ignore.case = TRUE)
  }
    else(x)), stringsAsFactors= FALSE)
  
  return(data)
}

###This is a pile of hot garbage feel free to fix through the issues (it's functional though)
fuzzy_match <- function(df, x, y){
  xm <- df[,x]
  y <- tolower(y)
  xm <- xm[!xm %in% y]
  if (any(nchar(xm) < 5)){
    xms <- xm[nchar(xm) < 5]
    dists <- as.matrix(stringdist::stringdistmatrix(unique(xms), y, method = 'osa', weight = c(1,.10,1,1), useNames = "string" ))
    xml <- xm[nchar(xm) >= 5]
    distl <- as.matrix(stringdist::stringdistmatrix(unique(xml), y, method = 'osa', weight = c(1,1,1,1), useNames = "string" ))
    dist <- rbind(dists,distl)
    }
  else {
    dist <- as.matrix(stringdist::stringdistmatrix(unique(xm), y, method = 'osa', weight = c(1 ,1, 1,1), useNames = "string" ))
  }
  dist <- reshape2::melt(dist)
  names(dist)<- c("Value","Index","Distance")
  dist$Value <- as.character(dist$Value)
  dist$Index <- as.character(dist$Index)
  dist <- dist[dist$Distance <= 2.5 & dist$Distance != 0,]
  df[,x] <- ifelse(is.na(match(df[,x], dist$Value)) , df[,x], dist$Index[match(df[,x], dist$Value)])
  
  return(df)
}

#just some ad-hoc replacements I did....
clean_schools <- function(data = df, x){
  coln <- paste(x,"_Cleaned", sep = "")
  data[,coln] <- tolower(data[,x])
  data[,coln] <- gsub(pattern = "\\<uni\\w+|^uni\\w+|^u ", replacement = "university", x = data[,coln])
  data[,coln][(data[,coln]=="university of michigan, ann arbor"|data[,coln]=="university of michigan - ann arbor")] <- "university of michigan"
  data[,coln][(data[,coln]=="ucla"|data[,coln]=="university of california, los angeles (ucla)")] <- "university of california, los angeles"
  data[,coln][data[,coln]=="ucsd"] <- "university of california, san diego"
  data[,coln][data[,coln]=="university of maryland, college park"] <- "university of maryland"
  data[,coln][data[,coln]=="university of wisconsin, madison"] <- "university of wisconsin"
  data[,coln][data[,coln]=="university of colorado, boulder"] <- "university of colorado"
  data[,coln][(data[,coln]=="uva"|data[,coln]=="university of virginia (uva)")] <- "university of virginia"
  data[,coln][(data[,coln]=="nyu"|data[,coln]=="new york university (nyu)")] <- "new york university"
  data[,coln][data[,coln]=="upenn"] <- "university of pennsylvania"
  data[,coln][data[,coln]=="harvard"] <- "harvard university"
  data[,coln][data[,coln]=="yale"] <- "yale university"
  data[,coln][data[,coln]=="tufts"] <- "tufts university"
  data[,coln][data[,coln]=="duke"] <- "duke university"
  data[,coln][data[,coln]=="uga"] <- "university of georgia"
  data[,coln][(data[,coln]=="penn state"|data[,coln]=="penn state university")] <- "pennsylvania state university"
  return(data)
  }

clean_progs <- function(data = df, x){
  coln <- paste(x,"_Cleaned", sep = "")
  data[,coln] <- tolower(data[,x])
  data[,coln] <- gsub("[[:punct:]]", "", data[,coln])
  #general/applied
  data[,coln][data[,coln]=="psychology"] <- "general psychology"
  data[,coln][data[,coln]=="applied psychology"] <- "general psychology"
  data[,coln][data[,coln]=="psychology and social behavior"] <- "general psychology"
  data[,coln][data[,coln]=="psychology and social intervention"] <- "general psychology"
  
  #behavioral
  data[,coln][data[,coln]=="psychology behavioral"] <- "behavioral psychology"
  data[,coln][data[,coln]=="brain behavior and cogntion psychology"] <- "behavioral psychology"
  data[,coln][data[,coln]=="cognitive and behavioral neuroscience psychology"] <- "behavioral psychology"
  data[,coln][data[,coln]=="psychology neuroscience and behavior"] <- "behavioral psychology"
  
  #cog
  data[,coln][data[,coln]=="psychology cognitive"] <- "cognitive psychology"
  data[,coln][data[,coln]=="perception and cognition psychology"] <- "cognitive psychology"
  data[,coln][data[,coln]=="psychology cognition perception"] <- "cognitive psychology"
  data[,coln][data[,coln]=="psychology cognition and perception"] <- "cognitive psychology"
  
  #clinical
  data[,coln][data[,coln]=="psychology clinical"] <- "clinical psychology"
  data[,coln][data[,coln]=="adult clinical psychology psyd"] <- "clinical psychology"
  data[,coln][data[,coln]=="clinical psychology health emphasis"] <- "clinical psychology"
  data[,coln][data[,coln]=="clinical and counseling psychology"] <- "clinical psychology"
  data[,coln][data[,coln]=="childfamily clinical psychology"] <- "clinical psychology"
  data[,coln][data[,coln]=="clinical psychology clinical science"] <- "clinical psychology"
  data[,coln][data[,coln]=="clinical psychology child track"] <- "clinical psychology"
  data[,coln][data[,coln]=="clinical psychology health track"] <- "clinical psychology"
  
  #counseling
  data[,coln][data[,coln]=="psychology counseling"] <- "counseling psychology"
  data[,coln][data[,coln]=="combined program counseling psychology"] <- "counseling psychology"
  data[,coln][data[,coln]=="collaborative program in counseling psychology"] <- "counseling psychology"
 
  #dev
  data[,coln][data[,coln]=="psychology developmental"] <- "developmental psychology"
  data[,coln][data[,coln]=="applied developmental and educational psychology"] <- "developmental psychology"
  data[,coln][data[,coln]=="human development and psychology"] <- "developmental psychology"
  data[,coln][data[,coln]=="developmental and biological psychology"] <- "developmental psychology"
  
  #educational
  data[,coln][data[,coln]=="combined program in education and psychology"] <- "educational psychology"
  data[,coln][data[,coln]=="educational psychology and educational technology"] <- "educational psychology"
  data[,coln][data[,coln]=="educational psychology quantitative methods"] <- "educational psychology"
  
  
  #experimental
  data[,coln][data[,coln]=="psychology experimental"] <- "experimental psychology"
  data[,coln][data[,coln]=="applied experimental and human factors psychology"] <- "experimental psychology"
  data[,coln][data[,coln]=="research psychology"] <- "experimental psychology"
  data[,coln][data[,coln]=="experimental psychology developmental"] <- "experimental psychology"
  
  #forensic
  data[,coln][data[,coln]=="psychology forensic"] <- "forensic psychology"
  
  #health
  data[,coln][data[,coln]=="psychology health"] <- "health psychology"
  
  #neuro
  data[,coln][(data[,coln]=="behavioral neuroscience psychology"|data[,coln]=="psychology behavioral neuroscience")] <- "neuropsychology"
  data[,coln][data[,coln]=="psychology cognitive neuroscience"] <- "neuropsychology"
  data[,coln][data[,coln]=="clinical psychology neuropsychology"] <- "neuropsychology"
  data[,coln][data[,coln]=="philosophyneurosciencepsychology"] <- "neuropsychology"
  
  #i/o
  data[,coln][data[,coln]=="industrial organizational psychology io psychology"] <- "industrial-organizational psychology"
  data[,coln][data[,coln]=="io psychology industrialorganizational psychology"] <- "industrial-organizational psychology"
  data[,coln][data[,coln]=="io psychology"] <- "industrial-organizational psychology"
  data[,coln][data[,coln]=="psychology i/o"] <- "industrial-organizational psychology"
  data[,coln][data[,coln]=="socialorganizational psychology"] <- "industrial-organizational psychology"
  data[,coln][data[,coln]=="organizational psychology"] <- "industrial-organizational psychology"
  data[,coln][data[,coln]=="social organizational psychology"] <- "industrial-organizational psychology"
  
  #school
  data[,coln][data[,coln]=="psychology school"] <- "school psychology"
  data[,coln][data[,coln]=="school and clinical child psychology"] <- "school psychology"
  data[,coln][data[,coln]=="school and applied child psychology"] <- "school psychology"
  data[,coln][data[,coln]=="schoolclinical child psychology"] <- "school psychology"
  
  #social
  data[,coln][data[,coln]=="psychology social"] <- "social psychology"
  data[,coln][data[,coln]=="socialpersonality psychology"] <- "social psychology"
  data[,coln][data[,coln]=="social and personality psychology"] <- "social psychology"
  data[,coln][data[,coln]=="applied social and community psychology"] <- "social psychology"
  data[,coln][data[,coln]=="applied socialcommunity psychology"] <- "social psychology"
  data[,coln][data[,coln]=="affect and social psychology"] <- "social psychology"


  
  
  return(data)
}

closer_look <- function(data =df, x ="Month_Day"){
  data <- subset(data, Month_Day < "2018-05-01" | Month_Day > "2018-11-01" )
  xm <- format(as.Date(data[,x], format = "%Y-%m-%d"),"%m")
  d <- as.POSIXlt(data[,x])
  d$year <- d$year-1
  start <- as.Date("1970-01-01")
  data$New_Month_Day <- ifelse( as.numeric(xm) > 4, as.Date(d, format = "%Y-%m-%d"), as.Date(data[,x], format = "%Y-%m-%d"))
  data$New_Month_Day <- as.Date(data$New_Month_Day, origin=start)
  return(data)
}


agg_df <- function(data = df, x, y, z){
  out <- aggregate( data[,x] ~ data[,y] + data[,z] , data = data, function(x) length(x))
  names(out)<- c(y,z,"N")
  return(out)
}

##create graphs

plot_annual<- function (df , x , y, z, title){ 
  ggplot(data = df, aes(x = df[,x] , y = df[,y], group = df[,z], colour =  df[,z])) +
    geom_line(stat = "identity", size = .60) +
    scale_x_date(date_breaks = "2 weeks", date_labels = "%b %d", limits = as.Date(c('2018-01-01','2018-12-31'))) +
    ggtitle(title)+
    labs(x = "Date", y = "N") +
    scale_color_brewer(palette = "Spectral", direction = -1) +
    theme(panel.grid.major = element_line(size = 0.5, color = "gray92"), legend.title = element_blank(), 
          plot.title = element_text(face="bold", size=20, hjust=0),
          axis.line = element_line(size = 0.4, color = "black"), axis.text.y = element_text(size = 9), axis.text.x = element_text(size = 9, angle = 90, hjust = .5))}

plot_closer <- function(df,x,y,z, title){ 
  ggplot(data = df, aes(x = df[,x] , y = df[,y], group = df[,z], colour =  df[,z])) +
    geom_line(stat = "identity", size = .60) +
    scale_x_date(date_breaks = "5 days", date_labels = "%b %d") +
    ggtitle(title)+
    labs(x = "Date", y = "N") +
    scale_color_brewer(palette = "Spectral", direction = -1) +
    theme(panel.grid.major = element_line(size = 0.5, color = "gray92"), legend.title = element_blank(), 
          plot.title = element_text(face="bold", size=20, hjust=0),
          axis.line = element_line(size = 0.4, color = "black"), axis.text.y = element_text(size = 9), axis.text.x = element_text(size = 9, angle = 90, hjust = .5))}

plot_day<- function(df,x,y,title){ 
    ggplot(data = df, aes(x = df[,x], y = df[,y])) +
    geom_bar(stat = "identity", size = .65, fill = "cornflowerblue") +
    scale_x_discrete(limits=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")) +
    ggtitle(title)+
    labs(x = "Date", y = "Relative Frequency") +
    theme(panel.grid.major = element_line(size = 0.5, color = "gray92"), legend.title = element_blank(), 
          plot.title = element_text(face="bold", size=20, hjust=0),
          axis.line = element_line(size = 0.4, color = "black"), axis.text.y = element_text(size = 9), axis.text.x = element_text(size = 9, angle = 90, hjust = .5))}

#theme for table output
tt1<- ttheme_minimal(
  core = list(bg_params = list(fill = c("gray92","cornflowerblue"), col=NA),
              
              fg_params = list( fontface=1 )),
  
  colhead = list(fg_params= list( col= "navyblue", fontface= 4L, fontsize = 12)))

tt2<- ttheme_minimal(
  core = list(bg_params = list(fill = c("gray92","cornflowerblue"), col=NA),
              
              fg_params = list( fontface=1, cex = .8 )),
  
  colhead = list(fg_params= list( col= "navyblue", fontface= 4L, fontsize = 12)))

tt3<- ttheme_minimal(
  core = list(bg_params = list(fill = c("gray92","cornflowerblue"), col=NA),
              
              fg_params = list( fontface=1, cex = 1 )),
  
  colhead = list(fg_params= list( col= "navyblue", fontface= 4L, fontsize = 12)))


set_up_table <- function(x, tt1) {
  if (names(x)[2]=="Month_Day"){
    table <- tableGrob(x, theme = tt1, rows = NULL)
    h <- grobHeight(table)
    w <- grobWidth(table)
    title <- textGrob(paste("Top 10 ", x[1,1]," Dates", sep= ""), gp=gpar(fontsize=14))
  }
  else {
    table <- tableGrob(x, theme = tt1, rows = NULL)
    h <- grobHeight(table)
    w <- grobWidth(table)
    title <- textGrob(paste("Top 10 ", x[1,1]," Schools", sep= ""),gp=gpar(fontsize=14))
  }
  padding <- unit(0.5,"line")
  table <- gtable_add_rows(table, 
                           heights = grobHeight(title) + padding,
                           pos = 0)
  table <- gtable_add_grob(
    table, list(title),
    t = 1, l = 1, r = ncol(table) )
  return(table)}

plot_stats<- function (df , x , y, z, title){ 
  ymn <- df[,y] - df[,"SD"]
  ymx <- df[,y] + df[,"SD"]
  ylmn <- df[,y] - 1.5*df[,"SD"]
  ylmx <- df[,y] + 1.5*df[,"SD"]
  ggplot(data = df, aes(x = df[,x] , y = df[,y], fill = df[,z])) +
    geom_bar(stat = "identity", size = .45, position = position_dodge()) +
    geom_errorbar(aes(ymin= ymn, ymax= ymx), width=.2 , position = position_dodge(.9)) +
    scale_fill_brewer(palette = "Set1", direction = -1, type = "div") +
    geom_text(aes(label= sprintf("N = %s", df[,"N"])), vjust= 1.5 , color="white", position = position_dodge(0.9), size= 2.5) +
    coord_cartesian(ylim = c( min(ylmn) , max(ylmx) )) +
    ggtitle(names(title)) +
    labs(x = "", y = "Average") +
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
    theme(panel.grid.major = element_line(size = 0.5, color = "gray92"), legend.title = element_blank(), 
      plot.title = element_text(face="bold", size=20, hjust=0),
      axis.line = element_line(size = 0.4, color = "black"), axis.text.y = element_text(size = 9), axis.text.x = element_text(size = 8, hjust = 0.5, face = "bold"))}