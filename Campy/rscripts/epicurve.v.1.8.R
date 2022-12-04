

#################################################
# plot epicurve of cases 
#
# Author: Daniel Gardiner
# Contact: daniel.gardiner@phe.gov.uk
# date: 13.10.2016
# credit: this function is loosely based on the plotIncidence function created 
#         as part of the GRINDER hackathon https://github.com/Hackout2/time
#
# Arguments: 
#
# x: a data frame
# date.col: a character specifying the column containing dates
# time.period: a character of the desired time period for the epicurve, this can either be 
#                  day, year, month, year.month, iso.year, iso.week, iso.year.week, use.date.col.as.is
#              NOTE: if time.period = use.date.col.as.is the date.col will be treated as a factor and will be used as the x-axis 
# fill.by: a character specifying the column to stratify by fill colour 
# split.by: a character specifying the column to stratify by faceting 
# shade.by: a character specifying the column to stratify by shade colour 
# start.at: a character in the format "yyyy-mm-dd" to specify the start date for the epicurve
# stop.at: a character in the format "yyyy-mm-dd" to specify the stop date for the epicurve
# xlab: a character specifying the x-axis label for the epicurve 
# ylab: a character specifying the y-axis label for the epicurve
# fill.by.legend.title: a character specifying the fill legend title 
# shade.by.legend.title: a character specifying the shade legend title 
# angle: a numeric to specify the x-axis label angel for the epicurve  
# col.pal: a numeric specifying the colour palette  (range 1-8 inclusive) OR a 
#          character stating 'phe' to use the phe colour palette  
# label.breaks: a numeric specifying the interval for x-axis label breaks 
# epi.squares: a logical  specifying if episquares should be used on the epicurve 
# na.rm: a logical  specifying if missing dates should be removed from the epicurve



epicurve = function(x, date.col, time.period, fill.by=NULL, split.by=NULL, shade.by=NULL,
                    start.at=NULL, stop.at=NULL, xlab=NULL, ylab="Incidence", width=1,
                    fill.by.legend.title = NULL, shade.by.legend.title = NULL,
                    angle=45, col.pal=1, label.breaks =0, epi.squares = TRUE, na.rm = FALSE) {
  
  library(ggplot2)
  library(ISOweek)
  library(scales)
  
  # handle arguments 
  
  if(!is.null(fill.by) && is.numeric(fill.by)) fill.by = names(x)[fill.by]
  
  if(!is.null(split.by) && is.numeric(split.by)) split.by = names(x)[split.by]
  
  if(!is.null(shade.by) && is.numeric(shade.by)) shade.by = names(x)[shade.by]
  
  if(!is.null(col.pal) && col.pal==0) col.pal = NULL # 0 will be the default palette
  
  if(!(col.pal == "phe" | (col.pal >= 0 & col.pal <= 8))) {
    
    col.pal = "phe"
    
    warning("col.pal must either be an integer from 1 to 8 or 'phe', 
            setting col.pal='phe'")
  }
  
  if(!(time.period %in% c("day", "year", "month", "quarter", "year.month", "year.quarter", "iso.year", "iso.week", "iso.year.week", "use.date.col.as.is"))){
    stop("time.period must be either: day, year, quarter, month, year.quarter, year.month, iso.year, iso.week, iso.year.week, use.date.col.as.is")
  }
  
  ##############################################################################
  # Define factor column (date.col.temp) to be used along the x-axis
  #
  # if time.period argument is use.date.col.as.is then use date.col along the x-axis
  # otherwise convert the date provided in date.col to the time period specified
  # in the time.period argument
  
  
  
  if(time.period == "use.date.col.as.is"){
    
    x$date.col.temp = x[, date.col]
    
  } else { 
    
    # load get.dates function 
    
    get.dates = function(x){  
      
      if(class(x) == "Date"){
        
        NULL
        
      } else {
        
        stop("x is not a date")
      }
      
      df = data.frame(day = as.character(x),
                      year = format(x, "%Y"), 
                      month = format(x, "%m"))
      
      df$year.month = paste0(df$year, df$month)
      
      df$iso.year = sapply(strsplit(ISOweek(x), "-W"), function(x) x[1])
      
      df$iso.week = sapply(strsplit(ISOweek(x), "-W"), function(x) x[2])
      
      df$iso.year.week = gsub("-W", "", ISOweek(x))
      
      df$quarter = NA
      
      df$quarter[!is.na(df$day)] = sprintf("%02d", ceiling(as.numeric(as.character(df$month[!is.na(df$day)]))/3))
      
      df$year.quarter = paste0(df$year, df$quarter)
      
      df  
    }
    
    # append the get.dates data.frame to the data.frame provided 
    
    x = data.frame(x, get.dates(x[, date.col]))
    
    
    # create a new factor column for the x-axis (levels of the factor contain
    # dates ranging from start.at to stop.at)
    
    start.at = as.Date(start.at) 
    
    stop.at = as.Date(stop.at)  
    
    all.dates = get.dates(seq(start.at, stop.at, 1))
    
    all.dates = unique(all.dates[, time.period]) 
    
    x$date.col.temp = factor(x[, time.period],
                             levels = all.dates)  
    
    
    # recode dates that fall outside of start.at/stop.at to NA
    
    x[!(as.character(x[, date.col]) %in% 
          as.character(get.dates(seq(start.at, stop.at, 1))$day)), "date.col.temp"] = NA
    
    ## REMOVE MISSING DATES ##
    
    cat(paste(sum(is.na(x$date.col.temp)), "rows have missing dates OR dates outside of the start/stop period"))
    
    if(na.rm) x = x[!is.na(x$date.col.temp), ]
    
    
    # order the levels of the date.col.temp column
    
    x$date.col.temp = factor(x$date.col.temp,
                             levels = sort(levels(x$date.col.temp)))
    
  }
  # we have now defined the factor column (date.col.temp) to be used along the x-axis
  ##############################################################################
  
  
  
  # order data for plotting 
  
  if(!is.null(fill.by) & !is.null(shade.by)){
    
    x = x[order(x[, fill.by], x[, shade.by]), ]
    
  } else if(!is.null(fill.by)){
    
    x = x[order(x[, fill.by]), ]
    
  } else if(!is.null(shade.by)){
    
    x = x[order(x[, shade.by]), ]
    
  } else{
    
    NULL
    
  }
  
  # create blocks column (this is to allow for epi squares to be added)
  
  x$blocks = 1:nrow(x)
  
  # generate plot 
  
  p = ggplot(x) 
  
  if(epi.squares){
    
    p = p + geom_bar(aes_string(x="date.col.temp", fill=fill.by, alpha=shade.by, group = "blocks"),
                     colour = "black", width = width)
    
  } else {
    
    p = p + geom_bar(aes_string(x="date.col.temp", fill=fill.by, alpha=shade.by),
                     colour = "black", width=width)
    
  } 
  
  p = p + labs(x = xlab, y = ylab)
  
  p = p + scale_x_discrete(breaks = levels(x$date.col.temp)[c(T, rep(F, label.breaks))],
                           drop=FALSE)
  
  p = p + scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1)*1.1)))),
                             expand = c(0,0))
  
  p = p + geom_hline(aes(yintercept = 0))
  
  p = p + theme(title = element_text(size = 11, colour = "black", face="bold"),
                axis.text.x = element_text(angle = angle, vjust = .5, size = 10,
                                           colour = "black"),
                axis.text.y = element_text(hjust = 1, size = 9,
                                           colour = "black"),
                legend.text= element_text(hjust = 1, size = 11,
                                          colour = "black", face="bold"),
                axis.title = element_text(size=16, face="bold"),
                strip.text.y = element_text(hjust = 1, size = 16,
                                            colour = "black", face="bold"),
                legend.position="right")
  
  p = p + labs(fill = fill.by.legend.title,
               alpha = shade.by.legend.title)
  
  
  
  if(!is.null(split.by)) p = p + facet_grid(paste(split.by, ".", sep = "~"),
                                            drop = FALSE)
  
  if(col.pal == "phe"){
    
    phe.cols = c("#822433", "#00B092", "#002776", "#EAAB00", "#8CB8C6", 
                 "#E9994A",  "#00A551", "#A4AEB5", "#00549F", "#DAD7CB")
    
    p = p + scale_fill_manual(values = phe.cols)
    
  } else if(!is.null(col.pal)){
    
    p = p + scale_fill_brewer(type = "qual", 
                              palette = col.pal, drop = FALSE)
    
  } else {
    
    NULL
    
  }
  
  p
} 


################################################################################
# # EXAMPLE
# 
# # generate dummy data 
# 
# set.seed(2)
# 
# data = data.frame(dates = sample(seq(as.Date('2014-01-01'), as.Date('2016-04-01'), by="day"), 200, replace = T),
#                   sex = c("Male", "Female"),
#                   conf = sample(c("Confirmed", "Probable", "Poisslbe"), 200, replace = T),
#                   status = sample(c("Student", "Staff"), 200, replace = T))
# 
# 
# data$dates.year.month = factor(format(data$dates, "%Y_%m"),
#                                levels = unique(format(seq(min(data$dates), max(data$dates), 1), "%Y_%m")))
# 
# 
# # use function
# 
# epicurve(data, date.col = "dates", time.period = "year.quarter", 
#          fill.by="sex", split.by=NULL, shade.by=NULL,
#          start.at = "2014-01-01", stop.at = "2015-06-22",
#          xlab="Year week", ylab="Count", 
#          fill.by.legend.title = "Sex", shade.by.legend.title = NULL, angle=90, 
#          col.pal="phe", label.breaks = 1, epi.squares = FALSE, na.rm = TRUE) 
# 
# epicurve(x = data, date.col = "dates.year.month", time.period = "use.date.col.as.is", 
#          fill.by="sex", split.by=NULL, shade.by=NULL,
#          start.at = "2014-01-01", stop.at = "2015-06-22",
#          xlab="Year week", ylab="Count", 
#          fill.by.legend.title = "Sex", shade.by.legend.title = NULL, angle=90, 
#          col.pal=7, label.breaks = 0, epi.squares = FALSE, na.rm = TRUE)
# 
# 
# epicurve(data, date.col = "dates", time.period = "year.quarter", 
#          fill.by="sex", split.by=NULL, shade.by=NULL,
#          start.at = "2014-01-01", stop.at = "2015-06-22",
#          xlab=NULL, ylab="Count", 
#          fill.by.legend.title = NULL, shade.by.legend.title = NULL, angle=90, 
#          col.pal=8, label.breaks = 0, epi.squares = FALSE, na.rm = TRUE)
# 
# epicurve(data, date.col = "dates", time.period = "year.month", 
#          fill.by="sex", split.by=NULL, shade.by=NULL,
#          start.at = "2014-01-01", stop.at = "2016-01-31",
#          xlab=NULL, ylab="Count", 
#          fill.by.legend.title = "Sex", shade.by.legend.title = NULL, angle=90, 
#          col.pal=2, label.breaks = 0, epi.squares = FALSE, na.rm = FALSE)
# 
# epicurve(data, date.col = "dates", time.period = "day", 
#          fill.by="sex", split.by=NULL, shade.by=NULL,
#          start.at = "2014-01-01", stop.at = "2014-02-19",
#          xlab=NULL, ylab="Count", fill.by.legend.title = NULL, angle=90, 
#          col.pal=4, label.breaks = 3, epi.squares = FALSE, na.rm = TRUE)
# 
# epicurve(data, date.col = "dates.year.month", time.period = "use.date.col.as.is", 
#          fill.by="sex", split.by=NULL, shade.by=NULL,
#          start.at = "2014-01-01", stop.at = "2014-02-19",
#          xlab=NULL, ylab="Count", fill.by.legend.title = NULL, angle=90, 
#          col.pal=4, label.breaks = 3, epi.squares = FALSE, na.rm = TRUE)
# 
# 
# epicurve(data, date.col = "dates", time.period = "month",
#          fill.by="sex", split.by=NULL, shade.by="conf",
#          start.at = "2014-01-01", stop.at = "2016-04-20",
#          xlab=NULL, ylab="Count", 
#          fill.by.legend.title = NULL, shade.by.legend.title = NULL, angle=0, 
#          col.pal=8, label.breaks = 0, epi.squares = FALSE, na.rm = TRUE)
# 
# 
# epicurve(data, date.col = "dates", time.period = "month",
#          fill.by="conf", split.by="conf", shade.by="sex",
#          start.at = "2014-01-01", stop.at = "2016-04-20",
#          xlab=NULL, ylab="Count", 
#          fill.by.legend.title = "Confirmed status", shade.by.legend.title = "Sex", 
#          angle=0, col.pal="phe", label.breaks = 0, epi.squares = TRUE, na.rm = TRUE)
# 
# epicurve(data, date.col = "dates", time.period = "month", 
#          fill.by="sex", split.by="conf", shade.by=NULL,
#          start.at = "2014-01-01", stop.at = "2016-04-20",
#          xlab=NULL, ylab="Count", 
#          fill.by.legend.title = NULL, shade.by.legend.title = NULL, angle=0, 
#          col.pal=7, label.breaks = 0, epi.squares = TRUE, na.rm = TRUE)
# 
# 
# 

