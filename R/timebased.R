#' Analyze each variable in respect to a time variable
#'
#' @param data_analyze a data frame to analyze
#' @param date_variable the variable that will be used to pivot all other variables
#' @param time_unit the time unit to use if not automatically
#' @param outdir an optional output directory to save the resulting plots as png images
#'
#' @examples
#' library(xray)
#' data(longley)
#' longley$Year=as.Date(paste0(longley$Year,'-01-01'))
#' timebased(longley, 'Year')
#'
#' @export
#' @import dplyr
#' @import ggplot2
#' @importFrom grDevices boxplot.stats
#' @importFrom utils head
#' @importFrom stats quantile
#' @importFrom stats setNames
#'
timebased <- function(data_analyze, date_variable, time_unit="auto", outdir) {

  # Obtain metadata for the dataset
  varMetadata = suppressWarnings(anomalies(data_analyze)$variables)

  # If it's remote, bring it home, and remove nulls
  data_analyze = filter(data_analyze, !is.na(!!date_variable)) %>%  collect()

  dateData = data_analyze[[date_variable]]

  if('POSIXct' %in% class(dateData) | 'POSIXlt' %in% class(dateData)){
    # Remove timezone
    attr(dateData, "tzone") <- "UTC"

  }else if(! 'Date' %in% class(dateData)){
    # Not a Date nor a POSIXct/POSIXlt, what are you giving me?

    if('character' %in% class(dateData) | 'factor' %in% class(dateData)){ #Try to convert strings
      dateData = as.Date(as.character(dateData))
    }else{
      warning('You need to specify a date variable as the second parameter')
      return()
    }
  }

  #Determine time unit
  if(time_unit == 'auto'){
    timeRange = as.double(difftime(max(dateData, na.rm=T), min(dateData, na.rm=T), units='secs'))
    min=60
    hour=min*60
    day=hour*24
    year=day*365
    time_unit = case_when(
      timeRange > year*2 ~ 'year',
      timeRange > day*35 ~ 'month',
      timeRange > hour*6 ~ 'hour',
      timeRange > min*10 ~ 'minute',
      TRUE ~ 'second'
    )
  }

  dateData=lubridate::floor_date(dateData, unit=time_unit)

  # Start rolling baby!
  i=0
  resVars = c()
  results = foreach::foreach(i=1:nrow(varMetadata)) %do% {
    var=varMetadata[i,]
    varName=as.character(var$Variable)
    if(var$pNA=='100%'){
      #All null
      warning(paste0("The variable ", varName, " is completely NA, can't plot that."))
      return()
    }else if(var$Variable == date_variable){
      #Do nothing when date var
      return()
    }else if(!var$type %in% c('Integer', 'Logical', 'Numeric', 'Factor', 'Character')){
      #Do not try to plot anything
      warning(paste0('Ignoring variable ', varName, ': Unsupported type for visualization'))
      return()
    }else{
      resVars=c(resVars,varName)

      if(var$type %in% c('Integer', 'Numeric')){
        # Box plot for visualizing difference in distribution among time

        varAnalyze = data.frame(dat=as.double(data_analyze[[varName]]), date=as.factor(dateData))

        ylim1 = boxplot.stats(varAnalyze$dat)$stats[c(1, 5)]

        ggplot(varAnalyze, aes(date, dat)) +
          geom_boxplot(fill='#ccccff', outlier.color = 'red', outlier.shape=1, na.rm=T) +
          theme_minimal() +
          labs(x = varName, y = "Rows") +
          coord_cartesian(ylim = ylim1*1.1) +
          ggtitle(paste0("Histogram of ", var$Variable)) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))

      }else{
        # 100% stacked barchart showing difference in categorical composition

        varAnalyze = data.frame(dat=as.character(data_analyze[[varName]]), date=dateData)
        topten = group_by(varAnalyze, dat) %>% count() %>% arrange(-n) %>% ungroup()
        if(nrow(topten) > 10){
          topten=head(topten, 10)
          warning(paste0("On variable ", varName, ", more than 10 distinct variables found, only using top 10 for visualization."))
        }

        others = anti_join(varAnalyze, topten, by='dat') %>%
          group_by(date) %>% count() %>% ungroup() %>%
          mutate(dat='Others') %>% select(date, dat, n)
        grouped = group_by(varAnalyze, date, dat) %>%
          semi_join(topten, by='dat') %>%
          count() %>% arrange(date, -n) %>% ungroup() %>%
          rbind(others)


        abbr = function (x) {return (abbreviate(x, minlength = 10))}


        ggplot(grouped, aes(x=date, y=n, fill=dat)) +
          geom_bar(position='fill', stat='identity') +
          scale_y_continuous(labels = scales::percent_format()) +
          scale_fill_brewer(palette='Paired', label=abbr) +
          theme_minimal() +
          labs(x = var$Variable, y = "Rows", fill=varName) +
          ggtitle(paste0("Evolution of variable ", varName)) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }
    }

  }

  results[sapply(results, is.null)] <- NULL
  batches = ceiling(length(results)/4)

  foreach::foreach(i=1:batches) %do% {
    firstPlot=((i-1)*4)+1
    lastPlot=min(firstPlot+3, length(results))
    if(lastPlot==firstPlot){
      plot(results[[firstPlot]])
    }else{
      grid::grid.newpage()
      grid::pushViewport(grid::viewport(layout = grid::grid.layout(2,2)))

      row=1
      col=1
      for (j in firstPlot:lastPlot) {
        print(results[[j]], vp = grid::viewport(layout.pos.row = row,
                                                layout.pos.col = col))
        if(row==2){
          row=1
          col=col+1
        }else{
          row=row+1
        }
      }
    }
  }


  if(!missing(outdir)){
    foreach::foreach(i=1:length(results)) %do% {
      ggsave(filename=paste0(outdir, '/', gsub('[^a-z0-9 ]','_', tolower(resVars[[i]])), '.png'), plot=results[[i]])
    }
  }

  return(paste0(length(results), " charts have been generated."))
}
