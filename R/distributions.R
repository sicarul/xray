#' Analyze each variable and generate a histogram describing it's distribution.
#'
#' Also returns a table of all numeric variables describind it's percentiles 1, 10, 25, 50 (median), 75, 90 and 99.
#'
#' @param data_analyze a data frame to analyze
#' @param outdir an optional output directory to save the resulting plots as png images
#' @param charts set this to false to avoid generating charts, useful for batch script usage
#'
#'
#' @examples
#' library(xray)
#' distributions(mtcars)
#'
#' @export
#' @import dplyr
#' @import ggplot2
#' @importFrom graphics plot
#' @importFrom stats dnorm
#' @importFrom stats sd
#' @importFrom utils setTxtProgressBar
#' @importFrom utils txtProgressBar
distributions <- function(data_analyze, outdir, charts=T) {


  # If it's remote, bring it home
  if(inherits(data_analyze, 'tbl_sql')){
    # Collect up to 100k samples
    print("Remote data source, collecting up to 100k sample rows")
    data_analyze = collect(data_analyze, n=100000)
  }

  # Obtain metadata for the dataset
  varMetadata = suppressWarnings(anomalies(data_analyze)$variables)


  if(charts==T){
    # Start rolling baby!
    i=0
    pb <- txtProgressBar(0, nrow(varMetadata)) # Progress bar
    resVars = c()
    results = foreach::foreach(i=seq_len(nrow(varMetadata))) %do% {
      var=varMetadata[i,]
      varName=as.character(var$Variable)
      setTxtProgressBar(pb, i)
      #Ignore unsupported types
      if(!var$type %in% c('Integer', 'Logical', 'Numeric', 'Factor', 'Character')){
        warning('Ignoring variable ', varName, ': Unsupported type for visualization.')
      }else{
        resVars=c(resVars,as.character(varName))

        if(var$type %in% c('Integer', 'Numeric')){

          varAnalyze = data.frame(dat=as.double(data_analyze[[varName]]))
          range=max(varAnalyze$dat,na.rm=T)-min(varAnalyze$dat,na.rm=T)

          # Histogram for numeric variables with at least 10 distinct values
          if(var$qDistinct > 10){
            bins = case_when(
              nrow(varAnalyze) > 1000 & var$qDistinct > 50 ~ 20,
              nrow(varAnalyze) > 5000 & var$qDistinct > 30 ~ 15,
              TRUE ~ 10
            )

            ggplot(varAnalyze, aes(dat)) +
              geom_histogram(aes(y=..density..), bins=bins,show.legend = FALSE, col='grey', fill='#5555ee') +
              scale_fill_discrete(h = c(180, 250), l=50) +
              stat_function(fun = dnorm,
                                     args = list(mean = mean(varAnalyze$dat, na.rm=TRUE), sd = sd(varAnalyze$dat, na.rm=TRUE)),
                                     col = 'red') +
              theme_minimal() +
              labs(x = varName, y = "Rows") +
              ggtitle(paste("Histogram of", varName))

          }else{
            # Plot a bar chart if less than or equal to 10 distinct values
            varAnalyze = data.frame(dat=as.character(data_analyze[[varName]]))
            ggplot(varAnalyze, aes(dat, fill=dat)) +
              geom_bar(show.legend = FALSE) +
              scale_fill_discrete(h = c(180, 250), l=50) +
              theme_minimal() +
              labs(x = varName, y = "Rows") +
              ggtitle(paste("Bar Chart of", varName)) +
              theme(axis.text.x = element_text(angle = 45, hjust = 1))
          }
        }else{
          # Plot a grouped bar chart for character values
          varAnalyze = data.frame(dat=as.character(data_analyze[[varName]]))
          topvars = group_by(varAnalyze, dat) %>% count() %>% arrange(-n)
          topten=topvars
          if(nrow(topvars) > 10){
            topten=head(topvars, 10)
            warning("On variable ", varName, ", more than 10 distinct variables found, only using top 10 for visualization.")
            others = anti_join(varAnalyze, topten, by='dat') %>%
              count() %>% mutate(dat='Others') %>% select(dat, n)
          }

          grouped = group_by(varAnalyze, dat) %>%
            semi_join(topten, by='dat') %>%
            count() %>% arrange(-n) %>% ungroup()

          if(nrow(topvars)>10){
            grouped = rbind(grouped, others)
          }

          ggplot(grouped, aes(x=dat, y=n, fill=dat)) +
            geom_bar(stat='identity', show.legend = FALSE) +
            coord_flip() +
            scale_fill_discrete(h = c(180, 250), l=50) +
            theme_minimal() +
            labs(x = varName, y = "Rows") +
            ggtitle(paste0("Bar Chart of ", varName)) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
        }
      }

    }
    close(pb)

    results[vapply(results, is.null, logical(1))] <- NULL
    batches = ceiling(length(results)/4)

    foreach::foreach(i=seq_len(batches)) %do% {
      firstPlot=((i-1)*4)+1
      lastPlot=min(firstPlot+3, length(results), na.rm=T)
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
      foreach::foreach(i=seq_along(results)) %do% {
        ggsave(filename=file.path(outdir, paste0(gsub('[^a-z0-9 ]','_', tolower(resVars[[i]])), '.png')), plot=results[[i]])
      }
    }
  }


  distTable=foreach::foreach(i=seq_len(nrow(varMetadata)), .combine=rbind) %do% {
    var=varMetadata[i,]
    varName=as.character(var$Variable)
    if(var$type %in% c('Integer', 'Numeric')){
      data.frame(
        cbind(varName,
              t(round(quantile(data_analyze[[varName]], probs=c(.01, .1, .25, .5, .75, .9, .99), na.rm=T), 4))
      ))
    }
  }
  distTable=setNames(distTable, c('Variable', 'p_1', 'p_10', 'p_25', 'p_50', 'p_75', 'p_90', 'p_99'))

  distTable

}
