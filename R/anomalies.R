#' Analyze a dataset and search for anomalies
#'
#' If any anomalous columns are found, they are reported as a warning and returned in a data.frame. To interpret the output, we are getting these anomalies:
#' \itemize{
#' \item NA values: NA
#' \item 0 values: Zero
#' \item Blank strings: Blank
#' \item Infinite numbers: Inf
#' }
#'
#' All of these value are reported in columns prefixed by q (quantity), indicating the rows with the anomaly, and p (percentage), indicating percent of total rows with the anomaly.
#'
#' And, also any columns with only one distinct value, which means the column doesn't bring information to the table (If all rows are equal, why bother having that column?). We report the number of distinct values in qDistinct.
#'
#' @param data_analyze a data frame or tibble to analyze
#' @param anomaly_threshold the minimum percentage of anomalous rows for the column to be problematic
#' @param distinct_threshold the minimum amount of distinct values the column has to have to not be problematic, usually you want to keep this at it's default value.
#'
#' @examples
#'
#' library(xray)
#' anomalies(mtcars, anomaly_threshold=0.5)
#'
#' @export
#' @import dplyr
anomalies <- function(data_analyze,
                      anomaly_threshold = 0.8,
                      distinct_threshold = 2) {
  if(anomaly_threshold < 0 || anomaly_threshold > 1){
    warning("anomaly_threshold can be between 0 and 1, which mean 0% and 100%.")
    return()
  }

  varNames = names(data_analyze)



  # Check for anomalies by column and summarize them
  if(inherits(data_analyze, 'tbl_sql')){
    # Special treatment for SQL sources so it works even when it's a dplyr connected dataset (created with tbl(...))
    analyze = data_analyze %>%
      mutate_all(funs(as.character)) %>%
      mutate_all(funs(
        case_when(
          is.na(.) ~ 'NA',
          . == '0' ~ 'Zero',
          . == '' ~ 'Blank',
          TRUE ~ 'Value'
        )
      ))%>%
      summarize_all(funs(
        qNA=sum(.=='NA'),
        qZero=sum(.=='Zero'),
        qBlank=sum(.=='Blank'),
        qValues=sum(.=='Value'),
        qInf=sum(.=='Infinite'),
        q=n()
      )) %>% collect()
  }else{
    # Local Data Frame or unknown type of dataset, let's hope it works
    analyze = data_analyze %>%
      mutate_all(funs(
        case_when(
          is.na(.) ~ 'NA',
          . == Inf | . == -Inf ~ 'Infinite',
          . == 0 ~ 'Zero',
          as.character(.) == '' ~ 'Blank',
          TRUE ~ 'Value'
        )
      ))%>%
      summarize_all(funs(
        qNA=sum(.=='NA'),
        qZero=sum(.=='Zero'),
        qBlank=sum(.=='Blank'),
        qValues=sum(.=='Value'),
        qInf=sum(.=='Infinite'),
        q=n()
      )) %>% collect()
  }

  # Distinct amount of values inside each column
  analyzeDistinct = data_analyze %>%
    summarize_all(funs(qDistinct=n_distinct(.))) %>%
    collect()


  # Generate summary
  analyzeOut=data.frame()
  for(var in varNames){
    outRow = data.frame(Variable=var)


    if(ncol(data_analyze)==1){
      prefix=''
    }else{
      prefix=paste0(var,'_')
    }

    q=analyze[[paste0(prefix,'q')]]
    outRow$q=q

    outRow$qNA=analyze[[paste0(prefix,'qNA')]]
    outRow$pNA=as.double(outRow$qNA/q)

    outRow$qZero=analyze[[paste0(prefix,'qZero')]]
    outRow$pZero=as.double(outRow$qZero/q)

    outRow$qBlank=analyze[[paste0(prefix,'qBlank')]]
    outRow$pBlank=as.double(outRow$qBlank/q)

    outRow$qInf=analyze[[paste0(prefix,'qInf')]]
    outRow$pInf=as.double(outRow$qInf/q)

    outRow$qDistinct=analyzeDistinct[[paste0(prefix,'qDistinct')]]


    if(ncol(analyzeOut)==0){
      analyzeOut=outRow
    }else{
      analyzeOut=rbind(analyzeOut,outRow)
    }
  }
  descriptions = getColumnDescriptions(data_analyze)
  analyzeOut$type=descriptions
  # Calculate percent anomalous
  finalReport = analyzeOut %>%
    mutate(anomalous_percent=pNA+pZero+pBlank+pInf) %>%
    arrange(-anomalous_percent, qDistinct)

  # Detect problematic variables
  problem_vars = filter(finalReport, anomalous_percent > anomaly_threshold | qDistinct < distinct_threshold) %>%
    mutate(problems=trimws(paste0(
      ifelse(anomalous_percent > anomaly_threshold, paste0('Anomalies present in ', xpercent(anomalous_percent), ' of the rows. '),''),
      ifelse(qDistinct < distinct_threshold, paste0('Less than ', distinct_threshold, ' distinct values. '),'')
    ))) %>%
    mutate_at(c('pNA', 'pZero','pBlank','pInf', 'anomalous_percent')
              , xpercent)

  finalReport = mutate_at(finalReport,
                          c('pNA', 'pZero','pBlank','pInf', 'anomalous_percent')
                          , xpercent)

  if(nrow(problem_vars) > 0){
    warning("Found ", nrow(problem_vars), ' possible problematic variables: \n', paste0(problem_vars$Variable, collapse=', '))
  }

  # Return the result
  return(list(
    variables=finalReport,
    problem_variables=problem_vars
  ))
}




colToDescription <- function(col) {
  col_class = class(col)[[1]]
  switch(col_class,
         logical = 'Logical',
         numeric = 'Numeric',
         integer = 'Integer',
         Date = 'Date',
         POSIXct = 'Timestamp',
         POSIXlt = 'Timestamp',
         factor = "Factor",
         character = "Character",
         "Unknown"
  )
}

getColumnDescriptions <- function(df) {
  return(
    vapply(
      df,
      FUN = colToDescription,
      character(1)
    )
  )
}

xpercent = function(x){
  ifelse(x==0, '-',paste0(round(x*100,2), '%'))
}
