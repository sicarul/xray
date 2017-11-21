#' Analyze a dataset and search for anomalies
#'
#' If any anomalous columns are found, they are reported as a warning and returned in a data.frame. The anomalies we try to detect are high percentages of:
#' \itemize{
#' \item NA values
#' \item 0 values
#' \item Blank strings
#' \item Infinite numbers
#' }
#' And, also any columns with only one distinct value, which means the column doesn't bring information to the table (If all rows are equal, why bother having that column?)
#'
#' @param data_analyze a data frame or tibble to analyze
#' @param anomaly_threshold the minimum percentage of anomalous rows for the column to be problematic
#' @param distinct_threshold the minimum amount of distinct values the column has to have to not be problematic, usually you want to keep this at it's default value.
#'
#' @examples
#'
#' xray::anomalies(mtcars, anomaly_threshold=0.5)
#'
#' @export
#' @import dplyr
anomalies <- function(data_analyze,
                      anomaly_threshold = 0.8,
                      distinct_threshold = 2) {
  if(anomaly_threshold < 0 | anomaly_threshold > 1){
    warning("anomaly_threshold can be between 0 and 1, which mean 0% and 100%.")
    return()
  }

  varNames = names(data_analyze)



  # Check for anomalies by column and summarize them
  if('tbl_sql' %in% class(data_analyze)){
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
        qNA=sum(ifelse(.=='NA', 1,0)),
        qZero=sum(ifelse(.=='Zero', 1,0)),
        qBlank=sum(ifelse(.=='Blank', 1,0)),
        qValues=sum(ifelse(.=='Value', 1,0)),
        qInf=sum(ifelse(.=='Infinite', 1,0)),
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
        qNA=sum(ifelse(.=='NA', 1,0)),
        qZero=sum(ifelse(.=='Zero', 1,0)),
        qBlank=sum(ifelse(.=='Blank', 1,0)),
        qValues=sum(ifelse(.=='Value', 1,0)),
        qInf=sum(ifelse(.=='Infinite', 1,0)),
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

    q=analyze[[paste0(var,'_q')]]
    outRow$q=q

    outRow$qNA=analyze[[paste0(var,'_qNA')]]
    outRow$pNA=outRow$qNA/q

    outRow$qZero=analyze[[paste0(var,'_qZero')]]
    outRow$pZero=outRow$qZero/q

    outRow$qBlank=analyze[[paste0(var,'_qBlank')]]
    outRow$pBlank=outRow$qBlank/q

    outRow$qInf=analyze[[paste0(var,'_qInf')]]
    outRow$pInf=outRow$qInf/q

    outRow$qDistinct=analyzeDistinct[[paste0(var,'_qDistinct')]]


    if(ncol(analyzeOut)==0){
      analyzeOut=outRow
    }else{
      analyzeOut=rbind(analyzeOut,outRow)
    }
  }
  descriptions = getColumnDescriptions(data_analyze)
  analyzeOut$type=descriptions
  # Calculate percent anomalous
  finalReport = analyzeOut %>% mutate(
    anomalous_percent=pNA+pZero+pBlank+pInf
  ) %>% arrange(-anomalous_percent, qDistinct)

  # Detect problematic variables
  problem_vars = filter(finalReport, anomalous_percent > anomaly_threshold | qDistinct < distinct_threshold) %>%
    mutate(problems=trimws(paste(
      ifelse(anomalous_percent > anomaly_threshold, paste0('Anomalies present in ', scales::percent(anomalous_percent), ' of the rows. '),''),
      ifelse(qDistinct < distinct_threshold, paste0('Less than ', distinct_threshold, ' distinct values. '),''),
      sep=''
    )))

  if(nrow(problem_vars) > 0){
    warning(paste0("Found ", nrow(problem_vars), ' possible problematic variables: \n', paste0(problem_vars$Variable, collapse=', ')))
  }

  # Return the result
  return(list(
    variables=finalReport,
    problem_variables=problem_vars
  ))
}




colToDescription <- function(col) {
  class = class(col)[[1]]
  switch(class,
         logical = {
           return('Logical')
         },
         numeric = {
           return('Numeric')
         },
         integer = {
           return('Integer')
         },
         Date = {
           return('Date')
         },
         POSIXct = {
           return('Timestamp')
         },
         POSIXlt = {
           return('Timestamp')
         },
         factor = {
           return("Factor")
         },
         character = {
           return("Character")
         }
  )
  return("Unknown")
}

getColumnDescriptions <- function(df) {
  return(
    sapply(
      df,
      FUN = colToDescription
    )
  )
}
