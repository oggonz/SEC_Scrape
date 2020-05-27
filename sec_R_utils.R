LOGFILE = format(Sys.time(), "%b_%d_%Y.log")
print(LOGFILE)

CSVFILE = format(Sys.time(), "%b_%d_%Y.csv")
print(CSVFILE)

get_filings_links <-function(str_ticker) {
  df_filings <- company_filings(str_ticker, type = "10-", count = 20)
  df_filings <- df_filings[df_filings$type == "10-K" | df_filings$type == "10-Q", ]
  df_filing_infos <- map_df(df_filings$href, filing_information)
  df_filings <- bind_cols(df_filings, df_filing_infos)
  return(head(as_tibble(df_filings),20))
}

write_log <- function(str_text) {
  print(str_text)
  if (file.exists(LOGFILE)) {
    write(str_text,file=LOGFILE,append=TRUE)
  } else {
    write(str_text,file=LOGFILE,append=FALSE)
  }
  
}

write_log_csv <- function(df) {
  if (file.exists(CSVFILE)) {
    write_csv(df,CSVFILE,append=TRUE)
  } else {
    write_csv(df,CSVFILE,append=FALSE)
  }
  
}

get_mdna_text <- function(str_href) {
  write_log("next link:")
  write_log(str_href)
  
  #make this a func
  str_file_path <- ''
  file_path = strsplit(str_href,'/')
  for (i in 5:length(file_path[[1]])-1) {
    str_file_path = paste0(str_file_path,"/",(file_path[[1]][i]))
  }
  str_file_path <- paste0(getwd(),"/",str_file_path)
  dir.create(str_file_path,recursive = TRUE)
  str_file_path
  str_file_name <- ''
  file_path = strsplit(str_href,'/')
  for (i in 4:length(file_path[[1]])) {
    str_file_name = paste0(str_file_name,"/",(file_path[[1]][i]))
  }
  str_file_name <- paste0(getwd(),str_file_name)
  str_file_name <- gsub(".htm",".csv",str_file_name)
  
  str_section = 'item 2|item 7'
  str_search = 'discussion'
  
  if (file.exists(str_file_name)) {  #add force equals true
    write_log("filing documents from cache ...")
    
    df_filing_documents <- read_csv(str_file_name,col_types = cols()) 
    df_filing_documents <- df_filing_documents %>% mutate_if(is.logical, as.character)
  } else {
    write_log("filing documents from sec ...")
    
    df_filing_documents <- filing_documents(str_href) %>%
      filter(!grepl('.pdf',href)) %>%
      write_csv(str_file_name)
  }
  
  str_doc_href <- df_filing_documents[df_filing_documents$type == "10-K" | df_filing_documents$type == "10-Q",]$href
  
  print(df_filing_documents[df_filing_documents$type == "10-K" | df_filing_documents$type == "10-Q",])  
  
  file_end <- gsub("https://www.sec.gov",'',str_doc_href)
  
  file_name = paste0(getwd(),file_end)
  
  #use cache if possible
  if (file.exists(file_name)) {
    
    doc <- read_csv(file_name,col_types = cols(.default = "c"))
    print("local cache")
    
  } else {
    
    doc <- parse_filing(str_doc_href)    
    
    str_file_path <- ''
    file_path = strsplit(file_name,'/')
    for (i in 3:length(file_path[[1]])-1) {
      str_file_path = paste0(str_file_path,"/",(file_path[[1]][i]))
    }
    str_file_path <- paste0(str_file_path,"/")
    dir.create(str_file_path,recursive = TRUE)
    write_csv(as_tibble(doc),file_name)
    
  }
  
  df_txt <- doc[grepl(str_section, doc$item.name, ignore.case = TRUE) & grepl(str_search, doc$item.name, ignore.case = TRUE), ] # only discussion for now
  #if default search fails, use a dictionary attempt
  if (nrow(df_txt) == 0) {
    write_log('going to backup')
    #paired vector of start and ending text to slice if found
    #going forward use tickers as an additional column
    #and port this to a csv file as part of the install.
    df_filter_list <- data.frame(
      start_text = c('Introduction',
                     'FUNCTIONAL EARNINGS', 
                     'DISCUSSION AND ANALYSIS',
                     'DISCUSSION AND ANALYSIS',
                     'DISCUSSION AND ANALYSIS',
                     'OVERVIEW',
                     'Business Overview',
                     'Financial Review',
                     'RESULTS OF OPERATIONS',
                     'Overview',
                     'Entergy operates',
                     "MANAGEMENT\'S FINANCIAL DISCUSSION",
                     'General',
                     "Management's Discussion",
                     'EXECUTIVE SUMMARY',
                     'EXECUTIVE OVERVIEW',
                     'EXECUTIVE OVERVIEW',
                     'The following management discussion and analysis',
                     'CURRENT ECONOMIC CONDITIONS',
                     'Overview and Highlights',
                     'Financial Review - Results of Operations'),
      end_text = c('Quantitative and qualitative disclosures about market risk',
                   "MANAGEMENT\'S REPORT",
                   'RISK FACTORS',
                   'FIVE-YEAR PERFORMANCE GRAPH',
                   'FINANCIAL STATEMENTS AND NOTES',
                   'Risk management includes the identification',
                   'Selected Loan Maturity Data',
                   'Risk Management',
                   'QUANTITATIVE AND QUALITATIVE DISCLOSURES',
                   'Forward-Looking Statements',
                   'New Accounting Pronouncements',
                   'New Accounting Pronouncements',
                   'Website information',
                   'Risk Disclosures',
                   'RISK FACTORS',
                   'A summary of contractual obligations is included',
                   'CONSOLIDATED RESULTS OF OPERATIONS',
                   'NON-GAAP FINANCIAL MEASURES',
                   'FORWARD-LOOKING STATEMENTS',
                   'Critical Accounting Policies and Estimates',
                   'Unregistered Sales of Equity Securities and Use of Proceeds')
    )
    
    #this would be case sensitive
    for (row in 1:nrow(df_filter_list)) { #should flip this to apply()
      
      start_text <- df_filter_list[row, "start_text"]
      end_text <- df_filter_list[row, "end_text"]
      
      write_log(paste0('trying ',start_text))
      write_log(paste0('to ',end_text))
      
      i_start = as.integer(which(grepl(start_text, doc$text))) 
      if (length(i_start) > 1) { #handle table of contents duplicates
        i_start = i_start[2]
      }
      i_end = as.integer(which(grepl(end_text, doc$text)))
      if (length(i_end) > 1) {
        i_end = i_end[2]
      }
      
      write_log(i_start)
      write_log(i_end)
      
      if (length(i_start) != 0 & length(i_end) != 0) {
        #i_start = as.numeric(i_start)
        #i_end = as.numeric(i_end)
        if (i_start < i_end) {        
          print(paste0('istart is:',i_start,' iend is:',i_end))
          df_txt = doc[i_start:i_end,]
          break
        }
      }
      
    }
    if (length(i_start) == 0 || length(i_end) == 0) {
      write_log("missing section for:")
      write_log(str_href)
    }
    
  }
  #we could do some text preprocessing here.
  
  df_txt <- as_tibble(df_txt) %>%
    #mutate(text = textclean::strip(text)) %>%
    mutate(section = str_search)
  
  return(df_txt)
}

get_document_text <- function(str_ticker, force = FALSE) { #not using force yet
  start_time <- Sys.time()
  
  write_log(str_ticker)
  
  str_write_name <- paste0('sec_data_folder/',str_ticker)
  
  write_log("get filings links ...")
  
  filings_csv <- paste0(str_write_name,"_filings.csv")
  
  if (file.exists(filings_csv)) {  
    write_log("from cache ...")
    
    df_filings <- read_csv(filings_csv,col_types = cols()) 
    df_filings <- df_filings %>% mutate_if(is.logical, as.character)
  } else {
    write_log("from sec ...")
    
    df_filings <- get_filings_links(str_ticker) %>%
      mutate(ticker = str_ticker) %>%
      write_csv(filings_csv)
  }
  
  write_log_csv(df_filings)
  
  #for debug
  i_test = nrow(df_filings) #for some reason this won't evaulate inside the if statement
  if (i_test == 0) {
    return(NULL)
  }
  
  write_log("get section text ...")
  
  df_data <- (df_filings) %>%
    rowwise() %>%
    mutate(nest_discussion = map(.x = href, .f = get_mdna_text)) %>%
    ungroup() %>%
    group_by(period_date) %>%
    arrange(desc(period_date))
  
  #jenky - find a rowwise application
  a <- df_data %>%
    select(period_date,filing_date,type,form_name,documents,nest_discussion) %>%
    unnest(nest_discussion)
  
  write_log("write to local csv  ...")
  df_data <- a %>%
    as_tibble() %>%
    write_csv(paste0(str_write_name,".csv"))
  
  end_time <- Sys.time()
  
  write_log(end_time - start_time)
  
  return(df_data)
}
