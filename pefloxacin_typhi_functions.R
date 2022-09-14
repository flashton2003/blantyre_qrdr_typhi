

read_all_typhi <- function(line_list_handle){
  all_typhi <- read_excel(line_list_handle, col_types = c("text", "text", "text", "text",
                                                          "text", "date", "text", "text", "numeric", 
                                                          "text", "text", "text", "text", "text", 
                                                          "text", "text", "text", "numeric", 
                                                          "text", "text", "text", "text", "text", 
                                                          "text", "text", "text", "text", "text", "text"))
  
  all_typhi$Month <- as.Date(cut(all_typhi$`Properly formatted date`, breaks = "month"))
  # Note starts with "Ignore" if there is some reason the sample should be ignored
  all_typhi <- all_typhi %>% mutate(any_qrdr = ifelse(is.na(QRDR), NA, "QRDR")) %>% filter(!grepl("^Ignore", Note))
  return(all_typhi)
}


read_strataa_prescriptions <- function(strataa_prescriptions_handle){
  # ps12_dov is the date of visit
  strataa_prescriptions <- read_csv(strataa_prescriptions_handle, col_types = cols(ps12_dov = col_datetime(format = "%d/%m/%Y %H:%M")))
  strataa_prescriptions$Month <- as.Date(cut(strataa_prescriptions$ps12_dov, breaks = "month"))
  # ps44_cipro is 1 if cipro was prescribed
  strataa_prescriptions <- strataa_prescriptions %>% filter(ps44_cipro == 1)
  return(strataa_prescriptions)
}


combine_by_month <- function(tyvac_by_month, strataa_by_month, cases_start, cases_end, input_type){
  # get the official start date of tyvac, as this will determine the start period
  combined_by_month <- data.frame(Month = seq(cases_start, cases_end, "months")) 
  #combined_by_month <- combined_by_month %>% rename(V1 = Month)
  combined_by_month <- left_join(combined_by_month, tyvac_by_month, by = 'Month')
  combined_by_month <- left_join(combined_by_month, strataa_by_month, by = 'Month')
  combined_by_month[is.na(combined_by_month)] <- 0
  if (input_type == 'qrdr'){
    combined_by_month$total_qrdr_n <- combined_by_month$strataa_qrdr_n + combined_by_month$tyvac_qrdr_n
    combined_by_month$total_qrdr_incidence_per_100000 <- (combined_by_month$total_qrdr_n / strataa_pop) * 100000
  }
  else if (input_type == 'cases'){
    combined_by_month$total_typhoid_n <- combined_by_month$tyvac_n + combined_by_month$strataa_n
    combined_by_month$total_typhoid_per100000 <- (combined_by_month$total_typhoid_n / strataa_pop) * 100000
  }
  else if(input_type == 'prescrip'){
    combined_by_month$total_prescrip_n <- combined_by_month$tyvac_eligible_ciprofloxacin_prescribed + combined_by_month$strataa_prescrip_n
    combined_by_month$total_prescrip_per_100000 <- (combined_by_month$total_prescrip_n / strataa_pop) * 100000
  }
  else if(input_type == 'blood_cultures'){
    combined_by_month$total_bc_n <- combined_by_month$tyvac_bc_n + combined_by_month$strataa_blood_cultures_n
    combined_by_month$total_bc_per_100000 <- (combined_by_month$total_bc_n / strataa_pop) * 100000
  }
  else{
    stop('ERROR - input_type argument needs to be one of (qrdr, cases, prescrip, blood_cultures)')
  }
  
  return(combined_by_month)
}

combine_cases_prescrip_qrdr_bc <- function(combined_qrdr_cases_by_month, combined_cases_by_month, combined_prescrip_by_month, combined_blood_cultures_by_month, cases_start, cases_end){
  qrdr_per_month <- select(combined_qrdr_cases_by_month, c('Month', 'total_qrdr_n'))
  cases_per_month <- select(combined_cases_by_month, c('Month', 'total_typhoid_n'))
  prescriptions_per_month <- select(combined_prescrip_by_month, c('Month', 'total_prescrip_n'))
  bc_per_month <- select(combined_blood_cultures_by_month, c('Month', 'total_bc_n'))
  
  entire_recruitment_period <- data.frame(Month = seq(cases_start, cases_end, "months"))
  entire_recruitment_period <- left_join(entire_recruitment_period, cases_per_month, by = 'Month')
  entire_recruitment_period <- left_join(entire_recruitment_period, prescriptions_per_month, by = 'Month')
  entire_recruitment_period <- left_join(entire_recruitment_period, qrdr_per_month, by = 'Month')
  entire_recruitment_period <- left_join(entire_recruitment_period, bc_per_month, by = 'Month')
  return(entire_recruitment_period)
}







