

line_list_handle <- "~/Dropbox/GordonGroup/STRATAA_pefloxacin_resistant_typhi/data_munging/2022.09.08/2022.09.08 line list.xlsx"

get_typhi_data <- function(line_list_handle){
  all_typhi <- read_excel(line_list_handle, col_types = c("text", "text", "text", "text",
                                                          "text", "date", "text", "text", "numeric", 
                                                          "text", "text", "text", "text", "text", 
                                                          "text", "text", "text", "numeric", 
                                                          "text", "text", "text", "text", "text", 
                                                          "text", "text", "text", "text", "text", "text"))
  # current line list includes one sample that isn't a valid id
  all_typhi <- all_typhi %>% filter(!grepl("^Ignore - mixup", Note))
  all_typhi$`Properly formatted date`<- as.Date(all_typhi$`Properly formatted date`)
  all_typhi$Month <- as.Date(cut(all_typhi$`Properly formatted date`, breaks = "month"))
  
  # Note starts with "Ignore" if there is some reason the sample should be ignored
  one_typhi_per_patient <- all_typhi %>% mutate(any_qrdr = ifelse(is.na(QRDR), NA, "QRDR")) %>% filter(!grepl("^Ignore - duplicate", Note))
  typhis <- list("all_typhi" = all_typhi, "one_typhi_per_patient" = one_typhi_per_patient)
  return(typhis)
}

get_strataa_tyvac_cases <- function(df, cases_start, cases_end){
  df %>% filter(grepl('STRATAA', Study) | grepl('TyVAC', Study)) %>% filter(Organism == 'Salmonella Typhi') %>% filter(`Properly formatted date` >= cases_start & `Properly formatted date` <= cases_end)
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
    combined_by_month$total_seq_typhoid_n <- combined_by_month$tyvac_n + combined_by_month$strataa_n
    combined_by_month$total_typhoid_per100000 <- (combined_by_month$total_seq_typhoid_n / strataa_pop) * 100000
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

combine_cases_prescrip_qrdr_bc_mc <- function(combined_qrdr_cases_by_month, sequenced_cases_by_month, sequenced_isolates_by_month, combined_prescrip_by_month, combined_blood_cultures_by_month, micro_confirmed_opp_by_month, micro_confirmed_all_by_month, cases_start, cases_end){
  qrdr_per_month <- select(combined_qrdr_cases_by_month, c('Month', 'total_qrdr_n'))
  cases_per_month <- select(sequenced_cases_by_month, c('Month', 'total_seq_typhoid_n'))
  sequenced_isolates_by_month <- select(sequenced_isolates_by_month, c('Month', 'total_seq_typhi_n'))
  prescriptions_per_month <- select(combined_prescrip_by_month, c('Month', 'total_prescrip_n'))
  bc_per_month <- select(combined_blood_cultures_by_month, c('Month', 'total_bc_n'))
  mc_opp_per_month <- select(micro_confirmed_opp_by_month, c('Month', 'micro_confirmed_opp_n'))
  mc_all_per_month <- select(micro_confirmed_all_by_month, c('Month', 'micro_confirmed_all_n'))
  
  entire_recruitment_period <- data.frame(Month = seq(cases_start, cases_end, "months"))
  entire_recruitment_period <- left_join(entire_recruitment_period, cases_per_month, by = 'Month')
  entire_recruitment_period <- left_join(entire_recruitment_period, sequenced_isolates_by_month, by = 'Month')
  entire_recruitment_period <- left_join(entire_recruitment_period, prescriptions_per_month, by = 'Month')
  entire_recruitment_period <- left_join(entire_recruitment_period, qrdr_per_month, by = 'Month')
  entire_recruitment_period <- left_join(entire_recruitment_period, bc_per_month, by = 'Month')
  entire_recruitment_period <- left_join(entire_recruitment_period, mc_opp_per_month, by = 'Month')
  entire_recruitment_period <- left_join(entire_recruitment_period, mc_all_per_month, by = 'Month')
  
  return(entire_recruitment_period)
}



plot_typh_presc_qrdr_bc_per100000 <- function(input_data) {
  typhoid_graph <- ggplot(input_data, aes(x = Month, y = total_typhoid_per100000)) + 
    geom_bar(stat = "identity") + 
    ylab('Sequencing confirmed typhoid\nper 100,000 people')
  
  cipro_prescrip_graph <- ggplot(input_data, aes(x = Month, y = total_prescrip_per_100000)) + 
    geom_bar(stat = "identity") + 
    ylab('Ciprofloxacin prescriptions\nper 100,000 people')
  
  qrdr_graph <- ggplot(input_data, aes(x = Month, y = total_qrdr_incidence_per_100000)) + 
    geom_bar(stat = "identity") +
    ylab('S. Typhi with QRDR mutations\nper 100,000 people')
  
  bc_graph <- ggplot(input_data, aes(x = Month, y = total_bc_per_100000)) + 
    geom_bar(stat = "identity") +
    ylab('Number of blood cultures\nper 100,000 people')
  
  p <- typhoid_graph / cipro_prescrip_graph / qrdr_graph / bc_graph + plot_layout(heights = c(5,5,5,5))
  
  return(p)
}

plot_typh_presc_qrdr_bc_n <- function(input_data, strataa_tyvac_qrdr_typhi) {
  
  strataa_tyvac_qrdr_typhi_by_mutation_by_month <- strataa_tyvac_qrdr_typhi %>% group_by(QRDR, Month) %>% summarise(n = n())
  
  typhoid_graph <- ggplot(input_data, aes(x = Month, y = total_seq_typhoid_n)) + 
    geom_bar(stat = "identity") + 
    ylab('# Sequencing confirmed typhoid') +
    ggtitle('B)')
  
  cipro_prescrip_graph <- ggplot(input_data, aes(x = Month, y = total_prescrip_n)) + 
    geom_bar(stat = "identity") + 
    geom_point(aes(x = Month, y = (total_qrdr_n / total_seq_typhoid_n) * 125), colour="red") +
    ylab('# Ciprofloxacin prescriptions') +
    scale_y_continuous("# Ciprofloxacin prescriptions", sec.axis = sec_axis(~ . / 125, name = "Prop. Typhi w/QRDR mutation", breaks = seq(0, 1, by = 0.2))) +
    ggtitle('D)')
    
  
  qrdr_graph <- ggplot(strataa_tyvac_qrdr_typhi_by_mutation_by_month, aes(x = Month, y = n, fill = QRDR)) + 
    geom_bar(stat = 'identity') +
    ylab('# S. Typhi with QRDR mutations') +
    ggtitle('C)')
  
  # not using this one anymore, combined with the cipro_prescrip_graph
  qrdr_proportion_typhoid_graph <-ggplot(input_data, aes(x = Month, y = total_qrdr_n / total_seq_typhoid_n)) +
    geom_point(colour="firebrick", shape=4) +
    scale_y_continuous(labels = scales::percent) +
    ylab('% of Typhi that were QRDR')
  
  bc_graph <- ggplot(input_data, aes(x = Month, y = total_bc_n)) + 
    geom_bar(stat = "identity") +
    ylab('# blood cultures') +
    ggtitle('A)')
  
  p <- bc_graph / typhoid_graph  / cipro_prescrip_graph / qrdr_graph  + plot_layout(heights = c(5,5,5,5,5))
  
  return(p)
  
  
}




