


get_ist_demo <- function(){
  
  ist_demo_ids <- jsonlite::fromJSON("https://api.ibb.gov.tr/ibb-istatistik-api/istatistik/address?search=&_=1645114660853")
  ist_ilce_demo_ids <- ist_demo_ids %>% filter(type == 1)
  ist_ilce_demo_list <- list()
  
  for( i in ist_ilce_demo_ids$id){
    
    ibb_item <- jsonlite::fromJSON(paste0("https://api.ibb.gov.tr/ibb-istatistik-api/istatistik/searchIstatistik?ilce=",i,"&_=1645114436383"))
    ist_ilce_demo_list <- append(ist_ilce_demo_list,list(ibb_item))
    
  }
  
  
  ist_ilce_demo_df <- data.table::rbindlist(ist_ilce_demo_list)
  ist_ilce_demo_df <- ist_ilce_demo_df %>% select(ilceAd,ikiBin19Nufus,universiteOran,onbesOndokuzYasErkek:otuzBesOtuzDokuzYasErkek,onbesOndokuzYasKadin:otuzBesOtuzDokuzYasKadin)
  
  
  ist_ilce_demo_df <- ist_ilce_demo_df %>% mutate(youth_female = onbesOndokuzYasKadin+yirmiYirmiDortYasKadin+yirmiBesYirmiDokuzYasKadin+otuzOtuzDortYasKadin+otuzBesOtuzDokuzYasKadin,
                                                  youth_male = onbesOndokuzYasErkek+yirmiYirmiDortYasErkek+yirmiBesYirmiDokuzYasErkek+otuzOtuzDortYasErkek+otuzBesOtuzDokuzYasErkek,
                                                  youth_male_share = youth_male/ikiBin19Nufus,
                                                  youth_female_share = youth_female/ikiBin19Nufus) %>% select(-c(onbesOndokuzYasErkek:otuzBesOtuzDokuzYasErkek,onbesOndokuzYasKadin:otuzBesOtuzDokuzYasKadin))
  
  ist_ilce_demo_df$no_tr <- ist_ilce_demo_df$ilceAd %>% stringi::stri_trans_general("latin-ascii")
  ist_ilce_demo_df$no_tr <- ist_ilce_demo_df$no_tr %>% tolower()  
  
  return(ist_ilce_demo_df)
  
}
