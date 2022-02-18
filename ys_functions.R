

get_ist_demo <- function(type=1){
  
  type = 2
  
  if(type==1){
    query_ <-"ilce"
  }else{
    query_ <- "mahalle"
  }
  
  ist_demo_ids <- jsonlite::fromJSON("https://api.ibb.gov.tr/ibb-istatistik-api/istatistik/address?search=&_=1645114660853")
  ist_ilce_demo_ids <- ist_demo_ids %>% filter(type == 2)
  ist_ilce_demo_list <- list()
  
  
  i <- 1
  while(i <= length(ist_ilce_demo_ids$id)){
    
    id <- ist_ilce_demo_ids$id[i]
  
  
    Sys.sleep(0.3)
    ibb_item <- jsonlite::fromJSON(paste0("https://api.ibb.gov.tr/ibb-istatistik-api/istatistik/searchIstatistik?",query_,"=",id,"&_=",1645114436383+i))
    ist_ilce_demo_list <- append(ist_ilce_demo_list,list(ibb_item))
    
    i <- i + 1
    
  }
  
  
  #ist_ilce_demo_list <- readRDS("C:/Users/Efe/Desktop/Projeler/ys2022_files/ist_mah_demo_list.rds")
  
  ist_ilce_demo_df <- data.table::rbindlist(ist_ilce_demo_list)
  ist_ilce_demo_df <- ist_ilce_demo_df %>% select(ilceAd,nufusYogunluk,yuzOlcumDeger,mahalleAd,mahalleKod,ikiBin19Nufus,universiteOran,onbesOndokuzYasErkek:otuzBesOtuzDokuzYasErkek,onbesOndokuzYasKadin:otuzBesOtuzDokuzYasKadin)
  
  
  ist_ilce_demo_df <- ist_ilce_demo_df %>% mutate(youth_female = onbesOndokuzYasKadin+yirmiYirmiDortYasKadin+yirmiBesYirmiDokuzYasKadin+otuzOtuzDortYasKadin+otuzBesOtuzDokuzYasKadin,
                                                  youth_male = onbesOndokuzYasErkek+yirmiYirmiDortYasErkek+yirmiBesYirmiDokuzYasErkek+otuzOtuzDortYasErkek+otuzBesOtuzDokuzYasErkek,
                                                  youth_male_share = youth_male/ikiBin19Nufus,
                                                  youth_female_share = youth_female/ikiBin19Nufus) %>% select(-c(onbesOndokuzYasErkek:otuzBesOtuzDokuzYasErkek,onbesOndokuzYasKadin:otuzBesOtuzDokuzYasKadin))
  
  ist_ilce_demo_df$no_tr <- ist_ilce_demo_df$ilceAd %>% stringi::stri_trans_general("latin-ascii")
  ist_ilce_demo_df$no_tr <- ist_ilce_demo_df$no_tr %>% tolower()  
  
  ist_mah_demo_df <- ist_ilce_demo_df
  ist_mah_demo_df$no_tr_mah <- ist_mah_demo_df$mahalleAd %>% stringi::stri_trans_general("latin-ascii")
  ist_mah_demo_df$no_tr_mah <- ist_mah_demo_df$no_tr_mah %>% tolower()
  
  saveRDS(ist_mah_demo_df,"ist_mah_demo_df.rds")
  
  return(ist_mah_demo_df)
  
}

