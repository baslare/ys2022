require(tidyverse)
require(rvest)
require(sf)
require(ggsci)
require(extrafont) 
require(plotly)

source("C:/Users/Efe/Desktop/Projeler/ys2022/ys2022/ys_functions.R")


#all_list <- jsonlite::fromJSON("C:/Users/Efe/Desktop/Projeler/ys2022_files/ys_all_list.json")
ys_restdist_df <- jsonlite::fromJSON("C:/Users/Efe/Desktop/Projeler/ys2022_files/ys_restdist_df.json")
#ys_restdist <-  jsonlite::fromJSON("C:/Users/Efe/Desktop/Projeler/ys2022_files/ys_restdist.json")
#ys_url <- jsonlite::fromJSON("C:/Users/Efe/Desktop/Projeler/ys2022_files/ys_disturl.json")
df_master <- readRDS("C:/Users/Efe/Desktop/Projeler/ys2022_files/df_master.RDS")
ibb23haz <- read.csv("C:/Users/Efe/Desktop/Projeler/esmt_flash/ibb23Haz/ibb23Haz/ibb23Haz.csv",encoding = "UTF-8")
dnm <- read.csv("dnm.csv",header = FALSE)
ist_mah_demo_df <- readRDS("C:/Users/Efe/Desktop/Projeler/ys2022_files/ist_mah_demo_df.rds")
ist_ilce_demo_df <- readRDS("C:/Users/Efe/Desktop/Projeler/ys2022_files/ist_ilce_demo_df.rds")

#all_list: contains the menus of each restaurant, df_master is the df form
#ys_restdist: contains the name,url,rating of the restaurants that make deliveries to each district in list format
#ys_restdist_df: contains the name,url,rating of the restaurants that make deliveries to each district (contains duplicate restaurants)
#ys_url: urls of each restaurant (unique key)
#df_master: all items in restaurant menus in a single df


  
  product_name <- "Lahmacun"
  
  if(product_name=="Lahmacun"){
    df_sub <- df_master %>% 
      filter(str_detect(products,product_name)) %>% 
      filter(!str_detect(products,"Menü|[0-9]+|Kebap|Dürüm|Döner|Aras.|Pizza|Duble|Pide")) %>% 
      filter(!str_detect(products,"F.nd.k"))
    text.color = "#8a1a04"
    sci.palette = "yellow"
  }else if(product_name=="Burger"){
    
    df_sub <- df_master %>% 
      filter(str_detect(products, product_name)) %>% 
      filter(!str_detect(products,"Mini|Falafel|Ekme.i|sos|Süt|Domatex|Sosu$|Sos$|Baharat|Mayone.|Ket.ap|Islak|Menü|[0-9]+|Kebap|Dürüm|Döner|Aras.|Pizza|Duble|Pide")) %>% 
      filter(!str_detect(products,"F.nd.k"))
    text.color = "#0f1852"
    sci.palette = "blue"
  }else if(product_name=="Döner"){
    
    df_sub <- df_master %>% 
      filter(str_detect(products, product_name)) %>% 
      filter(!str_detect(products,"Tavuk|Beyti|Mini|Falafel|Ekme.i|sos|Süt|Domatex|Sosu$|Sos$|Baharat|Mayone.|Ket.ap|Islak|Menü|[0-9]+|Pizza|Duble|Pide|.skender|Double|Burger|Pilav|Kebap|X")) %>% 
      filter(!str_detect(products,"F.nd.k"))
    text.color = "#8a1a04"
    sci.palette = "yellow"
  }else if(product_name=="Tavuk Döner"){
    df_sub <- df_master %>% 
      filter(str_detect(products, "Döner")) %>% 
      filter(!str_detect(products,"Et|Beyti|Mini|Falafel|Ekme.i|sos|Süt|Domatex|Sosu$|Sos$|Baharat|Mayone.|Ket.ap|Islak|Menü|[0-9]+|Pizza|Duble|Pide|.skender|Double|Burger|Pilav|Kebap|X")) %>% 
      filter(!str_detect(products,"F.nd.k"))
    text.color = "#0f1852"
    sci.palette = "blue"
  }
 
  #store the rating/min delivery price/min delivery time in a df for each *unique* restaurant
  ys_restdist_df_unique <- ys_restdist_df %>% distinct(rest_url,.keep_all = T) %>% select(-district)
  
  
  #calculate the averages in each restaurant for the desired group  
  rest_product_averages <- df_sub %>% group_by(rest_name) %>% summarise(av_price=mean(prices))
  #alternative statistics
  #rest_product_median <- df_sub %>% group_by(rest_name) %>% summarise(av_price=median(prices))
  #rest_product_min <- df_sub %>% group_by(rest_name) %>% summarise(av_price=min(prices))
  #rest_product_max <- df_sub %>% group_by(rest_name) %>% summarise(av_price=max(prices))
  
  
  #merge the product averages for each restaurant df with other restaurant statistics
  
  ys_restdist_df$min_package_tl <- ys_restdist_df$min_package_tl %>% scales::squish(range = c(10,max(ys_restdist_df$min_package_tl)))
  
  
  dist_product_averages <- left_join(ys_restdist_df,rest_product_averages,by=c("rest_url"="rest_name"))
  dist_product_averages <- dist_product_averages %>% drop_na()
  
  
  rest_data_unique <- dist_product_averages %>% distinct(rest_url,.keep_all = T)
  
  
  
  dist_product_averages <- dist_product_averages %>% group_by(district) %>% mutate(weighted_md_amount = (1/(min_package_tl))/sum(1/(min_package_tl)),
                                                                             weighted_md_time = 1/(min_delivery_time)/sum(1/(min_delivery_time)) ,
                                                                             weighted_both = 1/((min_package_tl*min_delivery_time))/sum(1/((min_package_tl*min_delivery_time)))) %>% 
    ungroup()
  
  dist_product_averages <- dist_product_averages %>% group_by(district) %>% summarise(av_price_amount =sum(weighted_md_amount*av_price),
                                                                                      av_price_time = sum(weighted_md_time*av_price),
                                                                                      av_price_both = sum(weighted_both*av_price),
                                                                                      av_price=mean(av_price),
                                                                                      count = n(),
                                                                                      weighted_rating = sum(weighted_both*rating))
                                                                                      
                                                                                      
  dist_product_averages$district_name <- dist_product_averages$district %>% str_split("/")
  dist_product_averages$district_name <- sapply(dist_product_averages$district_name, function(x) x[[3]])
  dist_product_averages$district_name <- dist_product_averages$district_name %>% str_split("-",n = 2)   
  
  dist_product_averages$distr_name <- sapply(dist_product_averages$district_name, function(x) x[[1]])                                       
  dist_product_averages$distr_name <- dist_product_averages$distr_name %>% str_replace("macka","sisli")
  dist_product_averages$distr_name <- dist_product_averages$distr_name %>% str_replace("buyukada|kinaliada|burgazada","adalar")
  dist_product_averages$distr_name <- dist_product_averages$distr_name %>% str_replace("eyup","eyupsultan")
  
  
  
  dist_product_averages$district_name <- sapply(dist_product_averages$district_name,function(x){
    if(length(x) > 1){
      x[[2]]
    }else{
      x[[1]]
    }
  })
  
  
  
  
  ilce_average <- dist_product_averages %>%
    group_by(distr_name) %>% 
    summarise(av_price=sum(av_price_amount*count/(sum(count))),
                           av_price_uw=mean(av_price_amount),
                           av_rating=sum(weighted_rating*count/(sum(count))),
              count=mean(count))
  
  
  ilce_map <- read_sf("C:/Users/Efe/Desktop/Projeler/ys2022/shapefiles/ist_ilce.shp")
  ilce_map$no_tr = ilce_map$name %>% stringi::stri_trans_general("latin-ascii")
  ilce_map$no_tr <- ilce_map$no_tr %>% tolower()
  ilce_map <- ilce_map %>% select(no_tr,name,geometry)
  
  #ist_ilce_demo_df <- get_ist_demo()
  
  
  ilce_map <- left_join(ilce_map,ist_ilce_demo_df,by="no_tr")
  
  final_df <- left_join(ilce_map,ilce_average,by=c("no_tr"="distr_name"))
  final_df$dist_area <- (sf::st_area(final_df)/1000000) %>% as.character() %>%  as.numeric() %>% scales::squish(c(35,200))
  
  istbbx <- c(28.3025,	40.7933,	29.5223,	41.3812)
  
  final_df <- final_df %>% st_crop( xmin = istbbx[1], xmax = istbbx[3],
                                    ymin = istbbx[2], ymax = istbbx[4])
  
  
  
  final_df <- final_df %>% mutate(uni_grad_share = 0.01*universiteOran)
  final_df <- final_df %>% rename(population = ikiBin19Nufus,rest_count = count)

  final_df <- final_df %>% st_cast("MULTIPOLYGON")
  final_df <- final_df %>% mutate(price_text=as.character(round(av_price,digits=1)))
  
  
  
  lakes <- read_sf("C:/Users/Efe/Desktop/Projeler/ys2022/shapefiles/goller.shp")
  lakes <- lakes %>% filter(type == "multipolygon")
  lakes <- lakes %>% st_cast("MULTIPOLYGON")
  
  
  quant_10 <- final_df$av_price %>% quantile(0.1,na.rm = T)
  quant_90 <- final_df$av_price %>% quantile(0.9,na.rm = T)
  
  
  plot_1 <- ggplot(final_df) + 
    geom_sf(aes(fill=av_price),color="white",show.legend = FALSE,size=0.2) + 
    geom_sf(data=lakes,color="transparent",fill="white") +
    geom_sf_text(aes(label=price_text,
                     size= 2 + log(10 + dist_area),group=name),
                 color=text.color,
                 show.legend = FALSE,
                 family="Noto Sans") + 
    scale_fill_material(sci.palette,oob=scales::squish,limits=c(quant_10,quant_90)) +  #15,20 for the lahmacun map
    scale_size(range = c(2.5,5)) +
    theme_bw() +
    theme(axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_blank(),
          axis.title = element_blank(),
          text = element_text(color=text.color,family = "Noto Sans"),
          panel.border = element_rect(color="gray",size=0.3),
          plot.title = element_text(hjust = 0.5,size = 10),
          plot.caption = element_text(size=8),
          plot.subtitle = element_text(hjust = 0.5,size = 8),
    ) +
    scale_x_continuous(limits = c(istbbx[1],istbbx[3])) +
    scale_y_continuous(limits=c(istbbx[2],istbbx[4])) +
    ggtitle(label = paste("Average", product_name ,"Prices \n in Istanbul Districts(TL)"),subtitle = 'February 2022') + labs(caption = "Efe Başlar - @baslare")
  
  #interactive
  #plotly::ggplotly(plot,tooltip = "name")





#lahmacun: ggsci - yellow / text: #8a1a04
#burger: ggsci - blue / text: #0f1852

ggsave("ist_lahmacun_2022.jpeg",dpi = 500,height = 14,width = 24,units = "cm")


ggplot(rest_product_averages) + 
  geom_histogram(aes(av_price),bins = 60,fill="#d6c033",alpha=0.5)+
  geom_vline(aes(xintercept=mean(av_price)),linetype="dotted",size=1,color="#8a1a04") +
  labs(x="Restoran Lahmacun Fiyati (TL)",y="Restoran Sayisi") +
  scale_x_continuous(breaks = seq(10,80,10)) +
  theme(panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="#faf5ed"),
        text = element_text(family = "Noto Sans"))

ggsave("ist_lahmacun_hist.jpeg",dpi = 500,height = 14,width = 24,units = "cm")



#### regression #####

nb_product_averages <- dist_product_averages
nb_product_averages$district_name <- nb_product_averages$district_name %>% str_remove("ilcesi")
nb_product_averages$district_name <- nb_product_averages$district_name %>% str_replace_all("-"," ") %>% str_trim() 
nb_product_averages$district_name <- nb_product_averages$district_name %>% str_remove("mah$")
nb_product_averages$district_name <- nb_product_averages$district_name %>% str_trim()
nb_product_averages$district_name <- nb_product_averages$district_name %>% str_remove(" mah(.)+")
nb_product_averages$district_name <- nb_product_averages$district_name %>% str_replace("(?<=([0-9]))\\s",". ")
nb_product_averages$district_name <- ifelse(nb_product_averages$district_name %>% str_detect("atakent"),"atakent",nb_product_averages$district_name)

#dnm <- left_join(dist_product_averages, ist_mah_demo_df, by=c("district_name"="no_tr_mah","distr_name"="no_tr"))
#write.csv(dnm,"dnm.csv")





dnm <- dnm %>% select(V2,V9)
dnm$V9 <- dnm$V9 %>% tolower() %>%  str_trim()

nb_product_averages <- left_join(nb_product_averages,dnm,by=c("district"="V2"))
nb_product_averages$V9 <- nb_product_averages$V9 %>% str_replace("catalcesme","catalmese")



colnam <- colnames(nb_product_averages)[which(sapply(nb_product_averages,is.numeric))]

nb_product_averages <- nb_product_averages %>% group_by(distr_name,V9) %>% summarise_at(.vars = colnam,.funs = mean)

nb_product_averages <- left_join(nb_product_averages, ist_mah_demo_df, by=c("V9"="no_tr_mah","distr_name"="no_tr"))     
nb_product_averages <- nb_product_averages %>% drop_na()

nb_product_averages  <-nb_product_averages  %>% mutate(uni_grad_share = 0.01*universiteOran)
nb_product_averages  <- nb_product_averages  %>% rename(pop_density = nufusYogunluk, pop = ikiBin19Nufus, rest_count = count)




ibb23haz$no_tr <- ibb23haz$district %>% stringi::stri_trans_general("latin-ascii") %>% tolower()
ibb23haz$no_tr_mah <- ibb23haz$precinct %>% stringi::stri_trans_general("latin-ascii") %>% tolower()


colnam_ibb <- colnames(ibb23haz)[which(sapply(ibb23haz,is.numeric))]
ibb23haz <- ibb23haz %>% group_by(no_tr,no_tr_mah) %>% summarise_at(.vars = colnam_ibb,.funs = sum)
ibb23haz$no_tr_mah <- ibb23haz$no_tr_mah %>% str_remove("mah[.]$") %>% str_trim()
ibb23haz$no_tr_mah <- ibb23haz$no_tr_mah %>% str_replace("merkez ataturk","ataturk")


full_df <- left_join(nb_product_averages,ibb23haz,by=c("V9"="no_tr_mah","distr_name"="no_tr"))
full_df <- full_df %>% mutate(akp_share = akp_06/voter_06,
                              chp_share = chp_06/voter_06,
)


full_df <- full_df %>% filter(pop > 500)
dd <- lm(av_price_amount ~ youth_male_share + youth_female_share + pop + pop_density + weighted_rating + uni_grad_share + chp_share + akp_share + rest_count,data=full_df)