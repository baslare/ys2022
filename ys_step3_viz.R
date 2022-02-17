require(tidyverse)
require(rvest)
require(sf)
require(ggsci)
require(extrafont) 
require(plotly)

source("ys_functions.R")


#all_list <- jsonlite::fromJSON("C:/Users/Efe/Desktop/Projeler/ys2022_files/ys_all_list.json")
#ys_restdist_df <- jsonlite::fromJSON("C:/Users/Efe/Desktop/Projeler/ys2022_files/ys_restdist_df.json")
#ys_restdist <-  jsonlite::fromJSON("C:/Users/Efe/Desktop/Projeler/ys2022_files/ys_restdist.json")
#ys_url <- jsonlite::fromJSON("C:/Users/Efe/Desktop/Projeler/ys2022_files/ys_disturl.json")
#df_master <- readRDS("C:/Users/Efe/Desktop/Projeler/ys2022_files/df_master.RDS")

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
  }else if(product_name=="Burger"){
    
    df_sub <- df_master %>% 
      filter(str_detect(products, product_name)) %>% 
      filter(!str_detect(products,"Mini|Falafel|Ekme.i|sos|Süt|Domatex|Sosu$|Sos$|Baharat|Mayone.|Ket.ap|Islak|Menü|[0-9]+|Kebap|Dürüm|Döner|Aras.|Pizza|Duble|Pide")) %>% 
      filter(!str_detect(products,"F.nd.k"))
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
                                                                                      weighted_count = sum(weighted_both*count),
                                                                                      weighted_rating = sum(weighted_both*rating))
                                                                                      
  dist_product_averages$district_name <- dist_product_averages$district %>% str_split("/")
  dist_product_averages$district_name <- sapply(dist_product_averages$district_name, function(x) x[[3]])
  dist_product_averages$district_name <- dist_product_averages$district_name %>% str_split("-",n = 2)   
  
  dist_product_averages$distr_name <- sapply(dist_product_averages$district_name, function(x) x[[1]])                                       
  dist_product_averages$distr_name <- dist_product_averages$distr_name %>% str_replace("macka","sisli")
  dist_product_averages$distr_name <- dist_product_averages$distr_name %>% str_replace("buyukada|kinaliada|burgazada","adalar")
  dist_product_averages$distr_name <- dist_product_averages$distr_name %>% str_replace("eyup","eyupsultan")
                                                                                      
                                                                                
  
  
  
  
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
  
  ist_ilce_demo_df <- get_ist_demo()
  
  ilce_map <- left_join(ilce_map,ist_ilce_demo_df,by="no_tr")
  
  final_df <- left_join(ilce_map,ilce_average,by=c("no_tr"="distr_name"))
  final_df$dist_area <- (sf::st_area(final_df)/1000000) %>% as.character() %>%  as.numeric() %>% scales::squish(c(35,200))
  
  istbbx <- c(28.3025,	40.7933,	29.5223,	41.3812)
  
  final_df <- final_df %>% st_crop( xmin = istbbx[1], xmax = istbbx[3],
                                    ymin = istbbx[2], ymax = istbbx[4])
  
  
  dd <- lm(av_price ~ youth_male_share + youth_female + ikiBin19Nufus + av_rating + universiteOran + count,data=final_df)
  
  final_df <- final_df %>% st_cast("MULTIPOLYGON")
  final_df <- final_df %>% mutate(price_text=as.character(round(av_price,digits=1)))
  
  
  
  lakes <- read_sf("C:/Users/Efe/Desktop/Projeler/ys2022/shapefiles/goller.shp")
  lakes <- lakes %>% filter(type == "multipolygon")
  lakes <- lakes %>% st_cast("MULTIPOLYGON")
  
  
  quant_10 <- final_df$av_price %>% quantile(0.1,na.rm = T)
  quant_90 <- final_df$av_price %>% quantile(0.9,na.rm = T)
  
  
  plot <- ggplot(final_df) + 
    geom_sf(aes(fill=av_price),color="white",show.legend = FALSE,size=0.2) + 
    geom_sf(data=lakes,color="transparent",fill="white") +
    geom_sf_text(aes(label=price_text,
                     size= 2 + log(10 + dist_area),group=name),
                 color="#0f1852",
                 show.legend = FALSE,
                 family="Noto Sans") + 
    scale_fill_material("yellow",oob=scales::squish,limits=c(quant_10,quant_90)) +  #15,20 for the lahmacun map
    scale_size(range = c(2.5,5)) +
    theme_bw() +
    theme(axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_blank(),
          axis.title = element_blank(),
          text = element_text(color="#0f1852",family = "Noto Sans"),
          panel.border = element_rect(color="gray",size=0.3),
          plot.title = element_text(hjust = 0.5,size = 10),
          plot.caption = element_text(size=8),
          plot.subtitle = element_text(hjust = 0.5,size = 8),
    ) +
    scale_x_continuous(limits = c(istbbx[1],istbbx[3])) +
    scale_y_continuous(limits=c(istbbx[2],istbbx[4])) +
    ggtitle(label = paste("Average", product_name ,"Prices \n in Istanbul Districts(TL)"),subtitle = 'February 2022') + labs(caption = "Efe Başlar - @baslare")
  
  #interactive
  plotly::ggplotly(plot,tooltip = "name")





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