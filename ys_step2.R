require(tidyverse)
require(rvest)
require(sf)
require(ggsci)
require(extrafont) 


ys_dist <- jsonlite::fromJSON("ys_restlist.json")
ys_url <- jsonlite::fromJSON("ys_disturl.json")


ys_dist <- lapply(ys_dist, function(x) x %>% as.data.frame() %>% data.table::transpose())
ys_dist <- Map(function(x,y) x %>% mutate(district=y),x=ys_dist,y=ys_url)

ys_dist_df <- ys_dist %>% bind_rows()

colnames(ys_dist_df)[1:5] <- c("rest","rest_url","rating","min_package_tl","min_delivery_time")

jsonlite::write_json(ys_dist_df,"ys_dist_df.json")


unique_url <- unique(ys_dist_df$rest_url)


rootURL <- "https://www.yemeksepeti.com"

i <- 1

all_list <- list()


# if recovering from the cache
# all_list <- jsonlite::fromJSON("ys_all_list.json")
i <- length(all_list)

while(i <= length(unique_url)){
  
  ys <- rvest::html_session(paste0(rootURL,unique_url[i]))
  
  if(ys$response$status_code == 200){
    
    classCheck <- ys %>% html_nodes(".product")
    
    
    if(length(classCheck) > 0){
      products <- ys %>% rvest::html_nodes(".product") %>% html_node(".product-info") %>% rvest::html_text(trim=T)
      products_info <-  ys %>% rvest::html_nodes(".product") %>% html_node(".product-desc") %>% rvest::html_text(trim=T)
      prices <- ys %>% rvest::html_nodes(".price") %>% rvest::html_text(trim=T)
    }else{
      products <- ys %>% rvest::html_nodes(".productName,.notAvailableProduct")  %>% rvest::html_text(trim=T)
      products_info <-  ys %>% rvest::html_nodes(".productInfo") %>% rvest::html_text(trim=T)
      prices <- ys %>% rvest::html_nodes(".newPrice") %>% rvest::html_text(trim=T)
      
    }
    
    
    rest_single_df <- data.frame(products,products_info,prices) %>% distinct(products,.keep_all = T)
    
    rest_single_item <- list(rest_single_df,unique_url[i])
    all_list <- append(all_list,list(rest_single_item))
  }
  
  
  
  if(i %% 50 == 0){
    message(paste0(i))
    write_json(all_list,"ys_all_list.json")
    
  }
  
  
  i <- i + 1
  
  
}

require(tidyverse)

all_list2 <- lapply(all_list, function(x) x[[1]])
rest_names <- sapply(all_list, function(x) x[[2]])

has_menu <- sapply(all_list2, is.data.frame)

all_list2 <- all_list2[has_menu]
rest_names <- rest_names[has_menu]

all_list2 <- Map(function(x,y) x %>% mutate(rest_name = y), x=all_list2,y=rest_names)


df_master <- all_list2 %>% bind_rows()
df_master$prices <- df_master$prices %>% str_remove("[A-z]+") %>% str_replace(",",".") %>% str_trim() %>% as.numeric()
ys_dist_df$min_package_tl <- ys_dist_df$min_package_tl  %>% str_remove("[A-z]+") %>% str_replace(",",".") %>% str_trim() %>% as.numeric()


df_sub <- df_master %>% 
  filter(str_detect(products,"Lahmacun")) %>% 
  filter(!str_detect(products,"Menü|[0-9]+|Kebap|Dürüm|Döner|Aras.|Pizza|Duble|Pide")) %>% 
  filter(!str_detect(products,"F.nd.k"))

rest_product_averages <- df_sub %>% group_by(rest_name) %>% summarise(av_price=mean(prices))

dist_product_averages <- left_join(ys_dist_df,rest_product_averages,by=c("rest_url"="rest_name"))

dist_product_averages <- dist_product_averages %>% drop_na()
dist_product_averages$district_name <- dist_product_averages$district %>% str_split("/")
dist_product_averages$district_name <- sapply(dist_product_averages$district_name, function(x) x[[3]])
dist_product_averages$district_name <- dist_product_averages$district_name %>% str_split("-",n = 2)                                              
       
dist_product_averages$distr_name <- sapply(dist_product_averages$district_name, function(x) x[[1]])                                       
dist_product_averages$neigh_name <- sapply(dist_product_averages$district_name, function(x) x[[2]]) 
dist_product_averages$distr_name <- dist_product_averages$distr_name %>% str_replace("macka","sisli")
dist_product_averages$distr_name <- dist_product_averages$distr_name %>% str_replace("buyukada|kinaliada|burgazada","adalar")
dist_product_averages$distr_name <- dist_product_averages$distr_name %>% str_replace("eyup","eyupsultan")

ilce_average <- dist_product_averages %>% group_by(distr_name) %>% summarise(av_price=mean(av_price))



ilce_map <- read_sf("shapefiles/ist_ilce.shp")
ilce_map$no_tr = ilce_map$name %>% tolower()
ilce_map$no_tr <- ilce_map$no_tr %>% stringi::stri_trans_general("latin-ascii")
ilce_map <- ilce_map %>% select(no_tr,name,geometry)

final_df <- left_join(ilce_map,ilce_average,by=c("no_tr"="distr_name"))
final_df$dist_area <- (sf::st_area(final_df)/1000000) %>% as.character() %>%  as.numeric() %>% scales::squish(c(35,200))

istbbx <- c(28.3025,	40.7933,	29.5223,	41.3812)

final_df <- final_df %>% st_crop( xmin = istbbx[1], xmax = istbbx[3],
                                 ymin = istbbx[2], ymax = istbbx[4])

final_df <- final_df %>% st_cast("MULTIPOLYGON")



lakes <- read_sf("shapefiles/goller.shp")
lakes <- lakes %>% filter(type == "multipolygon")
lakes <- lakes %>% st_cast("MULTIPOLYGON")



ggplot(final_df) + 
  geom_sf(aes(fill=av_price),color="white",show.legend = FALSE,size=0.2) + 
  geom_sf(data=lakes,color="transparent",fill="white") +
  geom_sf_text(aes(label=as.character(round(av_price,digits=1)),
                   size= 2 + log(10 + dist_area)),
               color="#8a1a04",
               show.legend = FALSE,
               family="Noto Sans") + 
  scale_fill_material("yellow",oob=scales::squish,limits=c(15,20)) + 
  scale_size(range = c(2.5,5)) +
  theme_bw() +
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.title = element_blank(),
        panel.border = element_rect(color="gray",size=0.3),
        plot.title = element_text(color="#8a1a04",hjust = 0.5,size = 10),
        plot.caption = element_text(color="#8a1a04",size=8),
        plot.subtitle = element_text(color="#8a1a04",hjust = 0.5,size = 8),
        text = element_text(family = "Noto Sans")
        
        
        ) +
  scale_x_continuous(limits = c(istbbx[1],istbbx[3])) +
  scale_y_continuous(limits=c(istbbx[2],istbbx[4])) +
  ggtitle(label = "İstanbul İlçelerinde \n Ortalama Lahmacun Fiyatları (TL)",subtitle = 'Şubat 2022') + labs(caption = "Efe Başlar - @baslare")

#interactive
#plotly::ggplotly()
  
ggsave("ist_lahmacun_2022.jpeg",dpi = 500,height = 14,width = 24,units = "cm")
  

ggplot(rest_product_averages) + 
  #geom_density(aes(av_price,..count..),fill=viridis::inferno(1,begin = 0.85),color=viridis::inferno(1,begin = 0.75),alpha=0.3,bw=0.5) +
  geom_histogram(aes(av_price),bins = 60,fill="#d6c033",alpha=0.5)+
  geom_vline(aes(xintercept=mean(av_price)),linetype="dotted",size=1,color="#8a1a04") +
  labs(x="Restoran Lahmacun Fiyati (TL)",y="Restoran Sayisi") +
  scale_x_continuous(breaks = seq(10,80,10)) +
  theme(panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="#faf5ed"),
        text = element_text(family = "Noto Sans"))

ggsave("ist_lahmacun_hist.jpeg",dpi = 500,height = 14,width = 24,units = "cm")
  


  

