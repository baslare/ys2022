require(tidyverse)
require(rvest)
require(sf)
require(ggsci)
require(extrafont) 
require(plotly)


ys_restdist <- jsonlite::fromJSON("ys_restdist.json")
ys_url <- jsonlite::fromJSON("ys_disturl.json")


ys_restdist <- lapply(ys_restdist, function(x) x %>% as.data.frame() %>% data.table::transpose())
ys_restdist <- Map(function(x,y) x %>% mutate(district=y),x=ys_restdist,y=ys_url)

ys_restdist_df <- ys_restdist %>% bind_rows()

ys_restdist_df <- ys_restdist_df %>% filter(rating != "-")
ys_restdist_df <- ys_restdist_df %>% mutate(rating = rating %>% str_replace(",",".") %>% as.numeric())
ys_restdist_df$min_package_tl <-  ys_restdist_df$min_package_tl  %>% 
  str_remove("[A-z]+") %>% str_replace(",",".") %>% str_trim() %>% as.numeric()

ys_restdist_df$min_delivery_time <- ys_restdist_df$min_delivery_time %>% as.numeric()

colnames(ys_restdist_df)[1:5] <- c("rest","rest_url","rating","min_package_tl","min_delivery_time")

jsonlite::write_json(ys_restdist_df,"ys_restdist_df.json")


unique_url <- unique(ys_restdist_df$rest_url)


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



#all_list <- jsonlite::fromJSON("C:/Users/Efe/Desktop/Projeler/ys2022_files/ys_all_list.json")
#ys_restdist_df <- jsonlite::fromJSON("C:/Users/Efe/Desktop/Projeler/ys2022_files/ys_restdist_df.json")
#ys_restdist <-  jsonlite::fromJSON("C:/Users/Efe/Desktop/Projeler/ys2022_files/ys_restdist.json")
#ys_url <- jsonlite::fromJSON("C:/Users/Efe/Desktop/Projeler/ys2022_files/ys_disturl.json")

#all_list: contains the menus of each restaurant, df_master is the df form
#ys_restdist: contains the name,url,rating of the restaurants that make deliveries to each district in list format
#ys_restdist_df: contains the name,url,rating of the restaurants that make deliveries to each district (contains duplicate restaurants)


all_list2 <- lapply(all_list, function(x) x[[1]])
rest_names <- sapply(all_list, function(x) x[[2]])

has_menu <- sapply(all_list2, is.data.frame)

all_list2 <- all_list2[has_menu]
rest_names <- rest_names[has_menu]

all_list2 <- Map(function(x,y) x %>% mutate(rest_name = y), x=all_list2,y=rest_names)


df_master <- all_list2 %>% bind_rows()
df_master$prices <- df_master$prices %>% str_remove("[A-z]+") %>% str_replace(",",".") %>% str_trim() %>% as.numeric()


saveRDS(df_master,"df_master.RDS")
###



  


  

