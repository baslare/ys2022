# -*- coding: utf-8 -*-
"""
Created on Wed Feb  9 23:55:23 2022

@author: Efe
"""

from selenium import webdriver
from selenium.webdriver.common.keys import Keys
import pandas as pd
import numpy as np
import json
import rpy2.robjects as robjects
from rpy2.robjects.packages import importr
from rpy2.robjects import pandas2ri

#Must be activated first
pandas2ri.activate()


rvest = importr("rvest")
xml2 = importr("xml2")


ys = rvest.html_session("www.yemeksepeti.com/istanbul")
distURL = rvest.html_attr(rvest.html_children(rvest.html_node(ys,xpath="/html/body/header/div/div/div/div[2]/select/optgroup[1]")),"data-url")
distURL_py = pandas2ri.ri2py_vector(distURL)


driver = webdriver.Firefox()

rootURL = "http://www.yemeksepeti.com"

i=0

data_list = []


#if recovering from the cache
#tmp = pd.read_json("ys_restlist.json")
#tmp = tmp.transpose()
#data_list = tmp.apply(lambda x: [np.array(j) for j in x.tolist()],axis=1)
#data_list = data_list.tolist()

while(i < len(distURL_py)):
    
      driver.get("about:blank")
        #using selenium to be able to get the list of the restaurants that are closed at a given time, as it is a JS action that displays all the restaurants

      rest_count = int(driver.find_element("class name","ys-result-count").find_element("tag name","span").text)
      current_count = len(driver.find_elements("class name","restaurantName"))

      while(current_count < rest_count):
          driver.find_element("tag name","body").send_keys(Keys.PAGE_DOWN)
          driver.find_element("tag name","body").send_keys(Keys.PAGE_DOWN)
          current_count = len(driver.find_elements("class name","restaurantName"))
          

      page_src = driver.page_source


        #call rvest from R to do the job, well, because it is amazing :)
      page_src = xml2.read_html(page_src)
      rest_list = rvest.html_nodes(page_src,".restaurant-main-info")
      rest_time = rvest.html_attr(rvest.html_node(rest_list,".head"),"data-deliverytime")
      rest_min = rvest.html_attr(rvest.html_node(rest_list,".head"),"data-minprice")
      rest_point = rvest.html_text(rvest.html_node(rvest.html_node(rest_list,".head"),".point"))
      rest_url = rvest.html_attr(rvest.html_node(rest_list,".restaurantName"),"href")
      rest_name = rvest.html_text(rvest.html_node(rest_list,".restaurantName"),trim=True)  
      
      
      rest_time = pandas2ri.ri2py_vector(rest_time)
      rest_min = pandas2ri.ri2py_vector(rest_min)
      rest_point = pandas2ri.ri2py_vector(rest_point)
      rest_url = pandas2ri.ri2py_vector(rest_url)
      rest_name = pandas2ri.ri2py_vector(rest_name)
      
      
      distr_prop = [rest_name,rest_url,rest_point,rest_min,rest_time]
      data_list.append(distr_prop)
      
      print(i, "of", len(distURL_py))
      if i % 10 == 0:
          out_file = open("ys_restlist.json", "w",encoding='utf-8')
          data_list_series = pd.Series(data_list)
          data_list_series.to_json(out_file,  force_ascii= False )
          out_file.close()
      
      i += 1
      
      out_file = open("ys_disturl.json", "w",encoding='utf-8')
      pd.Series(distURL_py).to_json(out_file,  force_ascii= False )
      out_file.close()
  
      
      
      
      
