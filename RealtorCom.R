library(httr)
library(bitops) #<- needed for RCurl, seeing if loading it explicitly helps mitigate connection errors
library(RCurl)
library(rvest)
library(XML)
library(xml2)
library(curl)
library(jsonlite)
library(civis)

##### REALTOR.COM #####
rcb <- "https://www.realtor.com/apartments/"
#Then the zip
pgvw <- "/pg-"
#Then the page number

zips <- {c("02118",
           "02119",
           "02120",
           "02130",
           "02134",
           "02135",
           "02445",
           "02446",
           "02447",
           "02467",
           "02108",
           "02114",
           "02115",
           "02116",
           "02215",
           "02128",
           "02129",
           "02150",
           "02151",
           "02152",
           "02124",
           "02126",
           "02131",
           "02132",
           "02136",
           "02109",
           "02110",
           "02111",
           "02113",
           "02121",
           "02122",
           "02124",
           "02125",
           "02127",
           "02210"
)}

realtor <- data.frame("Address" = as.character(),
                      "Price" = as.character(),
                      "Beds" = as.character(),
                      "Baths" = as.character(),
                      "SQFT" = as.character(),
                      "Lat" = as.character(),
                      "Lon" = as.character(),
                      "Pets" = as.character(),
                      "Desc" = as.character(),
                      "Link" = as.character(),
                      "Scrape_Date" = as.character(),
                      "Scrape_Zip" = as.character()
)


#for (i in 1:length(zips)){
for(i in 1){
  #First page to find out max pages
  
  ####
  print("BUILDING URL")
  ####
  
  
  urlp1 <- paste(rcb, zips[i], sep = "")
  
  slp <- sample(1:6, 1)
  print(paste("Sleeping for", slp, "seconds at", Sys.time()))
  Sys.sleep(slp)
  
  ####
  print("TRY CATCH STARTED")
  ####
  
  chka <-  tryCatch({
    read_html(urlp1)
  },
  error = function(e){e}
  )
  
  if(inherits(chka, "error")) {
    print("URL Broken")
    next
  }
  
  ####
  print("TRY CATCH SUCCEEDED!")
  ####
  
  slp <- sample(1:6, 1)
  print(paste("Sleeping for", slp, "seconds at", Sys.time()))
  Sys.sleep(slp)
  
  rclp <- read_html(urlp1)
  #get the max pages for this zip
  maxp <- rclp %>% 
    html_nodes(".pagination") %>%
    html_children() %>% 
    .[(grep("next", .)-1)] %>% 
    html_text() %>% 
    gsub("\n", "",.) %>% 
    as.numeric()
  print(paste("FOUND", maxp, "PAGES FOR ZIP", i))
  
  
  ####
  print("GETTING LINKS")
  ####
  
  
  #Get links
  plk <- rclp %>% 
    html_nodes(".component_property-card.js-component_property-card") %>% 
    html_nodes(".card-box.js-navigate-to.js-record-user-activity") %>% 
    html_attr("data-url") %>%
    paste("https://www.realtor.com", ., sep = "")
  
  if(maxp > 1){
    print(paste("RUNNING PAGES"))
    for (t in 2:maxp){
      print(paste("STARTING PAGE",t))
      urlp <- paste(rcb, zips[i],pgvw, t, sep = "")
      
      slp <- sample(1:6, 1)
      print(paste("Sleeping for", slp, "seconds at", Sys.time()))
      Sys.sleep(slp)
      
      ####
      print("STARTING PAGE TRY CATCH")
      ####
      
      chka <-  tryCatch({
        read_html(urlp)
      },
      error = function(e){e}
      )
      
      if(inherits(chka, "error")) {
        print("URL Broken")
        next
      }
      
      ####
      print("PAGE TRY CATCH SUCCEEDED!")
      ####
      
      slp <- sample(1:6, 1)
      print(paste("Sleeping for", slp, "seconds at", Sys.time()))
      Sys.sleep(slp)
      
      rclpv <- read_html(urlp)
      #get the max pages for this zip
      
      #Get links
      plkv <- rclpv %>% 
        html_nodes(".component_property-card.js-component_property-card") %>% 
        html_nodes(".card-box.js-navigate-to.js-record-user-activity") %>% 
        html_attr("data-url") %>%
        paste("https://www.realtor.com", ., sep = "")
      
      plk <- c(plk, plkv)
      
      print(paste("FOUND", length(plkv), "LISTINGS"))
    } #END ZIP MULTIPLE LANDING PAGE PULLS
    print(paste("FOUND", length(plk), "LISTINGS FOR ZIP", zips[i]))
  } #END IF STATEMENT FOR ZIPS WITH MORE THAN 1 PAGE
  
  #Extract link data
  
  ####
  print(paste("BEGIN LISTING SCRAPES FOR ZIP", zips[i]))
  ####
  
  for(z in 1:length(plk)){
    print(paste("STARTING LISTING", z, "OF", length(plk), "FOR ZIP", i))
    slp <- sample(1:6, 1)
    print(paste("Sleeping for", slp, "seconds at", Sys.time()))
    Sys.sleep(slp)
    
    ####
    print(paste("TRY CATCH FOR LISTING", z, "STARTED"))
    ####
    
    chka <-  tryCatch({
      read_html(plk[z])
    },
    error = function(e){e}
    )
    
    if(inherits(chka, "error")) {
      print("URL Broken")
      next
    }
    
    ####
    print(paste("TRY CATCH FOR LISTING", z, "SUCCEEDED!"))
    ####
    
    slp <- sample(1:6, 1)
    print(paste("Sleeping for", slp, "seconds at", Sys.time()))
    Sys.sleep(slp)
    
    lizt <- read_html(plk[z])
    
    #Check for .ldp-detail-rental-floorplans %>% list-floorplans
    #if it's there, then there are a few units with data
    #run a for loop for each of the nodes that come up
    #Do this run AFTER you already pull the description and other non-unit specific data
    flch <- lizt %>% 
      html_nodes("#ldp-detail-rental-floorplans") %>% 
      html_nodes(".list-floorplans")
    
    
    if(length(flch) > 0){
      
      ##Pets[?]
      pts <- lizt %>% 
        html_nodes(".load-more-features") %>% 
        html_nodes("ul.list-default") %>%
        html_children() %>% 
        .[grep("Allowed",.)] %>% 
        html_text() %>% 
        paste(., collapse = "; ")
      if(length(pts) == 0){
        pts <- NA
      }
      
      ##Post Description
      desc <- lizt %>% 
        html_nodes("#ldp-detail-romance") %>% 
        html_text()
      if(length(desc) == 0){
        desc <- NA
      }
      
      ##Address
      rcadd <- lizt %>% 
        html_nodes(".ldp-header-address") %>% 
        html_attr("content")
      if(length(rcadd) == 0){
        rcadd <- NA
      }
      
      ##lat
      lat <- lizt %>% 
        html_nodes(".ldp-header-address") %>% 
        html_nodes("meta") %>% 
        .[grep("latitude",.)] %>% 
        html_attr("content")
      if(length(lat) == 0){
        lat <- NA
      }
      
      ##lon
      lon <- lizt %>% 
        html_nodes(".ldp-header-address") %>% 
        html_nodes("meta") %>% 
        .[grep("longitude",.)] %>% 
        html_attr("content")
      if(length(lon) == 0){
        lon <- NA
      }
      
      #units
      un <- lizt %>% 
        html_nodes("#ldp-detail-rental-floorplans") %>% 
        html_nodes(".list-floorplans") %>%
        html_nodes(".accordion")
      
      
      
      
      print(paste("FOUND", length(un), "UNITS"))
      for(g in 1:length(un)){
        ##Price
        rcpr <- un[g] %>% 
          html_nodes(".panel") %>% 
          html_nodes("table") %>% 
          html_nodes(".col-price.text-right") %>% 
          html_text() %>% 
          gsub("\\$", "",.) %>% 
          gsub(",", "",.)
        if(length(rcpr) == 0){
          rcpr <- NA
        }
        
        ##Beds Num
        bds <- un[g] %>% 
          html_nodes(".panel") %>% 
          html_nodes(".accordion-heading") %>% 
          html_nodes("span") %>% 
          html_text() %>% 
          gsub(" Bedroom", "",.)
        if(length(bds) == 0){
          bds <- NA
        }
        
        ##Baths Num
        bths <- un[g] %>% 
          html_nodes(".panel") %>% 
          html_nodes("table") %>% 
          html_nodes(".col-bath.text-right") %>% 
          html_text() %>% 
          gsub(" ba", "",.)
        if(length(bths) == 0){
          bths <- NA
        }
        
        
        ##Sqf
        sqf <- un[g] %>% 
          html_nodes(".panel") %>% 
          html_nodes("table") %>% 
          html_nodes(".col-sqft.text-right") %>% 
          html_text() %>% 
          gsub(" sq ft", "",.) %>% 
          gsub(",", "",.) %>% 
          gsub("\n", "",.) %>% 
          gsub("\t", "",.) %>% 
          gsub(" ", "",.)
        if(length(sqf) == 0){
          sqf <- NA
        }
        
        grb <- data.frame("Address" = rcadd,
                          "Price" = rcpr,
                          "Beds" = bds,
                          "Baths" = bths,
                          "SQFT" = sqf,
                          "Lat" = lat,
                          "Lon" = lon,
                          "Pets" = pts,
                          "Desc" = desc,
                          "Link" = z,
                          "Scrape_Date" = format(Sys.time(), "%m-%d-%Y"),
                          "Scrape_Zip" = zips[i]
        )
        realtor <- rbind(realtor, grb)
        
        print(paste("FINISHED UNIT", g, "OF", length(un)))
      }
      print(paste("FINISHED LISTING", z, "OF", length(plk), "FOR ZIP", zips[i]))
      
    } else { # END FOR IF MULTIPLE UNITS IN LISTING
      print(paste("STARTING LISTING", z, "OF", length(plk), "FOR ZIP", zips[i]))
      ##Address
      rcadd <- lizt %>% 
        html_nodes(".ldp-header-address") %>% 
        html_attr("content")
      if(length(rcadd) == 0){
        rcadd <- NA
      }
      
      ##lat
      lat <- lizt %>% 
        html_nodes(".ldp-header-address") %>% 
        html_nodes("meta") %>% 
        .[grep("latitude",.)] %>% 
        html_attr("content")
      if(length(lat) == 0){
        lat <- NA
      }
      
      ##lon
      lon <- lizt %>% 
        html_nodes(".ldp-header-address") %>% 
        html_nodes("meta") %>% 
        .[grep("longitude",.)] %>% 
        html_attr("content")
      if(length(lon) == 0){
        lon <- NA
      }
      
      ##Price
      rcpr <- lizt %>% 
        html_nodes(".display-inline") %>% 
        .[grep("offers",.)] %>% 
        html_nodes("span") %>% 
        .[grep("price",.)] %>% 
        html_attr("content")
      if(length(rcpr) == 0){
        rcpr <- NA
      }
      
      ##Beds Num
      bds <- lizt %>% 
        html_nodes(".ldp-header-address") %>% 
        html_nodes("meta") %>% 
        .[grep("numberOfRooms",.)] %>% 
        html_attr("content")
      if(length(bds) == 0){
        bds <- NA
      }
      
      ##Baths Num
      bths <- lizt %>% 
        html_nodes("#ldp-property-meta") %>% 
        html_children() %>% 
        html_nodes("li") %>% 
        .[grep("bath",.)] %>% 
        html_text() %>% 
        gsub("\n", "",.) %>% 
        gsub("bath", "",.) %>% 
        gsub("  ", "",.)
      if(length(bths) == 0){
        bths <- NA
      }
      
      
      ##Sqf
      sqf <- lizt %>% 
        html_nodes("#ldp-property-meta") %>% 
        html_children() %>% 
        html_nodes("li") %>% 
        .[grep("sqft",.)] %>% 
        html_text() %>% 
        gsub("\n", "",.) %>% 
        gsub(" sq ft", "",.) %>% 
        gsub("  ", "",.)
      if(length(sqf) == 0){
        sqf <- NA
      }
      
      ##Pets[?]
      pts <- lizt %>% 
        html_nodes(".load-more-features") %>% 
        html_nodes("ul.list-default") %>%
        html_children() %>% 
        .[grep("Allowed",.)] %>% 
        html_text() %>% 
        paste(., collapse = "; ")
      if(length(pts) == 0){
        pts <- NA
      }
      
      ##Post Description
      desc <- lizt %>% 
        html_nodes("#ldp-detail-romance") %>% 
        html_text()
      if(length(desc) == 0){
        desc <- NA
      }
      
      
      grb <- data.frame("Address" = rcadd,
                        "Price" = rcpr,
                        "Beds" = bds,
                        "Baths" = bths,
                        "SQFT" = sqf,
                        "Lat" = lat,
                        "Lon" = lon,
                        "Pets" = pts,
                        "Desc" = desc,
                        "Link" = z,
                        "Scrape_Date" = format(Sys.time(), "%m-%d-%Y"),
                        "Scrape_Zip" = zips[i]
      )
      realtor <- rbind(realtor, grb)
  
      print(paste("FINISHED LISTING", z, "OF", length(plk), "FOR ZIP", zips[i]))
      
    } #END ELSE FOR IF MULTIPLE UNITS IN LISTING
  } #END ZIP LISTINGS FOR LOOP
  print(paste("FINISHED ZIP", zips[i], "--", i, "OF", length(zips)))
} #END ZIPS FOR LOOP

realtor <- unique(realtor)
#r.curs <- read.csv("H:/My Documents/Short Term Rental Scraper/Outputs/Realtor.csv", stringsAsFactors = FALSE)
#rnew <- rbind(r.curs, realtor)

write_civis(rnew, tablename = "sandbox.realtor_master", if_exists = "append")
write.csv(rnew, "H:/My Documents/Short Term Rental Scraper/Outputs/Realtor.csv", row.names = FALSE)

