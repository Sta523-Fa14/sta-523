source("check_packages.R")
check_packages(c("httr","XML","stringr","stats","rvest","htmltools"))


# Create directory LaQuinta to store the LaQuinta webpage.
dir.create("LaQuinta/", showWarnings = FALSE)

url_LaQuinta_Inns_List = paste("http://www.lq.com/en/findandbook/hotel-listings.html")
write(content(GET(url_LaQuinta_Inns_List), as="text"), file="LaQuinta/LaQuinta_Inns_List.html")
page = html("LaQuinta/LaQuinta_Inns_List.html")

dataframe_hreflinks = NULL  

# For loop to scrape href links to La Quinta centers in all US states.

  for (i in ((1:47)*4+6)) # La Quinta site has provided its Inns' details in only 47 US states. 
  {
	css_component = paste0(".row:nth-child(",i,") .col-sm-12")
	nodes = html_nodes(page, css_component)
	anchors = html_nodes(nodes, "a")
	href_data = html_attr(anchors,"href")
	href_data = data.frame(href_data)
	dataframe_hreflinks <- rbind(dataframe_hreflinks,href_data)
  }

colnames(dataframe_hreflinks)=c("hreflink")
dataframe_hreflinks_final <- unique(dataframe_hreflinks)
n = nrow(dataframe_hreflinks_final)

# Create sub directory Inns_Pages under the directory LaQuinta to store the Inn specific webpages.
dir.create("LaQuinta/Inns_Pages", showWarnings = FALSE)

# For loop to scrape download La Quinta Inn specific webpages.

	for (j in 1:n) 
	{
	url_LaQuinta_sub_pages = paste0("http://www.lq.com",dataframe_hreflinks_final[j,1])
	write(content(GET(url_LaQuinta_sub_pages), as="text"), file=paste0("LaQuinta/Inns_Pages/la.html"))
	file.rename("LaQuinta/Inns_Pages/la.html",paste0("LaQuinta/Inns_Pages/LaQuinta_",j,".html"))
	Sys.sleep(rexp(1,1/2))
	}

save(dataframe_hreflinks_final,file="LaQuinta/LaQuinta_Hreflinks_Data.Rdata")
