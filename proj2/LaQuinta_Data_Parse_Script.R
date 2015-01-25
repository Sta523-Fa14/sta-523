source("check_packages.R")
check_packages(c("httr","XML","stringr","stats","rvest","htmltools"))


load("LaQuinta/LaQuinta_Hreflinks_Data.Rdata")
m = nrow(dataframe_hreflinks_final)

La_Quinta_Inns_data = matrix(NA,m,10)

# For loop to scrape the La Quinta Inns specific information from the previously downloaded html pages. 

	for (p in 1:m) # Total number of La Quinta Inns is 848
	{
		# Selector Gadget is used to retrieve CSS component names for Inn details such as Inn name, Address, Phone number, and Fax number. 
	
		sub_page_url = paste0("LaQuinta/Inns_Pages/LaQuinta_",p,".html")
		sub_page = html(sub_page_url)
	
		sub_page_Inn_text = html_text(html_nodes(sub_page, "h3"))
		sub_page_Inn_name = gsub("\r\n        ","",sub_page_Inn_text)
		
		sub_page_details = html_nodes(sub_page, ".propProfileContent p")
		html_string=html_text(sub_page_details)
		details <- unlist(strsplit(html_string,'\r\n            '))
		Address = paste(details[1],details[2])
		Phone = gsub("Phone: 1-","",details[5])
		Fax = gsub("Fax: 1-","",details[8])
		
		La_Quinta_Inns_data[p,1] = p
		La_Quinta_Inns_data[p,2] = sub_page_Inn_name 
		La_Quinta_Inns_data[p,3] = Address 
		La_Quinta_Inns_data[p,4] = Phone
		La_Quinta_Inns_data[p,5] = Fax
		
		# Latitude and Latitude are retreived from the saved sub html pages. 
		# Selector Gadet was not helpful in retrieving these details.
		
		d1 = html_attr(html_nodes(sub_page,".ppMap img"), "src")
		
		details_lat_long = unlist(str_match_all(d1,".gif\\|([0-9\\.\\-]+),([0-9\\.\\-]+)&"))
		Latitude = details_lat_long[2]
		Longitude = details_lat_long[3]
		
		La_Quinta_Inns_data[p,6] = Latitude
		La_Quinta_Inns_data[p,7] = Longitude
	
		sub_page_amenities_html_string = html_text(html_nodes(sub_page, ".hotelFeatureList ul"))
		details_amenities <- unlist(strsplit(sub_page_amenities_html_string,'\r\n'))
	
		Floors = gsub("Floors:","",gsub(" ","",details_amenities[1]))
		Rooms = gsub("Rooms:","",gsub(" ","",details_amenities[4]))
		Suites = gsub("Suites:","",gsub(" ","",details_amenities[7]))
		
		La_Quinta_Inns_data[p,8] = as.numeric(Floors)
		La_Quinta_Inns_data[p,9] = as.numeric(Rooms)
		La_Quinta_Inns_data[p,10] = as.numeric(Suites)
	
		p = p + 1
	}

colnames(La_Quinta_Inns_data)=c("SerialNum","Inn Name","Address","Phone","Fax","Latitude","Longitude","Floors","Rooms","Suites")
save(La_Quinta_Inns_data,file="LaQuinta/Inns_Pages/La_Quinta_Inns_Data.Rdata")
