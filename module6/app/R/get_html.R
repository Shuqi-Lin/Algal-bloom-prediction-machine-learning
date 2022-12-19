require(rvest)
get_html <- function(site_id) {
  url <- "https://www.neonscience.org/field-sites/"
  myurl <- xml2::read_html(paste0(url, site_id))
  
  tst <- myurl %>% 
    html_nodes(".field-site__withSidebar-content") 
  tst2 <- html_children(tst[2])[2]
  xml2::write_html(tst2, "data/site.html")

  if(file.exists("data/site.html")) {
    return(htmltools::htmlTemplate("data/site.html"))
  } else {
    warning("HTML not downloaded")
  }
}
