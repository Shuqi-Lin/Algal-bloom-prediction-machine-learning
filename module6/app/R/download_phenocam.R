download_phenocam <- function(url) {
  require(rvest)
  web <- "https://phenocam.sr.unh.edu"

  imgsrc <- read_html(url) %>%
    html_node(xpath = '//*/img') %>%
    html_attr('src')
  # imgsrc
  
  destfile <- file.path("www",basename(imgsrc))
  
  # 
  download.file(paste0(web, imgsrc), 
                destfile = destfile, mode = "wb", quiet = TRUE)
  
  if(file.exists(destfile)) {
    message("Phenocam downloaded!")
  } else {
    "Phenocam download failed!"
  }
  return(destfile)
}

