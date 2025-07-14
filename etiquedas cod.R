library(sysfonts)
font_add_google("Tillana", "Tillana")
font_add_google("Permanent Marker", "Permanent Marker")
showtext::showtext_auto()


source("https://inkaverse.com/setup.r")
url <- "https://docs.google.com/spreadsheets/d/1NikLIdpI47PiaocJsWDXs_g89ovNSLls3krVXEkIu3s/edit?usp=sharing"
gs <- as_sheets_id(url)

fb <- gs %>% 
  range_read("fb")

font <- c("Permanent Marker", "Tillana", "Courgette")
huito_fonts(font)

label <- fb %>% 
  mutate(color = case_when(tetrazolio %in% "0.5" ~ "blue",tetrazolio %in% "1" ~ "red",tiempo %in% "12" ~ "blue",tiempo %in% "24" ~ "green" )) %>%
  label_layout(
    size = c(5, 8),
    border_color = "midnightblue"
  ) %>%
  include_image(
    value = "https://upload.wikimedia.org/wikipedia/commons/thumb/9/95/Logo-2016-solo-ok.png/250px-Logo-2016-solo-ok.png"
    , size = c(1.8,1.8)
    , position = c(1,6.9)
  ) %>% 
  include_barcode(
    value = "qrcode"
    , size = c(5, 5)
    , position = c(2.5,3.5)
  ) %>% 
  include_image(
    value = "https://huito.inkaverse.com/img/scale.pdf"
    , size = c(5,1)
    , position = c(2.3,0.5)
  )%>% 
  include_image(
    value = "https://st2.depositphotos.com/10614052/44305/i/450/depositphotos_443051008-stock-photo-beautiful-sunflower-seeds-white-background.jpg"
    , size = c(2,2)
    , position = c(4.2,6.7)
  )%>% 
  include_text(value = "Girasol"
               , position = c(2.4,7)
               , size = 8
               , color = "black"
               , opts = list(hjust = 0)
               , font[1]
  ) %>% 
  include_text(value = "Helianthus annuus"
               , position = c(2,7.4)
               , size = 10
               , color = "black"
               , opts = list(hjust = 0)
               , font[2]
  )%>%
  include_text(value = "plots"
               , position = c(2.4,6.5)
               , size = 12
               , color = "black"
               , opts = list(hjust = 0)
  ) 

label %>% label_print(mode = "preview")

label %>%
  label_print(mode = "complete", filename = "Girasol", nlabels = 24)
