library(sysfonts)
font_add_google("Tillana", "Tillana")
font_add_google("Permanent Marker", "Permanent Marker")
showtext::showtext_auto()


source("https://inkaverse.com/setup.r")
url <- "https://docs.google.com/spreadsheets/d/131gOsbyixewZBoytk-MwNKNanxA0cxVByQX9Wn980Lw/edit?gid=1809289525#gid=1809289525"
gs <- as_sheets_id(url)

fb <- gs %>% 
  range_read("fb")

font <- c("Permanent Marker", "Tillana", "Courgette")
huito_fonts(font)

label <- fb %>% 
  mutate(color = case_when(metodo %in% "Lija" ~ "blue",metodo %in% "Acetona" ~ "red",metodo %in% "Agua caliente" ~ "blue",metodo %in% "Control" ~ "green" )) %>%
  label_layout(
    size = c(5, 6),
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
    , position = c(2.5,0.5)
  )%>% 
  include_image(
    value = "https://png.pngtree.com/png-clipart/20201208/original/pngtree-ripe-scattered-chickpeas-png-image_5573029.jpg"
    , size = c(4.5,2)
    , position = c(4.5,7)
  )%>% 
  include_text(value = "Garbanzo"
               , position = c(2.1,7.5)
               , size = 12
               , color = "black"
               , opts = list(hjust = 0)
               , font[1]
  ) %>% 
  include_text(value = "Cicer arietinum"
               , position = c(2.1, 7.1)
               , size = 8
               , color = "black"
               , opts = list(hjust = 0)
               , font[2]
  )%>%
  include_text(value = "plots"
               , position = c(2.8,6.5)
               , size = 14
               , color = "black"
               , opts = list(hjust = 0)
  ) 


label %>% label_print(mode = "preview")

label %>%
  label_print(mode = "complete", filename = "Garbanzo", nlabels = 24)