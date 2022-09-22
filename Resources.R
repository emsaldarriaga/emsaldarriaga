#Misc Functions####
do.call(fun, element) #fun=function
city_priors = do.call(cbind, mget(paste0("cpr_",prop_priors)))
city_priors =do.call(cbind,mget(ls(pattern = "cpr_"))) #mget to gather multiple elements


CapStr <- function(y) { #to  capitalize only first letter
  c <- strsplit(y, " ")[[1]]
  paste(toupper(substring(c, 1,1)), substring(c, 2),
        sep="", collapse=" ")
}
capitalize_str <- function(charcter_string){
  sapply(charcter_string, CapStr)
}

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

deviates = 
  function(n,mu,sd){
    set.seed(1234567)
    i  = rnorm(n,mu,sd)
    i = ceiling(i)  
    return(i)
  }
deviates = Vectorize(deviates)

copy.table <- function(x,row.names=FALSE,col.names=TRUE,...) { #row.names=F as default, change while using
  #copy table in a format excel can understand
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

copy.table(table2)

rm(list = setdiff(ls(), lsf.str())) # Removes all objects except functions
rm(list=ls(pattern="temp")) #Removes with a pattern

find.java <- function() {
  for (root in c("HLM", "HCU")) for (key in c("Software\\JavaSoft\\Java Runtime Environment", 
                                              "Software\\JavaSoft\\Java Development Kit")) {
    hive <- try(utils::readRegistry(key, root, 2), 
                silent = TRUE)
    if (!inherits(hive, "try-error")) 
      return(hive)
  }
  hive
} #to find Java home path; once ID'd use it to declare in sys.env and the call rjava
Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jre1.8.0_301")

#Define new envir
wer = new.env()
##load data to new envir to not pollute rest of main envir
wer = local({load("OldRdata/05122021.Rdata"); environment()})
wer$object #to call an object

#Handling errors with tryCatch(); source: https://stackoverflow.com/questions/12193779/how-to-write-trycatch-in-r
ss = runif(6, -1,1)

func = function(x){
  out <- tryCatch(
    {
      d = sqrt(ss[x])*i
    },
    error=function(cond) {
      message(cond)
      return(NA)
    },
    warning=function(cond) {
      #message(cond)
      return(NA)
    }
  )    
  return(out)
}

#Check for the existence of a URL. Source: https://stackoverflow.com/questions/52911812/check-if-url-exists-in-r
url_exists <- function(x, non_2xx_return_value = FALSE, quiet = FALSE,...) {
  
  suppressPackageStartupMessages({
    require("httr", quietly = FALSE, warn.conflicts = FALSE)
  })
  
  # you don't need thse two functions if you're alread using `purrr`
  # but `purrr` is a heavyweight compiled pacakge that introduces
  # many other "tidyverse" dependencies and this doesnt.
  
  capture_error <- function(code, otherwise = NULL, quiet = TRUE) {
    tryCatch(
      list(result = code, error = NULL),
      error = function(e) {
        if (!quiet)
          message("Error: ", e$message)
        
        list(result = otherwise, error = e)
      },
      interrupt = function(e) {
        stop("Terminated by user", call. = FALSE)
      }
    )
  }
  
  safely <- function(.f, otherwise = NULL, quiet = TRUE) {
    function(...) capture_error(.f(...), otherwise, quiet)
  }
  
  sHEAD <- safely(httr::HEAD)
  sGET <- safely(httr::GET)
  
  # Try HEAD first since it's lightweight
  res <- sHEAD(x, ...)
  
  if (is.null(res$result) || 
      ((httr::status_code(res$result) %/% 200) != 1)) {
    
    res <- sGET(x, ...)
    
    if (is.null(res$result)) return(NA) # or whatever you want to return on "hard" errors
    
    if (((httr::status_code(res$result) %/% 200) != 1)) {
      if (!quiet) warning(sprintf("Requests for [%s] responded but without an HTTP status code in the 200-299 range", x))
      return(non_2xx_return_value)
    }
    
    return(TRUE)
    
  } else {
    return(TRUE)
  }
  
}

#Alternative to URL exsists; source; https://stackoverflow.com/questions/7012796/ping-a-website-in-r
ping <- function(x, stderr = FALSE, stdout = FALSE, ...){
  pingvec <- system2("ping", x,
                     stderr = FALSE,
                     stdout = FALSE,...)
  if (pingvec == 0) TRUE else FALSE
}

#404 error != not exists. The url could 'exist' but yields a "not found" error. The options above won't help
#Determine 404 error: Source: https://stackoverflow.com/questions/23139357/how-to-determine-if-a-url-object-returns-404-not-found
##Used: "Porject/VaxHesitancy/GovRaceResults.R"
ur = httr::http_status(httr::GET("url"))
ur$category == "success" #when the url yields reuslts, and "Client error" otherwise


#Scrap data from web####
#Source of nuances with html_table: "DSSides/MingWage.R"
#See: https://stackoverflow.com/questions/1395528/scraping-html-tables-into-r-data-frames-using-the-xml-package

df = url %>%   
  read_html() %>%  html_node(xpath = 'xpath') %>%  
  html_table(fill = TRUE,header=TRUE) #only if the website uses htm table element

any_version_html <- function(x){
  XML::htmlParse(x)
}
b = readLines(url)
c = paste0(b, collapse = "")
d = as.character(unlist(stri_extract_all_regex(c, '<table(.*?)/table>',
                                                omit_no_match = T, simplify = T))) #this returns table's full information in character class
e <- html_table(any_version_html(d))

df = url %>%
  read_html %>%
  html_nodes(xpath = '') %>%
  html_text() %>%
  paste0(collapse = "") %>%

df = read_html("....html") %>% #html object (saved from web page)
  html_nodes(".table-responsive") %>% 
  html_text()

hoc = url %>% read_html() %>% rvest::html_nodes('body') %>% 
  xml2::xml_find_all("//span[contains(@class,'imdb-rating')]") %>% #determine if 'span' is best or 'div'
  rvest::html_text()
#use when class can be ID'd and not clear @ID to use, specially when multiple, scattered elements present. Render with text, should produce a vector.
#SOurce: HouseofCards.R

#Scrap using class
hoc = "https://en.wikipedia.org/wiki/Perry_Adkisson" %>% 
  read_html() %>% 
  html_nodes(xpath = '//*[@class="infobox biography vcard"]') %>% 
  html_text()

#Requiring####
read = c("rvest","readxl","rlist","htmltab","httr","readr","htmltools","XML")
apis = c("tidyverse","tidycensus","tigris","censusapi","bea.R","idbr")
managedata = c("dplyr","stringr","doBy","collapse","data.table","reshape2")
spatial = c("SpatialEpi","SUMMER","stringi","sp","rgdal","rgeos","spdep","INLA","maptools","maps","geoR","sf","fields")
plot = c("ggplot2","cowplot","viridis","grid","colorspace","biscale","gridExtra","ggthemes","shape","RColorBrewer","plotly","dygraphs","scales","av","extrafont","lubridate")
misc = c("xts","kableExtra","hutils","gganimate")
bayes = c('rjags','rstan','coda', 'MCMCvis', 'MCMCpack', 'R2OpenBUGS', "bayesm", 'bayesplot',"rstanarm")

sapply(c(read,apis,managedata,plot),require, character.only=T)
rm(read,managedata,plot, bayes)

##Find installed packages.Priority on NA are installed by user
#source: https://www.r-bloggers.com/2015/06/list-of-user-installed-r-packages-and-their-versions/
ip = as.data.frame(installed.packages()[,c(1,3:4)])
ip = ip[is.na(ip$Priority),1:2,drop=FALSE]
ip
#write.table(ip,"C:/Users/irn13/Downloads/RPackages.txt",sep="\t",row.names=FALSE) #deleted

#Extracting characters####
#SOurce: "DSSides/PercentileHeight.R", "MinWage.R"
gsub(".*, ([0-9]{4})-.*", "\\1", var) #numbers with 4 digits between "," and "-"
gsub('.*SD= (.*)','\\1',var) #digits after "SD= "
#() signal what to keep, and '.*' signal portions to ignore (i.e., disregard before or after '.*')
gsub(".*, ([0-9]+)-.*", "\\1", var) #numbers with unkonwn length between "," and "-"
gsub("([0-9]+|[0-9]+.[0-9]+).*$", "\\1", var) #only integer or decimals, both with uknown number of digits
gsub(".*([0-9]{4}).*", "\\1", var) #numbers with 4 digits and nothing else 
gsub(" ([0-9])|([0-9])", "", var) #cleaning it from numbers
gsub(".*[\\$]([0-9]+|[0-9]+.[0-9]+).*$", "\\1",var) #use \\$ to scape the use of $ as function and use "$" as character

stringr::str_split(gsub("\\(([^()]+)\\)", "\\1", stringr::str_extract_all(var, "\\(([^()]+)\\)")),", ")
#extract numbers between parenthesis
sapply(df,FUN=function(x)stringr::str_extract_all(x, "\\(([^()]+)\\)")) #extract characters between parenthesis

stringi::stri_sub(var,start,end)  #extract specified number of characters
stringr::str_sub(var,start,end) #equivalent as stri_sub()
base::substr(var,start,end) #chunk of characters

stringr::str_extract_all(dff[j,]$RawDOB,"([0-9]{4})") #find all 4-digit numbers in a character

str_detect(var,"pattern",all=T) #bolean
str_locate(var,"pattern") #c(start,end)
ifelse(sapply(booklet$res, function(x) 
  any(sapply(c("pen","file","form","checklist","checklist"),
             stringr::str_detect, string = x)))==TRUE,"Office Supply",booklet$RCres)

#Flagged within vars on ACS5
dt  = tidycensus::load_variables(2018, "acs5", cache = TRUE) %>%
  mutate(f = stringr::str_detect(concept,pattern="SEX BY AGE (.* ALONE, NOT HISPANIC OR LATINO)"),
         f2 = stringr::str_detect(concept,pattern="WHITE"))

word(var,-1) #word() splits sentences by space and returns elements as specified

#Colors brewing and color selection####
#Ref1: https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
#Ref2: https://www.nceas.ucsb.edu/sites/default/files/2020-04/colorPaletteCheatsheet.pdf

#Off-the-shelf palettes (and costumizables): 
## Coolors: https://coolors.co/palettes/trending (beautifiul palettes ready to go)
## Paletton: http://paletton.com/#uid=14y0u0kcufs7lpmatkui3cxnxae (create sequential palettes varying shades)

RColorBrewer::display.brewer.pal(8, "Accent")
RColorBrewer::display.brewer.all(n=NULL, type="all", select=NULL, exact.n=TRUE, 
                   colorblindFriendly=T) #show ALL palettes
color_palette <- colorRampPalette(rev(RColorBrewer::brewer.pal(9, "BrBG")))(9)[c(8:6,4,2,1)]
scales::show_col(color_palette)
viridis(n=number,option = "B") #"magma"/"A", "inferno"/"B","plasma"/"C", "viridis"/"D", default "cividis"/"E".

scales::show_col(colorspace::rainbow_hcl(n=16)) #more flexbility (higher n) than Rcolorbrewer

hcl.pals(type = NULL) #display all color palettes in grDevices, based for colorspace::rainbow_hcl; see Ref2 for more related functions 
hcl.pals("qualitative") #qualitative, sequential, diverging, divergingx
hcl.colors(7, palette = "viridis", alpha = NULL, rev = FALSE, fixup = TRUE)
scales::show_col(hcl.colors(7, palette = "Roma", alpha = NULL, rev = FALSE, fixup = TRUE))

pal = colorspace::choose_palette(); pal(n=NUM) #brews n colors (no cap) based on parameters from choose_palette.

ggplot()+...+scale_fill_brewer(palette = "OrRd") #no need to define number of cols

#Apply####
apply(.,2,function(x) as.numeric(as.character(x)))
lapply(plots, ggplotGrob)
lapply(df[index], function(x) as.numeric(as.character(x)))
sapply(element, fun, character.only =T)
sapply(strsplit(ly, " "), length) #word count on string
ddply(flights, c("var1",...,"varn"), function(x) count(x$var)) #to apply a function to a subset of of a data frame

#Misc####
left_join(x, y, by=c("a"="b")) #when using with pipes, no need for x

doBy::summaryBy(var1+var2+...~by1+by2..., data=df, FUN=function(x) 
  quantile(x, probs = 0.025),keep.names=TRUE, na.rm=T) #sum can be substituted to any function

select(-var); select(-c(var,var,var)); select(var,var,var) #with pipes
setNames(c('V1','V2')) #with pipes; set for ALL cols

quantsf=formatC(quants, format = "d", big.mark = ",", big.interval = 3) #format

dplyr::case_when() #for multiple if-conditions, susbtitute of nesting. https://dplyr.tidyverse.org/reference/case_when.html

#Fonts: https://stackoverflow.com/questions/34522732/changing-fonts-in-ggplot2; https://statisticaloddsandends.wordpress.com/2021/07/08/using-different-fonts-with-ggplot2/
library(extrafont);font_import();loadfonts(device = "win")
library(showtext);font_add_google("Montserrat", "Montserrat");font_add_google("Roboto", "Roboto")
font_families() #all font families loaded
font.files() #all fonts available

#Resize image
x <- EBImage::readImage("../../../../fig.jpg")
dim(x)[1:2]
y <- resize(x, dim(x)[1]*3)
EBImage::writeImage(y, "../../../../path.png", quality = 100) #alternative: *.jpeg, *.jpg

#Formatting
#To create a col containing cof (95%CI) with 2 decimal digits
dt$`HR (95% CI)` <- ifelse(is.na(dt$se), "",
                           sprintf("%.2f (%.2f to %.2f)",
                                   dt$est, dt$low, dt$hi))

#Reshape####
tidyr::gather(employed,Quarter,Value,c(2:5))#convert to long format; source: "DSSides/LabourForce.R"
data.table::melt(data.table::setDT(df), 
                 id=1:2, measure=patterns("start", "end"),
                 value.name=c("start", "end"), variable.name="term") # "DSSides/LabourForce.R"

ct = dt %>%  #rows to cols
  gather(variable, value, -ID) %>% #keep variable and value
  spread(ID, value)
#Source: https://stackoverflow.com/questions/28680994/converting-rows-into-columns-and-columns-into-rows-using-r

#GGplot####
#See DSSides/FamilyArrengements.R (fig2.png output) for a good example on usage of annotation, margins, and size control of each element in the plot. Posted https://www.reddit.com/user/HitchHux/comments/t0kv37/family_arrangement_plot_update_realized_the/

##Annotation####
t= textGrob(expression(bold("estatement")),x=unit(0.9,"npc"), #higher, closer to right side. 0.5 center
         y=unit(-0.16,"npc"), #lower, closer to bottom
         gp = gpar(fontsize=18,col=col[1])) #col can vectorize, can use values
annotation_custom(grobTree(t))

annotation_custom(tableGrob(t), xmin=2010,xmax=2020,ymin=0.35, ymax=0.65) #grob specific for tables

annotate("rect", xmin = 1981, xmax = 1988.75, ymin = 65, ymax = 120,
         alpha = .1, fill="tomato1") #rectangle in plot

annotate("text", x = 1985, y = 95, label = "R. Reagan\n1st Term: +6.4M\n2nd Term: +8.1M",size=7, alpha=.8) #text in plot

geom_ribbon(aes(ymin=X0.P2.5, ymax=X0.P97.5), fill="brown3", alpha=0.2) #CI type lines

#Different fonts
expression(""*bold()*""*italic()); bquote() #works exclaty the same way
mdthemes::md_theme_<option> #is another option, 
#the syntaxis is like markdown with italics *SS* and bold **fff**
  

##Spacing####
scale_x/y_cont/disc/man(breaks = c(),labels = c(), expand = c(1E-4, 1E-4))
theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
      panel.spacing = unit(0, "cm"),
      plot.margin = unit(c(1, 2, 5, 1), "lines"))

##Saving####
png("path", width = 1800, height =1206, res = 108)
print(plot)
dev.off()

ggsave(file="path", plot=plot,width =20, height =4290/320,units="in",
       limitsize = F) #dpi of retina=320

##export a table to image
gridExtra::grid.table(gelman.diag(post_sc_11_l_s)$psrf %>% round(., 3))


##save multiple graphs
png("Images/gr_11_I%d.png", width = 1280, height = 720, res = 108) 
MCMCtrace(post_sc_11_l_s, params=c("amu","b"), pdf=F, Rhat = TRUE)
dev.off()

##Combining tables and plots####
##Sources: "DSSides/IncomeInequality.R"
plot_grid(p1,p2,p3,nrow=3,ncol=1, greedy = T, align = "hv");dev.new(width= 60, height=(4290/320)*3 , unit="in")

ggdraw() + draw_plot(g_map, 0, 0, 1, 1) +  draw_plot(g_legend, 0.8, 0.2, 0.2, 0.2)

ggplot() + ... + annotation_custom(grob = t, xmin=1.5e5,xmax=2e5,ymin=3e-5,ymax = 6e-5) 
#xmin, etc, define position in plot
t = tableGrob(d %>% mutate())

##Time series####
##Source:"DSSides/HeadGender.R"
ggplot(df, aes(x=var)) + geom_line(aes(y = y), color = "",size=1.3)
dygraphs::dygraph(df) %>%  dySeries(c("series1", "series2",...)) 

##Biscale####
#Source: "DSSides/USIncomeInequality.R"
df1 <- bi_class(df, x = x, y = x, style = "quantile", dim = 3) #creates a variable bi_class
ggplot()+ geom_attribute(aes(fill=bi_class))+...+ bi_scale_fill(pal = "DkViolet", dim = 3) 
g_legend =  bi_legend(pal = "DkViolet",dim = 3,xlab = "lab",ylab = "lab ",size = 12)

##World polygons
iso <-rgdal::readOGR(dsn = "../Make a Video/countries_shp", layer="countries") %>%
  as.data.frame(.@data) %>% select(c(ISO3,ISO2))
#map (arcgis based) is too heavy but it contains ISO3, ISO2, Name

worldMap <- rworldmap::getMap()
mapworld_df <- ggplot2::fortify( worldMap )
ggplot() + 
  geom_polygon(data= mapworld_df, aes(long,lat, group=group), fill="gray30") #see what happens if 'group' is not declared

map <- sf::st_read(system.file("shapes/world.gpkg", package="spData")) #sf emelemnt, plot geom_sf

#Flow maps, using actual geo maps
#https://gist.github.com/rafapereirabr/9a36c2e5ff04aa285fa3

##Census API####
#SOurce: https://cran.r-project.org/web/packages/censusapi/vignettes/getting-started.html
# USage: https://walker-data.com/tidycensus/articles/basic-usage.html
# https://api.census.gov/data/2018/acs/acs5/variables.html
# See PopPyramid.R for reference on how to use the Census API.

apis = View(censusapi::listCensusApis())

#tidycensus::
census_key= "a4163378432ff8af238a7e27d2e92e33602138c7"
census_api_key(census_key, install = T) #access the Census
readRenviron("~/.Renviron") #run to use now and avoid restarting 

fips_ref = tidycensus::fips_codes %>% as_tibble() %>% 
          dplyr::select(state_name, state_code, county, county_code) %>% 
          rename(state = state_name, state_fips = state_code, county_fips = county_code) %>% 
          mutate(county = gsub(" County", "", county))

county_laea %>%  #County geometry with Alaska and Hawaii shifted and re-scaled. GeoID & geometry
   
flag = fips[str_detect("Maricopa|Alameda|Orange",fips$County)==TRUE,] %>% select(GEOID) %>%
          unlist() %>% as.character()

flagca = flag[str_detect("California",flag$State)==TRUE,] %>% select(GEOID) %>%
  mutate(GEOID = substr(GEOID,3,5)) %>%  unlist() %>% as.character()

#Usage example: See Dissertation/PredictionMolel.R
tidycensus::get_acs(geography   = "county",variables   = c("B01001_001E"),
                    year= 2018,geometry = FALSE, 
                    cache_table = TRUE,
                    state = "CA",
                    county = flagca|flag)

#International Data Base
Sys.setenv(CENSUS_KEY="a4163378432ff8af238a7e27d2e92e33602138c7")
readRenviron("~/.Renviron")
Sys.getenv("CENSUS_KEY")

##CDC Api: https://data.cdc.gov/Case-Surveillance/COVID-19-Case-Surveillance-Public-Use-Data/vbim-akqf
df <- RSocrata::read.socrata(
  "https://data.cdc.gov/resource/vbim-akqf.json",
  app_token = "kteU2pPIsi2t8TbtmB45OaymX",
  email     = "emsb@uw.edu",
  password  = "93qzMNdeB!Es!Gx"
)

##Plotyl####
#add a text besides the aes
geom_point(aes(x=Diff_QALys,y=(Diff_Costs)/1e6,color = PSAdj_cat,
               text=paste(Zip_code,PSAdjustment,sep="-")),size=4)

##Bureau Economic Analysis API
##see DSSides/CountiesGDP.R Couldn't make it wokr

#Spatial Imaging####
install.packages("ggmap")
library(ggmap)
ggmap::register_google(key="AIzaSyACo9VfHKl73xfgjitGEecXGiZ5nfmlYe8")
#Source: https://rpubs.com/nickbearman/r-google-map-making, https://cran.r-project.org/web/packages/ggmap/readme/README.html, https://journal.r-project.org/archive/2013-1/kahle-wickham.pdf

#Create breaks####
#SOurce: "DSSides/LivingWage.R"
nbreaks <- num #bind=breaks-1
quants <- quantile(col,
                   seq(0, 1, length.out = nbreaks),na.rm = T)
# quants[length(quants)] <- ceiling(quants[length(quants)])
# quants[1] <- ifelse(floor(quants[1])<0,0,floor(quants[1])<0)
# quants <- round(quants)
quantsf=formatC(quants, format = "d", big.mark = ",", big.interval = 3) #format

df$col_cut <- cut(df$col,breaks=nums, #replace nums with 'quants'
                      labels=c('lab1','lab2'),na.rm = T,include.lowest = T, right = T)

#Mutate####
#Source: https://dplyr.tidyverse.org/reference/mutate_all.html
mutate_at(colnames(data[,2:14]),list(r=rescale)) #mutate_at holds characters. To create new variables use give it a name with 'list(r=FUN)', by default it puts a "_" between the old var nad the newtoken for th new ones

#Animations####
# Sources: (1) https://paldhous.github.io/ucb/2018/dataviz/week14.html
## (2) https://www.datanovia.com/en/blog/gganimate-how-to-create-plots-with-beautiful-animation-in-r/
## (3) https://www.r-bloggers.com/2018/10/the-av-package-production-quality-video-in-r/

#Loop####
for (i in 8:13){
  dd<-names(cc3b51[,i])
  assign(dd, as.numeric(gsub("[A-Z]+","", gsub("[a-z]+","", gsub('[[:space:]]',"",as.matrix(cc3b51[,i]))))))
}

#Paralleling####
#See file Dissertation/~/Portable/ for extensive work on parallel and references. In particular: ZC_analysis_parallel_v3, SandBox_Parallel and 'Ref on Parallel.txt'

#ShinyApp####
#Examples of Shiny in Healthcare: https://www.rstudio.com/blog/using-shiny-in-healthcare/
##Interface
ui <- shinyUI(fluidPage(
  titlePanel(""),
  sidebarLayout(
    sidebarPanel(
      sliderInput("var",
                  "title:",
                  min = val,max = val,value=val,timeFormat="%Y-%m-%d"),
      varSelectInput("var", "Var:", names),
      selectInput("metric", "Metric:",choices = c("1","2","3")),
      numericInput("x1", "Title", 10)
    ),
    mainPanel(
      plotOutput("plot"))
  )
))

server <- shinyServer(function(input, output) {
  output$plot <- renderPlot({
    Input<-input$var #'var' as define in UI
    #dataprep
    
    titlel = if (input$variable=="CumulativeCasesAbs") {
      "Cumulative Cases"
    } else if (input$variable=="CumulativeCasesRate") {
      "Cumulative Cases per 100,000 habitants"
    }  else if (input$variable=="NewCasesAbs") {
      "New Daily Cases"
    } else {
      "New Daily Cases per 100,000 habitants"
    }
    
    p1 = p
    p2 = 
      if (input$variable=="NewCasesAbs"|input$variable=="NewCasesRate") {
        ggplot()
      } else {
        ggplot()
      }
    cowplot::plot_grid()
  })
})

shinyApp(ui = ui, server = server)

#Loading
#Pull multiple dataset together. Only works if structure is the same. Equivalent to pull eachone by one and then rbind()
tbl <-
  list.files(path="../CORONAVIRUS/COVID_DATA/CDC/", pattern = "*.csv", full.names = T)[-2] %>% 
  purrr::map_df(~readr::read_csv(.,trim_ws = FALSE, skip = 3)) %>%
  mutate(Var = rep(list.files(path="../CORONAVIRUS/COVID_DATA/CDC/", pattern = "*.csv")[-2],each=7))

#Rmarkdown####
# Cheat sheet: https://blog.rstudio.com/2014/08/01/the-r-markdown-cheat-sheet/
#Declare either "$$\begin{aligned}" or "\begin{alig}" directly. The latter automatically numerates the equations and is a self-contained math environment, while "\begin{aligned}" needs to be placed inside a defined envir such as "$$...$$". Ref: https://stackoverflow.com/questions/26049762/erroneous-nesting-of-equation-structures-in-using-beginalign-in-a-multi-l
#Declare multiple packages from a .sty file containing all \usepackage{*} desired. Ref: https://tex.stackexchange.com/questions/171711/how-to-include-latex-package-in-r-markdown. See "CSSS564/AnBiblio_ES.Rmd" for application.
#Citation: https://rstudio.github.io/visual-markdown-editing/?_ga=2.60836844.1985613930.1623802517-814765358.1615837390#/citations?id=inserting-citations (visual rmardown); https://bookdown.org/yihui/rmarkdown-cookbook/bibliography.html
##With vidual rmardown we can add references that are not even in zotero, but we ALWAYS need to declare a ".bib" file so any reference can be appended to it. Same, we always need a ".cls" files to declare the style of the references; zotero style repo: https://www.zotero.org/styles

#Table with an histogram: https://stackoverflow.com/questions/66305686/rmarkdown-doesnt-recognize-the-inline-in-kable-styling-command

#Differences between LuaTeX, LaTex: https://tex.stackexchange.com/questions/36/differences-between-luatex-context-and-xetex/72#72

#Rmakrdown theme gallery: https://www.datadreaming.org/post/r-markdown-theme-gallery/

#Tables with kable, mostly for HTML: https://haozhu233.github.io/kableExtra/awesome_table_in_html.html

#Table by (for pdf and html): https://cran.r-project.org/web/packages/arsenal/vignettes/tableby.html
##ref("Costing_ATTACH_Draft1.Rmd")
mycontrols  <- arsenal::tableby.control(test=TRUE, total=TRUE,
                                        numeric.test="kwt", cat.test="chisq",
                                        numeric.stats=c("mean", "sd"),
                                        cat.stats=c("countpct"),
                                        stats.labels=list(mean='Mean', SD='SD'))
table_cost = arsenal::tableby(by_what~var+var, strata=var, 
                              data=cost_uniq, control=mycontrols)

#Modify the CSS to change how the output looks
##Example for the caption of an image: https://stackoverflow.com/questions/54983349/figure-caption-and-css-stylesheet-for-rmarkdown
##Source: Resumegit/EMSB_Resume.Rmd

#Rmarkdown manuscripts: https://stirlingcodingclub.github.io/Manuscripts_in_Rmarkdown/Rmarkdown_notes.html tried for ATTACH, the referneces doc didn't change anything