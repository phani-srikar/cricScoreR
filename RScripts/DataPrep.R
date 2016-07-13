#To scrap dat from cricsheet website.

#Below are sample urls that are used.
# url <- "http://stats.espncricinfo.com/ci/engine/stats/index.html?class=20;template=results;type=allround;view=ground"
# url<-"http://stats.espncricinfo.com/ci/engine/stats/index.html?class=1;home_or_away=2;opposition=25;team=5;template=results;type=fielding"
# url <- "http://stats.espncricinfo.com/ci/engine/stats/index.html?class=1;template=results;type=batting"

getData <- function(url){
  library(XML)
  library(rvest)
  resultdf <- NULL
  while(1){
    #browser()
    parsedPage <- read_html(url)
    resultdf <- rbind(resultdf,parsedPage %>% html_nodes("table") %>% .[[3]] %>% html_table())
    pageLinkNode <- html_nodes(parsedPage,xpath = "//*[@id='ciHomeContentlhs']/div[3]/table[2]/tr[1]/td[5]/a[1][@class='PaginationLink']")
    url <- html_attr(pageLinkNode,"href")
    if(length(url)<1) break
    else url <- paste0("http://stats.espncricinfo.com",url)
  }
  return(resultdf)
}

#To extract data

url1 ="http://stats.espncricinfo.com/ci/engine/stats/index.html?class=3;template=results;type=batting"
#getData function taken from cricdataR script.
ans1 = getData(url1)
write.csv(ans1,"C:/Users/admin/Downloads/Use_Case_Dhruv/MetaDataBatsmen.csv",append = TRUE,row.names = FALSE)
#Data for International T20 Bowlers.
url2 = "http://stats.espncricinfo.com/ci/engine/stats/index.html?class=3;template=results;type=bowling"
ans2 = getData(url2)
write.csv(ans2,"C:/Users/admin/Downloads/Use_Case_Dhruv/MetaDataBowlers.csv",append = FALSE,row.names = FALSE)

#For refining data in order to remove initial details of a match in all csv files.

setwd("C:/Users/admin/Downloads/Use_Case_Dhruv/t20_csv")
files <- list.files(pattern=".csv$")
t<-lapply(files, function(x){
  filex <- readLines(x)
  filex <- as.character(sapply(filex,function(y)if(!(grepl("info",y)||grepl("version",y)))return(y)))
  filex <- filex[filex!="NULL"]
  writeLines(filex,paste0("C://Users/admin/Downloads/Use_Case_Dhruv/Refined_t20data/",x))
})
testfiles <- sample(files,round(0.2*length(files)))
trainfiles <- setdiff(files,testfiles)
traindf = do.call(rbind, lapply(trainfiles, function(x) read.csv(paste0("C://Users/admin/Downloads/Use_Case_Dhruv/Refined_t20data/",x), header=FALSE,stringsAsFactors = FALSE)))


#For binding all csv files into one csv file.

filelist = list.files("E:\\UseCase\\Refined_ipldata")

lapply(1:length(filelist),function(x){
  prev = read.csv(paste0("E:\\UseCase\\Refined_ipldata\\",filelist[[x]]))
  colnames(prev) = c("Ball","Inning","Over","Team","StrikeBatsman",
                     "NonStrikeBatsman","Bowler","RunsScored","Extras","DismissalMethod",
                     "BatsmanOut")
  write.csv(prev,paste0("E:\\UseCase\\Refined_ipldata\\",filelist[[x]]),row.names = FALSE)
})

#To make it a global option.
FinalDataFrame=read.csv(paste0("E:\\UseCase\\Refined_ipldata\\",filelist[[1]]))

 for(x in 2:length(filelist)){
  temp = read.csv(paste0("E:\\UseCase\\Refined_ipldata\\",filelist[[x]]))
  FinalDataFrame = rbind(FinalDataFrame,temp)
}

write.csv(FinalDataFrame,"E:\\UseCase\\BindIPLData.csv")
