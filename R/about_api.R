library(XML)
library(RCurl)

api_key <- "dNPaIgkwOZ7owR99PJO%2BFsKmOF%2BletE5UwU%2FRfzki60YiHvBLwgygiuuVzXq63d2R7fuflBfpXk6rWMBLrlYRw%3D%3D"

get <- getForm(paste0("http://ws.bus.go.kr/api/rest/pathinfo/getPathInfoBySubway?ServiceKey=",api_key,
"&startX=126.83948388112836&startY=37.558210971753226&endX=127.01460762172958&endY=37.57250"))
xmlToDataFrame(get)

station <- "%EA%B4%91%ED%99%94%EB%AC%B8"


url <- paste0("http://ws.bus.go.kr/api/rest/pathinfo/getLocationInfo?ServiceKey=",
       api_key,"&stSrch=",station)

s_u <- xmlToList(url)

s_u$msgBody

data.frame(matrix(unlist(s_u$msgBody), nrow=13, byrow=T),stringsAsFactors=FALSE)
