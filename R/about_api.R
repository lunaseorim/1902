library(XML)
library(RCurl)
library(dplyr)

api_key <- "dNPaIgkwOZ7owR99PJO%2BFsKmOF%2BletE5UwU%2FRfzki60YiHvBLwgygiuuVzXq63d2R7fuflBfpXk6rWMBLrlYRw%3D%3D"

url = "http://openapi.gbis.go.kr/ws/rest/busrouteservice/station?serviceKey="
j1 <- data.frame()
j2 <- data.frame()
s_u_fine <- data.frame()
for (i in 1:104) {
  get <- getForm(paste0(url,api_key,"&routeId=", routelist$노선id[i]))
  s_u <- xmlToList(get)
  j2 <- data.frame()
  for (j in 1:length(s_u$msgBody)) {
    j1[j,1] <- ifelse(length(s_u$msgBody[j]$busRouteStationList$mobileNo)>0,
    s_u$msgBody[j]$busRouteStationList$mobileNo,NA)
    j2 <- rbind(data.frame(),j1)
  }
  s_u_fine <- rbind(s_u_fine,j2)
  
  cat(i,"/104","\n")
}

okfine2 <- data.frame(table(s_u_fine))

for (i in 1:104) {
  get <- getForm(paste0(url,api_key,"&routeId=", routelist$노선id[i]))
  s_u <- xmlToList(get)
  k2 <- data.frame(routelist$노선id[i],length(s_u$msgBody))
  okfine1 <- rbind(okfine1,k2)
  cat(i,"/104","\n")
}

get <- getForm(paste0(url,api_key,"&routeId=", routelist$노선id[1]))
s_u <- xmlToList(get)

s_u$msgBody[2]$busRouteStationList$stationId

