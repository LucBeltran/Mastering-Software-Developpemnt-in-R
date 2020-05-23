##### Load Library

library(dplyr)
library(tidyr)


##### LongitudionalData objects
make_LD <- function(df) {
  df <- df %>% nest(data = c(visit, room, value, timepoint))
  structure(df, class = c("LongitudinalData"))
}

print.LongitudinalData <- function(x) {
  cat("Longitudinal dataset with", length(x[["id"]]), "subjects")
}

subject <- function(df, id) UseMethod("subject")

subject.LongitudinalData <- function(df, id) {
  ind <- which(df[["id"]] == id)
  if (length(ind) == 0)
    return(NULL)
  structure(list(id = id, data = df[["data"]][[ind]]), class = "Subject")
}



# methods for Subject objects
print.Subject <- function(x) {
  cat("Subject ID:", x[["id"]])
}

summary.Subject <- function(object) {
  output <- object[["data"]] %>% 
    group_by(visit, room) %>%
    summarise(value = mean(value)) %>% 
    spread(room, value) %>% 
    as.data.frame
  structure(list(id = object[["id"]],
                 output = output))
}


visit <- function(subject, visit_nb) UseMethod("visit")

visit.Subject <- function(subject, visit_nb) {
  data <- subject[["data"]] %>% 
    filter(visit == visit_nb) %>% 
    select(-visit)
  structure(list(id = subject[["id"]],
                 visit_nb = visit_nb,
                 data = data), class = "Visit")
}


# methods for Visit objects 

room <- function(visit, room_n) UseMethod("room")

room.Visit <- function(visit, room_n) {
  data <- visit[["data"]] %>% 
    filter(room == room_n) %>% 
    select(-room)
  structure(list(id = visit[["id"]],
                 visit_num = visit[["visit_nb"]],
                 room = room_n,
                 data = data), class = "Room")
}

# methods for Room objects
print.Room <- function(x) {
  cat("ID:", x[["id"]], "\n")
  cat("Visit:", x[["visit_nb"]], "\n")
  cat("Room:", x[["room"]])
  invisible(x)
}

summary.Room <- function(object) {
  output <- summary(object[["data"]][["value"]])
  structure(list(id = object[["id"]],
                 output = output))
}

# methods for Summary objects 
print.Summary <- function(x) {
  cat("ID:", x[[1]], "\n")
  print(x[[2]])
}




######### Generate output file

sink('opp_output.txt')

data <- read_csv("data/MIE.csv")
x <- make_LD(data)
print(class(x))
print(x)

## Subject 10 doesn't exist
out <- subject(x, 10)
print(out)

out <- subject(x, 14)
print(out)

out <- subject(x, 54) %>% summary
print(out)

out <- subject(x, 14) %>% summary
print(out)

out <- subject(x, 44) %>% visit(0) %>% room("bedroom")
print(out)

## Show a summary of the pollutant values
out <- subject(x, 44) %>% visit(0) %>% room("bedroom") %>% summary
print(out)

out <- subject(x, 44) %>% visit(1) %>% room("living room") %>% summary
print(out)


sink(type = "message")
sink()
