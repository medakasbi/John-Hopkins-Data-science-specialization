
best <- function(state, outcome){
    data = read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
    
    
    if(!any(is.element(unique(my_data[["state"]]), state))){
        return("invalid state")
    }
    
    if(outcome == "heart attack"){
        out = 11
    }else if(outcome == "heart failure"){
        out = 17
    }else if (outcome == "pneumonia"){
        out = 23
    }else{
        return("Invalid outcome")}
    
    my_data <- data[c(2, 7, out)]
    names(my_data) = c("hospital", "state", outcome)
    #my_data = na.omit(my_data)
    t = tapply(my_data[[outcome]], my_data$state, min, na.rm = TRUE, simplify = FALSE)
    t[state]
    best_Hs = my_data$hospital[which(my_data[[outcome]] == t[state] & my_data$state == state)]
    sort(best_Hs)[1]
    
    
}
