
rankhospital <- function(state, outcome, num){
    data = read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
    
    # Check if state is valid
    if(!any(is.element(unique(data[["State"]]), state))){
        return("invalid state")
    }
    
    # Check if outcome is valid
    if(outcome == "heart attack"){
        out = 11
    }else if(outcome == "heart failure"){
        out = 17
    }else if (outcome == "pneumonia"){
        out = 23
    }else{
        ("Invalid outcome")
    }
    # subset initial data 
    my_data <- data[c(2, 7, out)]
    # rename columns 
    names(my_data) = c("hospital", "state", outcome)
    # reemove NA values
    my_data = na.omit(my_data)
    
    ## Order by state then outcome then hospital name
    my_data = my_data[order( my_data[, 2], my_data[, 3], my_data[, 1]),]
    #Split by state
    new_data = my_data[which(my_data[,2] == state),]
    # get the result
    if(num == "best"){
        num = 1
    }else if(num == "worst"){
        num = nrow(new_data)
    }else{num = num}
    
    new_data[num, 1]
      
}
