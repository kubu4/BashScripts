##Originally created by Sean Bennett at the Univ. of Washington

## Function CQagg takes arguments dataframe (data construct containing your data), 
## label.column(column index containing sample label (std1, unkn-1, etc)), and cq.column(column where cq values are located).
## A True/False flag filename allows user to chose either outputting to a .csv file, requesting the filename wanted,
## or just a generic return of the data frame.

## NOTES ##
## Current iteration does not test for/remove NaNs, it just transfers them over as they come. Can likely be added in to
## convert them to NAs if you like. The standard curves produce a large number of NAs in columns that correspond to the
## 3rd+ replicate of the experimentals. If that's an issue, I can probably coerce a ragged array 
## (array that does not have constant length in a dimension) but I don't know how that would output.

CQagg <- function(dataframe, label.column = 5 , cq.column = 8, filename = FALSE)   {
  
  temp.dataframe <- cbind(dataframe[ , 1:(cq.column - 1)], dataframe[,(cq.column + 1) : 
                                                                       length(dataframe)], dataframe[,cq.column]) # Rearranges supplied dataframe to put CQ values in last column
  
  temp.dataframe <- temp.dataframe[order(temp.dataframe$Content),] ## Orders dataframe
  
  output.df2 <- temp.dataframe[1,] ## Initilizes the return data frame with the first row of qPCR information
  rowcounter <- 2 # Index for keeping track of the row of the new data frame currently being operated on (Sample label)
  columncounter <- length(temp.dataframe) + 1 ## Index for keeping track of column of new data frame being operated on (individual cq values)
  
  ## Main for loop that iterates through the supplied data frame one row at a time.
  for(i in 2:length(temp.dataframe[,1]))  {
    
    ## Main logical test, checking if the current sample label is the same as the previous sample label. If so, appends
    ## current Cq value to the end of the return data frame and increments columncounter by 1
    if(temp.dataframe[i,label.column] == temp.dataframe[i - 1, label.column])   {
      output.df2[(rowcounter - 1),columncounter] <- temp.dataframe[i,length(dataframe)]
      columncounter <- columncounter + 1
      
      ## Fork for failed if tests, creates a new row in the return data frame with the new sample label, increments rowcounter
      ## by 1 and resets columncounter to it's intial value. 
    } else {
      output.df2[rowcounter,] <- temp.dataframe[i,]
      rowcounter <- rowcounter + 1
      columncounter <- length(temp.dataframe) + 1
    }
  }
  colnames(output.df2)[length(dataframe):length(output.df2)] <- paste('CQ', 1:(length(output.df2) - length(dataframe) + 1), sep = "")
  # The above line renames newly created columns in the format CQ* where * is the replicate number ex. CQ1, CQ2, etc.
  if(filename == FALSE) { # Tests if the filename argument is passed as TRUE/FALSE, if false, then just returns the output to a variable
    return(output.df2)
  } else # Else fork, when filename == TRUE, outputs the dataframe to a csv file with a user supplied filename
    filename <- readline("Input file name for saving, include .csv in the filename (ex. test.csv):  ") #Queries user for filename
  write.csv(output.df2, filename )
}
