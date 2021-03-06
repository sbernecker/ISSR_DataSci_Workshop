#' Exercise 2-- in this exercise you will be going for speed and clarity. You 
#' will be taking the block of code below and wrapping it into several functions
#' and parallelizing everything to make it run faster. This assignment will be
#' more similar to what you might actually encounter in the wild. (notice how I 
#' use the "#'" symbol at the beginning of each line? In RStudio this will 
#' automatically continue the block comment --- super handy!)


#' Here is the start of the code you will be modifying. It reads in 11 years of
#' cosponosrship data and turns it into sociomatricies suitable for network
#' analysis. You will want to make use of parts of this block of code -- with 
#' heavy modifications -- in the exercise

############################ START EXAMPLE CODE ################################

# set working directory
#mywd <- "~/ISSR_Data_Science_Summer_Summit_15/Data"
setwd("C:/Users/Sam/Documents/GitHub/ISSR_DataSci_Workshop/Data")

#' We are going to be looking at a dataset that comprises 11 sessions on 
#' congress and sepcifically the bills cosponsored by senators over that period
#' of time. This is a very complex data management problem.
Congresses <- 11
Bills_To_Use <- 10

#' The assign() function lets us name objects usign the paste function in R. 
#' This is good for batch reading in data or generating lots of data objects.

# lets begin by loading in some data:
print("Loading Raw Senate Cosponsorship Matrix Data")
for(i in 1:Congresses){
    cur <- 99 + i
    
    # read in the .csv files
    data_name <- paste(cur,"_senmatrix.txt", sep = "")
    temp <- read.csv(data_name, stringsAsFactors = F, header = F)
    
    # keep first Bills_To_Use bills from each congress
    temp <- temp[,1:Bills_To_Use]
    
    #name th object
    object_name <-  paste("Senate_Raw_Cosponsorship_",cur, sep = "")
    
    #assigns the raw cosponsorship data to a named variable
    assign(object_name,temp) 
}
#'SB: okay, so now we have eleven data frames with the names given in Object_name, and each data frame contains rows and columns of numerals 0, 1, and 2; each column is a bill, each row is a senator, and 1 represents the sponsor and 2 represents a cosponsor; these data frames each contain ten "columns" (variables) (bills) with between 100 and 102 "rows" (observations) (senators) each

#' The get() function performs the opposite funtion of the assign function and 
#' lets us get objects by their pasted name:

#' save a list of all the raw data which we will not care about after we have 
#' transformed it so we can remove it later
all_objects <- ls()

cat("Transforming Raw data into Cosponsorship Matricies \n")

#'okay, this creates eleven "sociomatrices," square matrices with the number of rows and columns for each senator; a column is "sponsorship" and a row is "cosponsorship" so to speak (i.e., a column represents when that senator sponsors a bill, which people cosponsor it); each cell contains a count of how many times the row senator has cosponsored the column senator's bill, so max here would be ten
for(i in 1:Congresses){
    cur <- 99 + i
    cat(paste("Currently on Congress number: ",cur,"\n", sep = ""))
    
    # get the current data object
    object_name <-  paste("Senate_Raw_Cosponsorship_",cur, sep = "")
    temp <- get(object_name)
    
    #create a sociomatrix to populate
    num_senators <- length(temp[,1])
    temp_sociomatrix <- matrix(0,ncol = num_senators, nrow = num_senators)
    
    # this is an example of nested looping 
    for(j in 1:length(temp[1,])){#for every bill
        
        #find out who the bill sponsor is (coded as a 1)
        #YOU SHOULD ACTUALLY DO THIS USING WHICH SO THAT YOU CAN GO FASTER
        for(k in 1: length(temp[,1])){ #for every Senator
            if(temp[k,j] == 1){
                sponsor <- k
            }  
        }
        
        #find all of the cosponsors
        for(k in 1: length(temp[,1])){ #for every Senator
            if(temp[k,j] == 2){
                temp_sociomatrix[sponsor,k] <- temp_sociomatrix[k,sponsor] + 1
            } 
        }
    }
    
    sociomat_name <- paste("Senate_Cosponsorship_Matrix_",cur, sep = "")
    # assigns the cosponsorship matrix to a named variable
    assign(sociomat_name,temp_sociomatrix) 
   
}


#' remove all of the raw data -- how could we do all of this without creating 
#' these objects in the first place?
#' OKAY THIS APPEARS TO BE A HINT THAT THESE OBJECTS SHOULD NOT GET CREATED
rm(list = all_objects)

#' so far we have written code which could be turned into a function. A simple
#' example might be a function that counts the total number of cosponsorships in
#' a given congress.

Total_Cosponsorships <- function(cosponsorship_matrix){
    total <- 0
    for(i in 1:length(cosponsorship_matrix[1,])){
        for(j in 1:length(cosponsorship_matrix[,1])){
            total <- total + cosponsorship_matrix[j,i]
        }
    }
    return(total)
}

#' now we have to enter the function in the console before we can use it. If you
#' are using RStudio then you will see the function pop up in your workspace.
#' try it out!
Total_Cosponsorships(Senate_Cosponsorship_Matrix_100)

#' We can use functions inside of functions as well. Note that we can set 
#' default values for a function by putting an = sign next to the arguemnt name

Cosponsorships_per_Congress <- function(start = 100, end = 110){
    num_cases <- end - start + 1
    vector <- rep(0, times = num_cases)
    for(i in 1:num_cases){
        cur <- start - 1 + i
        #' sometimes we want an update of where we are in the process
        cat(paste("Currently on Congress Number: ", cur ,"\n", sep = ""))
        #' we can also call a function from inside a function... inside a 
        #' function... inside a function
        vector[i] <- Total_Cosponsorships(get(paste(
          "Senate_Cosponsorship_Matrix_",cur, sep = "")))
    }
    return(vector)
}

#' try it out (note that you do not need to have any arguments because we have 
#' set default values for all arguments -- this can be dangerous!)
Cosp_per_Congress <- Cosponsorships_per_Congress()

#plot our work
barplot(Cosp_per_Congress,
        xlab = "Congress", 
        ylab = "Number of Cosponsorships", 
        names = c(100:110),
        col = rainbow(11))


#6.5 having some fun -- what does this function do? How does it work?
library(statnet)

par(mfrow = c(1,1))
colors <- (1:11)
years <- 100:110

netplot <- function(year, color){
    net <- as.network(get(paste("Senate_Cosponsorship_Matrix_",year, sep = "")))
    plot(net, vertex.col = color)
    if(color == 1){
        output <- degree(net)
    }else{
        output <- rbind(output,degree(net))
    }
    Sys.sleep(.2)
    return(output)
}

result <- mapply(netplot, years,colors)

apply(result,sum)

############################ END EXAMPLE CODE ##################################

#' Your Assignment: Take the code on lines 16-95 and make it faster, more 
#' flexible and less messy. You should start by filling in the helper functions
#' outlined below, followed by the top level function which will take an 
#' arbitrary number of input files of the same format as you are working with
#' and generate a list of sociomatricies out of them:

Read_In_Data <- function(filenames){
  #' you will want to loop over the vector of filenames and load each one in to
  #' a slot in the Raw_Data_List object you create before returning it. Printing
  #' out progress would be nice!
  return(Raw_Data_List)
}

Generate_Sociomatrix <- function(list_index, raw_data_list, num_bills){
  #' you only need to 
  #' 
  #' takes in a list index, extracts the right raw data object from the raw data
  #' list object, iterates over a specified number of
  #' columns (bills) and returns a square (direted) sociomatrix 
  return(Sociomatrix)
}

PreProcess_Network_Data <- function(filenames, num_bills, num_cores){
  #' read in the data an generate an internal raw data list object. Then use
  #' parallel processing (one of the three functions we discussed) to process
  #' the data and return a list containing all sociomatricies. Try different
  #' approaches and see which is fastest?
  return(Sociomatrix_List)
}

#' Once you have a working function, try speed testing different versions with 
#' different kinds of parallelization against the stock code for different 
#' values of num_bills, num_cores and for different numbers of files to work on.
#' You can use the following code to determine the elapsed time:

system.time({
  #' Your function goes here!
})

#' You may then want to stick all of your testing inside of a loop to automate 
#' it. Save your results in a dataframe and then give plotting them a try. What
#' do you observe?


################################################################################
#' Answer Code Below -- Spoiler ALERT!
################################################################################










#' get filenames
mywd <- "~/Dropbox/RA_and_Consulting_Work/ISSR_Data_Science_Summer_Summit_15/Data"
setwd(mywd)
filenames <- NULL
for(i in 100:110){
  filenames <- append(filenames, paste(i,"_senmatrix.txt", sep = ""))
}

# define the function
Read_In_Data <- function(filenames){
  Raw_Data_List <- vector(mode = "list", length = length(filenames))
  for(i in 1:length(filenames)){
    cat("Current Dataset", i,"\n")
    Raw_Data_List[[i]] <- read.csv(filenames[i], stringsAsFactors = F, header = F)
  }
  return(Raw_Data_List)
}

# define function
Generate_Sociomatrix <- function(list_index, raw_data_list, num_bills){
  #' takes in a list index, extracts the right raw data object from the raw data
  #' list object, iterates over a specified number of
  #' columns (bills) and returns a square (direted) sociomatrix 
  temp <- raw_data_list[[list_index]]
  #create a sociomatrix to populate
  num_senators <- length(temp[,1])
  Sociomatrix <- matrix(0,ncol = num_senators, nrow = num_senators)
  # this is an example of nested looping 
  for(j in 1:num_bills){#for every bill
    cat("Current Index:",list_index,"Bill:",j,"of",length(temp[1,]),"\n")
    #find out who the bill sponsor is (coded as a 1)
    for(k in 1: length(temp[,1])){ #for every Senator
      if(temp[k,j] == 1){
        sponsor <- k
      }  
    }
    #find all of the cosponsors
    for(k in 1: length(temp[,1])){ #for every Senator
      if(temp[k,j] == 2){
        Sociomatrix[sponsor,k] <- Sociomatrix[sponsor,k] + 1
      } 
    }
  }
  return(Sociomatrix)
}

PreProcess_Network_Data <- function(filenames, num_bills, num_cores){
  # num_bills = 2
  # num_cores = 2
  Raw_Data <- Read_In_Data(filenames)
  cat("Processing Data into Sociomatricies... \n")
  # Packages:
  require(doMC)
  require(foreach)
  # Register number of cores
  registerDoMC(num_cores)
  # Run analysis in parallel
  wrapper <- function(index){
    print(index)
    max_num_bills <- length(Raw_Data[[index]][1,])
    if(num_bills> max_num_bills){
      num_bills <- max_num_bills
    }
    result <- Generate_Sociomatrix(index,Raw_Data,num_bills)
    return(result)
  }
  Sociomatrix_List <- foreach(i=1:length(filenames)) %dopar% {
    result <- wrapper(i)
  }
  #' read in the data an generate an internal raw data list object. Then use
  #' parallel processing (one of the three functions we discussed) to process
  #' the data and return a list containing all sociomatricies. Try different
  #' approaches and see which is fastest?
  return(Sociomatrix_List)
}


system.time({
  Result <- PreProcess_Network_Data(filenames,10,4)
})
#'    user  system elapsed 
#'  43.303   1.646  49.726

system.time({
  Result <- PreProcess_Network_Data(filenames,10,1)
})
#'    user  system elapsed 
#'  48.914   0.875  66.553 
