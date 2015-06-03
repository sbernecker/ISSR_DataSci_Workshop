setwd("C:/Users/Sam/Documents/GitHub/ISSR_DataSci_Workshop/Data")

#' Okay, so I wanna start by creating matrices of the data, probably WITHOUT saving each of the data frames read from the .txt files ... I just want to create the matrix objects
#' 
#' 
#' But I'll actually start by working with the code to create a matrix, without worrying where I'm getting it ... I could make it a function and then apply it for each of the files using a loop or possibly even apply.
#' 
#'

#functions
#' outlined below, followed by the top level function which will take an 
#' arbitrary number of input files of the same format as you are working with
#' and generate a list of sociomatricies out of them:

#this produces a character vector of file names ... I stole this code from Matt
filenames <- NULL
for(i in 100:110){
  filenames <- append(filenames, paste(i,"_senmatrix.txt", sep = ""))
}

#This takes a character vector of filenames and produces a list of data frames, one data frame for each file
Read_In_Data <- function(filenames){
  #This creates a list of null elements that is the length of the filename vector, i.e., a null element for every file, with filenames as the labels
  Raw_Data_List = list()
  Raw_Data_List[filenames] = list(NULL)
  
  #loops over all given file names and puts a data frame in each list; also tells you which file you're "saving" at the moment.
  for (i in 1:length(filenames)){
    cat("Now adding", filenames[i], "to raw data list object. This is file number", i, "\n")
    Raw_Data_List[[i]] = read.csv(filenames[i], header = FALSE, stringsAsFactors = FALSE)
  }
  return(Raw_Data_List)
}

#this takes a numeric list index, a list of data frames, and a number of bills for which you want the sociomatrix calculated, and returns the appropriate sociomatrix for the data frame at that index
Generate_Sociomatrix <- function(list_index, raw_data_list, num_bills){
  #this line gives you the number of senators in the data frame
  dimensions = nrow(raw_data_list[[list_index]])
  #creates a square matrix of zeroes with the number of appropriate senators and "names" for each senator which is actually a number
  sennames = as.character(1:dimensions)
  sociomatrix = matrix(0, dimensions, dimensions, dimnames = list(cosponsor = sennames, sponsor = sennames)
  #loops through the specified number of bills
  for (bill in num_bills){
    #gives the row index of the sponsor of the bill
    spons = which(raw_data_list[[list_index]][,bill] == 1)
    #gives a vector of row indices of cosponsors of the bill
    cospons = which(raw_data_list[[list_index]][,bill] == 2)
    #adds 1 to the appropriate cells in the sociomatrix, i.e., to each cosponsor row in the sponsor column
    sociomatrix[cospons,spons] = sociomatrix[cospons,spons] + 1
  }
  return(sociomatrix)
}

#okay, this is the big wrapper function that combines the functions I already created and outputs a sociomatrix list. I'm not going to try parallelization because, uh, I have only two cores, and my goal right now is not to learn how to do that. I just want to learn basic R data management techniques. ... well, actually ... maybe I'll try it. But I'll create a boring function first. 
PreProcess_Network_Data <- function(filenames, num_bills, num_cores = 1){
 #creates a list of data frames NEED TO DO SOMETHING WITH THIS
  raw_data_list = Read_In_Data(filenames)
  #creates an empty list in which to put the sociomatrices, 
  Sociomatrix_List = list()
  nummatrices = length(raw_data_list)
  matrixnames = paste("Matrix#", 1:length(raw_data_list), sep="")
  Sociomatrix_List[matrixnames] = list(NULL)
  #loops over each file, generates a sociomatrix, and puts it in the appropriate spot in the sociomatrix list
  for (i in nummatrices){
    cat("Generating sociomatrix #", i, "of", nummatrices)
    Sociomatrix_List[[i]] = Generate_Sociomatrix(i, raw_data_list, num_bills)
  }
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