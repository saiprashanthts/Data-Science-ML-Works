
# Before Importing the data . We have to set the working diectory like setwd("C:/Users/Sai Prashanth T S/Downloads")
 
#	Import the Bollywood data set in Rstudio in a variable named bollywood


  bollywood <- read.csv('bollywood.csv')
  View(bollywood)


#	When you import a data set, R stores character vectors as factors (by default)
# You can check the structure of the data frame by using str()
  str(bollywood)

# You can change the attribute 'Movie' from factor to character type using the given command
  bollywood$Movie <- as.character(bollywood$Movie)
	 

#Q1.
#	Access the last 10 movies (from the bottom of the Bollywood data frame) using column bollywood$Movie
# tail command is used to access the dataframe from bottom 
#tail(bollywood$Movie,10)
# Store the names of those movies in last_10 vector (in the same order)
     
  last_10 <- as.vector(tail(bollywood$Movie,10))
# to check whether last_10 is vector or not we can check by using is.vector(last_10)
	  
#Q2.
#	Find out the total number of  missing values (NA) in the bollywood data frame.
# Store the result in na_bollywood vector
  
#is.na() is used to identify the NA's in the dataframe and since total was asked we can sum up the result
     
	na_bollywood <- as.vector(sum(is.na(bollywood)))
	
# To check whether na_bollywood is vector or not we can check by using is.vector(na_bollywood)
	  
	
#Q3
#	Write the command to find out which movie tops the list in terms of Total Collections
# Store the movie name in variable named top_movie
 
# which.max() will return the index position of maximum total collections and finding out the row in the dataaframe and fetching only the movie name from it
  top_movie <- bollywood[which.max(bollywood$Tcollection),] ['Movie']

  
#Q4
#	Write the command to find out which movie comes second on the list in terms of Total Collections
# Store the movie name in variable named top_2_movie
# to find the secnd maximum we can sort the result by descending and finding out the second element from it and storing only the movie name into top_2 movie
  top_2_movie <- bollywood[which(bollywood$Tcollection==sort(bollywood$Tcollection,TRUE)[2]),] ['Movie']
	  
	
# Now let's find out the movies shot by Shahrukh, Akshay and Amitabh separately.
# subset() function is used for that. The code has already been written for you. 
	
	shahrukh <- subset(bollywood, Lead == "Shahrukh")
	akshay <- subset(bollywood, Lead == "Akshay")
	amitabh <- subset(bollywood, Lead  == "Amitabh")

# You can view what the above data frames look like

		   
#Q5
#	What is the total collection of Shahrukh, Akshay and Amitabh movies individually?
# You can use	a column named 'Tcollection' for this 
 
# to find out the total collection of shahrukh sum up the TCollection of the dataframe and store it in the variable	
  shahrukh_collection <- sum(shahrukh$Tcollection)

# to find out the total collection of akshay sum up the TCollection of the dataframe and store it in the variable
	akshay_collection <- sum(akshay$Tcollection)

# to find out the total collection of amitabh sum up the TCollection of the dataframe and store it in the variable	    
	amitabh_collection <- sum(amitabh$Tcollection)
    
	
#Q6  
# Write command/s to find out how many movies are in Flop, Average, Hit and Superhit categories in the entire Bollywood data set.
# to find out how many movies are in different category we can use summary function on the verdict column of the dataframe column
	summary(bollywood$Verdict)
   
#You can use SAPPLY function if you want to apply a function specific columns in a data frame 
#You can write a command to find the maximum value of Ocollection, Wcollection, Fwcollecion and Tcollection using sapply
# we can make use of sapply function along with max function , na.rm is used to remove the NA's value while finding out the maxs
	sapply(bollywood[c('Ocollection', 'Wcollection', 'Fwcollection', 'Tcollection')], max,na.rm=TRUE)
  
#Q7 
# Write a command to find the names of the movies which have the maximum Ocollection, Wcollection, Fwcollecion & Tcollection
# Store the names of 4 movies in same sequence in movie_result vector
# we can make use of sapply function to find out along with it which.max is used to find out the index and unlisting and converting it into vector
  movie_result <- as.vector(unlist(bollywood[sapply(bollywood[c('Ocollection', 'Wcollection', 'Fwcollection', 'Tcollection')], which.max),]['Movie'],use.names = FALSE))
# To check if it is vector make useof is.vector(movie_result)
	

   
    


    
    
    