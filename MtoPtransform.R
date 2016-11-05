###Multinomial to Poisson transformation of variables.

#####Transforms data such that multinomial regression can be applied in INLA.

###Before using this function to transform the data you need:
#orig.data 			- A data set. 
#						(Dataframe with names.)
#id.string		 	- A way to ID each observation in the data. 
#						(String with name of ID in dataframe.)
#response.string 	- Decide upon the desired response in the data. 
#						(String with the name of the multinomial response intended to use.)
#covariate.string 	- Which covariates that you MAY want to use in a final model.
#						(String with the names of the covariates intended to use.)
#manipulateTF		- Decide which elements should be centralized around "x-mean(x)". 
#						(vector consisting of T/F values)

###Return values:
#The function will return "-1" if the input specifications is wrong.
#The function will return, if succesful:
#	The transformed data.
#	The new covariates are named in such a fashion: 
#		For instance a predictor named "predictor" with a (K category) multinomial response will be named  
#		"predictor.1" (...) "predictor.(K-1)" (etc)
#	A string in the console which can be copied to the INLA formula.

###Errors to look out for:
#If a coloumn in the dataframe has a variable "resp2" errors will be made, so keep that in mind.

mtop.inla = function(orig.data,id.string,response.string,covariate.string,manipulateTF){
	
	#Mandatory
	if(missing(orig.data)){
		print("Please provide the data you want to transform.")
		return(-1)
	}
	#Mandatory
	if(missing(id.string)){
		print("Please provide the name of identifcation parameter")
		return(-1)
	}
	#Mandatory
	if(missing(response.string)){
		print("Please provide the name of response parameter")
		return(-1)
	}
	#Mandatory
	if(missing(covariate.string)){
		print("Please provide the name of covariate(s) you want to utilize later")
		return(-1)
	}
	#Optional
	if(missing(manipulateTF)){
		manipulateTF = rep(FALSE,length(unique(covariate.string)))
	}
	
	#Control checks: (Relying on previous optional to work properly)
	if(length(covariate.string) != length(manipulateTF)){
		print("Error; manipulateTF is not of same length as string of covariates")
		return(-1)
	}
	
	#Storing data in more similar terminology to the example provided by MNA_example2.R
	ml = orig.data
	
	#Ensuring that the response is a factor. Removing all chances of ordinality.
	ml[,response.string] = as.factor(ml[,response.string])
	
	#Ensuring that all the unique factors can be referenced later.
	ref.resp = as.character(sort(unique(ml[,response.string]),decreasing = TRUE))
	print(ref.resp)
	print("This transform will use the last element above as a reference!")
	
	#Finding the amount of responses.
	K = length(ref.resp)
	print(K)
	
	#Setting up for extension of original sample.
	ml$number = 1 
	ml$resp2  = ml[,response.string]
	
	#Creating a list of matrices to iterate through.
	ml.new = list(replicate((K-1), ml,simplify = FALSE))
	
	#Initializing.
	mI = ml
	prev.ml = ml
	
	#Change needs to be smart.
	#Sub-recursive function.
	recca.ifelse = function(p.ml,index,kk,ref){
		index = index + 1
		#If reached end of reference string set equal to first element
		if(index >= kk){ 
			ref[1]
			#Also terminates the recursion.
		}
		#If current value is ref[index] then set to next ref[index+1] if not then check next element
		else{ 
			ifelse(p.ml$resp2 == ref[index],ref[index+1],recca.ifelse(p.ml,index,kk,ref))
		}
	
	}
	
	#Mother-recursive function
	rec.ifelse = function(t.ml,p.ml,index,KK,ref){
		index = index + 1
		
		changed = ifelse(p.ml$resp2 == ref[index],ref[index+1],recca.ifelse(p.ml,index,KK,ref))
		
		return(changed)
	}
		
	#Binding together by rows.
	for(i in 1:(K-1)){
		#Getting the replicates from the list
		this.ml = ml.new[[1]][[i]]
		this.ml$number = 0
		
		#Mixing up response - before binding together - Seems ok!
		this.ml$resp2 = rec.ifelse(this.ml,prev.ml,0,K,ref.resp)
		
		#Finalizing this iteration
		prev.ml = this.ml
		mI = rbind(mI,this.ml)
	}
	
	#For storing the new names of the covariates.
	new.cov.names = c()
	
	Covnum = length(unique(covariate.string))
	#Splitting up covariates in K-1 dummies.
	#Outer loop to check all the different parts of covariate string.
	for(j in 1:Covnum){
		#Extracting the j'th covariate
		this.cov = covariate.string[j]
		
		#Extracting values for manipulation. #Center may be changed...
		value.cov = mI[,this.cov]
		response.index = mI$resp2
		
		#Manipulates
		if(manipulateTF[j]){
			tmp.cov.manip = value.cov - mean(value.cov)
		}
		else{
			tmp.cov.manip = value.cov
		}
		
		#Inner loop to create K-1 dummy variables
		for(k in 1:(K-1)){
			#Setting last element in ref.resp as reference.
			mI[,paste(this.cov,ref.resp[k],sep=".")] = (tmp.cov.manip*(response.index == ref.resp[k]))
			new.cov.names = c(new.cov.names,paste(this.cov,ref.resp[k],sep="."))
		}
		
	}
	#return the transformed data
	print("The following are new covariates you will be using")
	print("Please copy the following //partial formula// and paste it into another formula")
	cat(new.cov.names,sep =" + ")
	
	
	#Return final product
	return(mI)			
}
