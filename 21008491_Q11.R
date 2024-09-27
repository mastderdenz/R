#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
fldf<-function(X,Type,p1,X.new){
#######################################################################
##
##    Fisher's linear discriminant function classifies individuals
##    into type 1 or type 2 based on their measurements of k 
##    characteristics. The expression is D(x) = a'x, where a is
##    a k*1 vector and x is a k*1 vector. 
##    The arguments are:
##
##    X       A matrix containing all of the sample measurements
##            with a row for each individual and a column for
##            each variable.
##    Type    A vector of 1s and 2s corresponding to the rows of
##            X and indicating the type of each individual in
##            the sample.
##    p1      The proportion of Type 1 individuals in the
##            population. The default value is the proportion of
##            Type 1 individuals in the sample.
##    X.new   A matrix containing measurements for a new sample
##            of individuals whose types are not known. The
##            default value is a 0*k matrix.
##            
##    The function returns a list containing components a (the
##    vector a), X.class (a vector with an element for each row
##    of X, containing Fisher's classification of each individual
##    in the original sample), ClassTab (a 2*2 table whose (i,j)th
##    element contains the number of Type i individuals in the
##    original sample that were classified as Type j), HitRate
##    (the percent of individuals in the original sample who were
##    classified correctly), and X.new.class (a vector with an
##    element for each row of X.new, containing Fisher's
##    classification of each individual in the new sample).
##
#######################################################################
  #
  # Started by initialising all the quantities that we need, 
  # as follows:
  #
  # X.new         A matrix of 0 rows and ncol(X) columns (initialised to 0*number 
  #               of columns of matrix X if not provided).
  # π1            The proportion of Type 1 individuals in the population.
  # π2            The proportion of Type 2 individuals in the population.
  #
  # Using the "if statements" to loop to check if X.new is provided or not. If not, 
  # then X.new will be set to the default matrix of 0 rows and ncol(X) columns.
  #
  if (nrow(X.new)==0){
    X.new<-matrix(0,0,ncol(X))
  }
  π1<-p1 # The proportion of Type 1 individuals in the population.
  π2<-1-p1 # The proportion of Type 2 individuals in the population.
  #
  # Then we calculate the pooled sample covariance matrix using the values we 
  # produce from calculating sample mean vector and sample covariance matrix for
  # Type 1 and 2 individuals.
  #
  n1<-sum(Type==1) # The number of Type 1 individuals in the sample.
  n2<-sum(Type==2) # The number of Type 2 individuals in the sample.
  x1<-colMeans(X[Type==1,]) # k*1 sample mean vector for Type 1 individuals.
  x2<-colMeans(X[Type==2,]) # k*1 sample mean vector for Type 2 individuals.
  S1<-cov(X[Type==1,]) # k*k sample covariance matrix for Type 1 individuals.
  S2<-cov(X[Type==2,]) # k*k sample covariance matrix for Type 2 individuals.
  S<-((n1+n2-2)^(-1))*((n1-1)*S1+(n2-1)*S2) #The pooled sample covariance matrix.
  #
  a<-solve(S)%*%(x1-x2) # Calculation of the vector a.
  #
  # Using the "if statement" to loop to check the classification of each individual
  # in the original sample.Type 1 is returned if  'a'[x - 1/2*(x1 + x2)] > log(π1/π2)
  # and Type 2 otherwise.
  #
  X.class<-ifelse(t(a)%*%t(X-0.5*(x1+x2))>log(π1/π2),1,2) #This is the classification 
  # of the original sample.
  #
  # Using the "if statement" to loop to check the classification of each individual
  # in the new sample.Type 1 is returned if  'a'[x - 1/2*(x1 + x2)] > log(π1/π2)
  # and Type 2 otherwise.
  #
  X.new.class<-ifelse(t(a)%*%t(X.new-0.5*(x1+x2))>log(π1/π2),1,2)
  #
  ClassTab<-table(Type,X.class) # Classification table.
  HitRate<-(ClassTab[1,1]+ClassTab[2,2])/sum(ClassTab) # The percentage of 
  # individuals in the original sample who were classified correctly.
  #
  # Return list 
  #
  list(a=a,X.class=X.class,ClassTab=ClassTab,HitRate=HitRate,X.new.class=X.new.class)
}
