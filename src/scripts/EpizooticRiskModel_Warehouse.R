# ______________________________________________________________________________
#
# EPIZOOTIC RISK MODEL
# 
# AUTHOR: Georgianna Silveira
# ERROR HANDLING: Brenda Hanley
# MODEL LOGGING: Brenda Hanley
# QAQC: Brenda Hanley
#
# DATE: August 2024
# Location: Cornell Wildlife Health Laboratory
# License: MIT
#
# This code was adapted for the CWD Data warehouse from:
#     1) JANUARY 2022 original SEI model warehouse code by Cara Them.
#     2) SLEI Model software published by Hanley et al. 2022 at
#       https://ecommons.cornell.edu/items/1231a1d6-fd15-44d4-be8c-c3bb46c2fd1d

# This code was written under:
# R version 4.4.0 (2024-04-24 ucrt) -- "Puppy Cup"
# Copyright (C) 2024 The R Foundation for Statistical Computing
# Platform: x86_64-w64-mingw32/x64
#
# ______________________________________________________________________________

# Load packages. 
    library(tidyverse) # Version 4.4.0

# Info about Required Data. 

  # There are three required files that will always be imported:
  # 1. A .csv file with user-selected parameters.
  # 2. A .csv file with the list of subadmin for the focal state.
  # 3. A .csv file with the animals sampled that match the user inputs.

# Model log file started with Python data processing script. 
    model_log_filepath=file.path("", "data", "attachments", "info.txt")

# Continue the log started with the Python script. 
    write("\n-------------------",file=model_log_filepath,append=TRUE)
    line = 'Model Execution'
    write(line,file=model_log_filepath,append=TRUE)

# Read in the (Required) Parameters file. 
    params_filepath = file.path("", "data", "params.csv")
    params=readr::read_csv(params_filepath)
    # Note: The params.csv has to exist b/c Python generated it and b/c model 
    # has to create it. Therefore, this error handling is in the python code and 
    # this R script will not run if it does not exist. 

# Read in the (Required) Subadmin file. 
    subadmin_filepath = file.path("", "data", "sub_administrative_area.csv")
    subadmin=readr::read_csv(subadmin_filepath) 
    # Note: The sub_administrative_area.csv has to exist b/c Python generated it 
    # and b/c model has to create it. Therefore, this error handling is in the 
    # python code and this R script will not run if it does not exist. 
    
# Read in (Required) Samples file. 
    sample_filepath = file.path("", "data", "sample.csv")
    sample=readr::read_csv(sample_filepath) 
    # Note: The sub_administrative_area.csv has to exist b/c Python generated 
    # it and b/c model has to create it. Therefore, this error handling is in 
    # the python code and this R script will not run if it does not exist.

# Conduct preliminary cleaning. 
    # SubAdmin file. 
    # Create data frame containing only the subadmin names and IDs.
    SubAdmin_Standard=data.frame("SubAdminID"=unique(subadmin$id),"SubAdminName"=unique(subadmin$full_name)) 
      
    # Get the area of each subadmin in square kilometers.
    Area=(as.numeric(unique(subadmin$aland)))/(1000*1000)
      
    # Append the Area to the SubAdmin_Standard to create 
    # the 'standard' area data frame.
    SubAdmin_Standard_Area=as.data.frame(cbind(SubAdmin_Standard,Area))
      
    # Sample file. 
    # Make sure sample is data frame. 
    sample_df=as.data.frame(sample)
        
    # Get the dimensions of the original sample data. 
    OrigDim=as.numeric(nrow(sample_df))
      
    # Remove any samples with missing subadmin.
    sample_df_clean=sample_df[!is.na(sample_df$sub_administrative_area_id),]
      
    # Get the dimensions of the clean sample data.  
    CleanDim=as.numeric(nrow(sample_df_clean))
        
        # If at least one sample remains after cleaning.
           if (CleanDim>0){
                    
                        # If all samples were retained.
                        if(CleanDim==OrigDim){
                        # Print a message stating that all samples have known subadmin.
                        line="All samples belong to a known sub administration area and therefore the model can use every record."
                        write(line,file=model_log_filepath,append=TRUE)
                        } # End if all samples had known subadmin.
                    
                        # If some samples were we retained. 
                        if(CleanDim!=OrigDim){
                        # Print a message saying how many samples you started with.
                        line=paste("The sample data set started with", OrigDim, "samples.")
                        write(line,file=model_log_filepath,append=TRUE)
                        # Print a message saying how many samples were removed.
                        line=paste("However,", OrigDim-CleanDim, "samples were removed due to missing sub administration area.")
                        write(line,file=model_log_filepath,append=TRUE)                          
                        # Print a message saying how many samples are remaining in model. 
                        line=paste("Thus, the model will now use the", CleanDim, "complete samples.")
                        write(line,file=model_log_filepath,append=TRUE)                          
                        } # End if some samples were removed.
                    
          } # End if at least one sample remains after removal.
        
          # If all samples were removed during cleaning.
          if (CleanDim==0){
          # Print error message to log file.
          line="We're sorry. The data does not contain at least one record with a known sub adminstrative area, which is a requirement of this model. Please go back to the Warehouse and select a different set of samples to analyze."
          write(line,file=model_log_filepath,append=TRUE) 
          # Kill the code.  
          quit("yes")
          } # End if all samples were removed.
        
# At this point in the code, there has to be data in 'sample_clean' 
# because if there are no samples, the code would have been terminated.

# Populate the enviro contamination (load) vector.                     
# The goal of this next section is to create a data frame containing the subadmins 
# with one (or more) positive test(s) and the number of positives in those subadmin.
    positives=subset(sample_df_clean,sample_df_clean$result=="Detected")
    PosDim=as.numeric(nrow(positives))  
    
          # If there exists positive tests.
          if(PosDim>0){
          # Keep only column ID and test result columns.
          positives2=subset(positives, select=c("sub_administrative_area_id", "result"))
          # Clean up column names.
          names(positives2)=c("SubAdminID","Load")
          # Count number of positive samples per subadmin.
          Load=as.data.frame(positives2%>%group_by(SubAdminID)%>%summarize(Load=n()))
          # Join load vector to the list of all subadmin. 
          # By doing a left join, subadmin with no CWD load = NA.
          SubAdmin_Standard_Load=left_join(SubAdmin_Standard_Area,Load,by="SubAdminID")
          # Change subadmin with NA to Load = 0.
          SubAdmin_Standard_Load$Load=replace_na(SubAdmin_Standard_Load$Load,0)
          } # End if positive tests.
          
          # If there does not exist positive tests.
          if(PosDim==0){
          # Create a Load of length subadmin, where all subadmin have Load = 0.
          Load=rep(0,times=nrow(SubAdmin_Standard_Area))
          # Bind the load vector to the list of all subadmin.
          SubAdmin_Standard_Load=cbind(SubAdmin_Standard_Area,Load)
          # Print a message that there were no positive samples.
          line="No positive CWD tests were reported in this data set, so the environmental contamination value has been set to zero for all sub administrative areas."
          write(line,file=model_log_filepath,append=TRUE)                               
          } # End if no positive tests.
            
# Import and Format the Demographic Data. 
    
    # Create a new header in the model log.
    line=''
    write(line,file=model_log_filepath,append=TRUE)
    line='Demographic Data'
    write(line,file=model_log_filepath,append=TRUE)
    
  # There are four additional files that will only be read in IF the user opts to  
  # run the model with different demographic factors for each subadmin.
    
  # If not, then the demography files do not exist and constants for each
  # demographic factor will be pulled from the parameters file.

  # Create an import function to see whether optional demographic data exists. 
  importdemography <- function(x) {
    # Create tryCatch function that attempts import a demographic file.
    # If a file does not exist, this function returns a null value.
    # If the file does exist, it returns the file.
    # Note that the decision tree below will record the existence of each file on the log.
      tryCatch(expr={
      readr::read_csv(file.path("","data", x))},
      error=function(cond){
      return(NULL)})
      } # End import function.

  # Wrangling the (Optional) Fecundity file 
  Fecundity_import=importdemography("demography_fecundity.csv")
    
      # If Fecundity data does not exist. 
      if(is.null(Fecundity_import)){
      # Print message to log file.
      line="Fecundity data does not exist. The model will use user-input values instead."
      write(line,file=model_log_filepath,append=TRUE) 
      } # End if Fecundity data does not exist.
    
      # If Fecundity data exists.
      if(!is.null(Fecundity_import)){
      # Print success message to log file.
      line="Fecundity data loaded."
      write(line,file=model_log_filepath,append=TRUE) 
      # Convert the list to a data frame where each row represents a subadmin.
      Fecundity_df=as.data.frame(Fecundity_import)
      # Clean up column names.
      colnames(Fecundity_df)=c("SubAdminID", "Fecundity")
      } # End if Fecundity data exists. 
    
  # Wrangling the (Optional) Harvest Mortality file  
  Harvest_import=importdemography("demography_harvest_mortality.csv")
    
      # If Harvest data does not exist. 
      if(is.null(Harvest_import)){
      # Print message to log file.
      line="Harvest data does not exist. The model will use user-input values instead."
      write(line,file=model_log_filepath,append=TRUE) 
      } # End if Harvest data does not exist.
    
      # If Harvest data exists. 
      if(!is.null(Harvest_import)){
      # Print success message to log file.
      line="Harvest data loaded."
      write(line,file=model_log_filepath,append=TRUE) 
      # Convert the list to a data frame where each row represents a subadmin.
      Harvest_df=as.data.frame(Harvest_import)
      # Clean up column names.
      colnames(Harvest_df)=c("SubAdminID", "Harvest")
      } # End if Harvest data exists.
    
  # Wrangling the (Optional) Natural Mortality file.
  Mortality_import=importdemography("demography_natural_mortality.csv")
    
      # If natural Mortality data does not exist.
      if(is.null(Mortality_import)){
      # Print message to log file.
      line="Natural Mortality data does not exist. The model will use user-input values instead."
      write(line,file=model_log_filepath,append=TRUE) 
      } # End if natural Mortality data does not exist.
    
      # If natural Mortality data exists.
      if(!is.null(Mortality_import)){
      # Print success message to log file.
      line="Natural Mortality data loaded."
      write(line,file=model_log_filepath,append=TRUE) 
      # Convert the list to a data frame where each row represents a subadmin.
      Mortality_df=as.data.frame(Mortality_import)
      # Clean up column names.
      colnames(Mortality_df)=c("SubAdminID", "Mortality")
      } # End if natural Mortality exists.
    
  # Wrangling the (Optional) Density file. 
  Density_import=importdemography("demography_density.csv")
    
      # If Density data does not exist.
      if(is.null(Density_import)){
      # Print message to log file.
      line="Density data does not exist. The model will use user-input values instead."
      write(line,file=model_log_filepath,append=TRUE) 
      } # End if Density data does not exist.
    
      # If Density data exists.
      if(!is.null(Density_import)){
      # Print success message to log file.
      line="Density data loaded."
      write(line,file=model_log_filepath,append=TRUE)           
      # Convert the list to a data frame where each row represents a subadmin.
      Density_df=as.data.frame(Density_import)
      # Clean up column names.
      colnames(Density_df)=c("SubAdminID", "Density")
      } # End if Density data exists.

# Notes at this point in the script: 
    
# Required data.   
# Parameter data exists, and it is called 'params'. 
# Subadmin data exists, and 'SubAdmin_Standard_Area' contains a comprehensive 
# list of subadmin, their IDs, and their areas (in square km). 
# Samples data exist, and 'SubAdmin_Standard_Load' contains a comprehensive list 
# of subadmin, their IDs, areas (in square km), and load. 
    
# Optional data.
# If data exists, then 'Fecundity_df' contains a comprehensive list of subadmin ID and their Fecundity. 
# If data exists, then 'Harvest_df' contains a comprehensive list of subadmin ID and their Harvest. 
# If data exists, then 'Mortality_df' contains a comprehensive list of subadmin ID and their Mortality. 
# If data exists, then 'Density_df' contains a comprehensive list of subadmin ID and their Density.
    
# Create the baseline ModelMatrix. 
ModelMatrix=SubAdmin_Standard_Load
    
# Add demographic data to ModelMatrix. 
# Fecundity.
    
      # If Fecundity data exists, use it.
      if(!is.null(Fecundity_import)){
      # Append the data to the Model Matrix.
      ModelMatrix=left_join(ModelMatrix,Fecundity_df,by="SubAdminID")
      rm(Fecundity_df)
      } # End if Fecundity data exists.
    
      # If Fecundity data does not exist, use a static parameter. 
      if(is.null(Fecundity_import)){
      # Get a static vector of Fecundity. 
      Fecundity=rep(as.numeric(params$fecundity_value),nrow(ModelMatrix))
      # Append the vector to Model Matrix.
      ModelMatrix=cbind(ModelMatrix,Fecundity)
      rm(Fecundity)
      } # End if Fecundity data does not exist.
    
# Harvest Mortality.
    
      # If Harvest Mortality data exists, used it.
      if(!is.null(Harvest_import)){
      # Append the data to the Model Matrix.
      ModelMatrix=left_join(ModelMatrix,Harvest_df,by="SubAdminID")
      rm(Harvest_df)
      } # End if Harvest Mortality exists.
    
      # If Harvest Mortality does not exist, use a static parameter. 
      if(is.null(Harvest_import)){
      # Get a static vector of Harvest Mortality.
      Harvest=rep(as.numeric(params$harvest_mortality_value),nrow(ModelMatrix))
      # Append the vector to Model Matrix.
      ModelMatrix=cbind(ModelMatrix,Harvest)
      rm(Harvest)
      } # End if Harvest Mortality data does not exist.
    
# Natural Mortality.
    
      # If natural Mortality data exists, used it.
      if(!is.null(Mortality_import)){
      # Append the data to the Model Matrix.
      ModelMatrix=left_join(ModelMatrix,Mortality_df,by="SubAdminID")
      rm(Mortality_df)
      } # End if natural Mortality exists.
    
      # If natural Mortality does not exist, use a static parameter. 
      if(is.null(Mortality_import)){
      # Get a static vector of natural Mortality.
      Mortality=rep(as.numeric(params$natural_mortality_value),nrow(ModelMatrix))
      # Append the vector to Model Matrix.
      ModelMatrix=cbind(ModelMatrix,Mortality)
      rm(Mortality)
      } # End if natural Mortality data does not exist.
    
# Density.
    
      # If Density data exists, used it.
      if(!is.null(Density_import)){
      # Append the data to the Model Matrix.
      ModelMatrix=left_join(ModelMatrix,Density_df,by="SubAdminID")
      rm(Density_df)
      } # End if Density data exists.
    
      # If Density does not exist, use a static parameter. 
      if(is.null(Density_import)){
      # Get a static vector of Density.
      Density=rep(as.numeric(params$density_value),nrow(ModelMatrix))
      # Append the vector to Model Matrix.
      ModelMatrix=cbind(ModelMatrix,Density)
      rm(Density)
      } # End if Density data does not exist.

# Create a new header in the model log.
    line=''
    write(line,file=model_log_filepath,append=TRUE)
    line='Epidemiological Data'
    write(line,file=model_log_filepath,append=TRUE)  
  
# Add epidemiological data to the model matrix. 
# GammaE. 
      # Use static parameter.
      gammaE=rep(as.numeric(params$transmission_via_subclinical_deer),nrow(ModelMatrix))
      # Append vector to Model Matrix.
      ModelMatrix=cbind(ModelMatrix,gammaE)
      rm(gammaE)
      # Log the message.
      line='Data depicting transmission between subclinical hosts and new hosts does not exist. The model will use user-input values instead.'
      write(line,file=model_log_filepath,append=TRUE) 

# GammaI.
      # Use static parameter.
      gammaI=rep(as.numeric(params$transmission_via_clinical_deer),nrow(ModelMatrix))
      # Append vector to Model Matrix.    
      ModelMatrix=cbind(ModelMatrix,gammaI)
      rm(gammaI)
      # Log the message.
      line='Data depicting transmission between clinical hosts and new hosts does not exist. The model will use user-input values instead.'
      write(line,file=model_log_filepath,append=TRUE) 
      
# Sigma.    
      # Use static parameter.
      sigma=as.numeric(ModelMatrix$Load/10000)
      # Append vector to Model Matrix.  
      ModelMatrix=cbind(ModelMatrix,sigma)
      rm(sigma)
      # Log the message.
      line='Data depicting transmission between the environment and new hosts does not exist. The model will use user-input values instead.'
      write(line,file=model_log_filepath,append=TRUE)   
      
# Omega.   
      # Use static parameter.
      omega=rep(as.numeric(params$omega),nrow(ModelMatrix))
      # Append vector to Model Matrix.  
      ModelMatrix=cbind(ModelMatrix,omega)
      rm(omega)
      # Log the message.
      line='Data on disease progression from disease-free hosts to subclinical hosts does not exist. The model will use user-input values instead.'
      write(line,file=model_log_filepath,append=TRUE) 
      
# Epsilon.    
      # Use static parameter.
      epsilon=rep(as.numeric(params$epsilon),nrow(ModelMatrix))
      # Append vector to Model Matrix.  
      ModelMatrix=cbind(ModelMatrix,epsilon)
      rm(epsilon)
      # Log the message.
      line='Data on disease progression from subclinical to clinical hosts does not exist. The model will use user-input values instead.'
      write(line,file=model_log_filepath,append=TRUE) 
      
# Alpha.
      # Use static parameter.
      alpha=rep(as.numeric(params$alpha),nrow(ModelMatrix))
      # Append vector to Model Matrix.  
      ModelMatrix=cbind(ModelMatrix,alpha)
      rm(alpha)
      # Log the message.
      line='Data on disease progression from clinical host to CWD mortality hosts does not exist. The model will use user-input values instead.'
      write(line,file=model_log_filepath,append=TRUE) 
      
# q.   
      # Use static parameter.
      q=rep(as.numeric(params$q),nrow(ModelMatrix))
      # Append vector to Model Matrix.  
      ModelMatrix=cbind(ModelMatrix,q)
      rm(q)
      # Log the message.
      line='Data on transmission (contact) type does not exist. The model will use user-input values instead.'
      write(line,file=model_log_filepath,append=TRUE) 
      
# Eta.   
      # Use static parameter.
      eta=rep(as.numeric(params$eta),nrow(ModelMatrix))
      # Append vector to Model Matrix.  
      ModelMatrix=cbind(ModelMatrix,eta)
      rm(eta)
      # Log the message.
      line='Data on the rate of prion removal from the environment does not exist. The model will use user-input values instead.'
      write(line,file=model_log_filepath,append=TRUE) 
      
# PhiE.  
      # Use static parameter.
      phiE=rep(as.numeric(params$phi1),nrow(ModelMatrix))
      # Append vector to Model Matrix.  
      ModelMatrix=cbind(ModelMatrix,phiE)
      rm(phiE)
      # Log the message.
      line='Data on the prion deposition rate of subclinical carcasses does not exist. The model will use user-input values instead.'
      write(line,file=model_log_filepath,append=TRUE) 
      
# PhiI.    
      # Use static parameter.
      phiI=rep(as.numeric(params$phi2),nrow(ModelMatrix))
      # Append vector to Model Matrix.  
      ModelMatrix=cbind(ModelMatrix,phiI)
      rm(phiI)
      # Log the message.
      line='Data on the prion deposition rate of clinical carcasses does not exist. The model will use user-input values instead.'
      write(line,file=model_log_filepath,append=TRUE) 
      
# PhiX.   
      # Use static parameter.
      phiX=rep(as.numeric(params$phi3),nrow(ModelMatrix))
      # Append vector to Model Matrix.  
      ModelMatrix=cbind(ModelMatrix,phiX)
      rm(phiX)
      # Log the message.
      line='Data on the prion deposition rates of hosts that died from CWD does not exist. The model will use user-input values instead.'
      write(line,file=model_log_filepath,append=TRUE) 
      
# ThetaE.
      # Use static parameter.
      thetaE=rep(as.numeric(params$theta1),nrow(ModelMatrix))
      # Append vector to Model Matrix.  
      ModelMatrix=cbind(ModelMatrix,thetaE)
      rm(thetaE)
      # Log the message.
      line='Data on the shedding rate of subclinical hosts does not exist. The model will use user-input values instead.'
      write(line,file=model_log_filepath,append=TRUE) 
      
# ThetaI.
      # Use static parameter.
      thetaI=rep(as.numeric(params$theta2),nrow(ModelMatrix))
      # Append vector to Model Matrix.  
      ModelMatrix=cbind(ModelMatrix,thetaI)
      rm(thetaI)
      # Log the message.
      line='Data on the shedding rate of clinical hosts does not exist. The model will use user-input values instead.'
      write(line,file=model_log_filepath,append=TRUE) 

# Notes at this point in the script: .
# The n x 21 Model Matrix now exists, where n is the number of sub 
# administrative areas and 21 are the variables. 

# Print the Model Matrix.
      setwd("/data/attachments")
      # Write the Model Matrix to the attachments working directory.
      write.csv(ModelMatrix, "EpizooticRiskModelInput.csv", row.names = FALSE)  

# Ensure that the ModelMatrix does not have lurking NAs from randomly missing data
# within a vector somewhere. 
	ModelMatrixDim=as.numeric(nrow(ModelMatrix))
	ModelMatrixComplete=ModelMatrix[complete.cases(ModelMatrix), ] 
	ModelMatrixCompleteDim=as.numeric(nrow(ModelMatrixComplete))

	# If a subadmin area was removed due to NA, then tell the user. 
	if (ModelMatrixCompleteDim>0 | ModelMatrixCompleteDim==ModelMatrixDim){
	# Tell the user how many areas were removed. 
    	line=''
    	write(line,file=model_log_filepath,append=TRUE)
    	line='Complete Cases'
    	write(line,file=model_log_filepath,append=TRUE)
	Diff=ModelMatrixDim-ModelMatrixCompleteDim
	line=paste("This model requires each subadministrative area to have values for all inputs.", Diff," subadministrative areas were removed from this analysis due to missing data within one ore more required fields.")
  	write(line,file=model_log_filepath,append=TRUE) 
  
  # Note. At this point in the code, the files exist and data exists in the files. Otherwise the code will have been terminated. 
  
  # Make the vectors their own objects for use in the computations. 
  # The convention of all caps means we are sure we can do math on these vectors. 
  AREA=as.numeric(ModelMatrixComplete$Area)
  LOAD=as.numeric(ModelMatrixComplete$Load) 
  FECUNDITY=as.numeric(ModelMatrixComplete$Fecundity)
  HARVEST=as.numeric(ModelMatrixComplete$Harvest)
  MORTALITY=as.numeric(ModelMatrixComplete$Mortality)
  DENSITY=as.numeric(ModelMatrixComplete$Density)
  GAMMAE=as.numeric(ModelMatrixComplete$gammaE)
  GAMMAI=as.numeric(ModelMatrixComplete$gammaI)
  SIGMA=as.numeric(ModelMatrixComplete$sigma)
  OMEGA=as.numeric(ModelMatrixComplete$omega)
  EPSILON=as.numeric(ModelMatrixComplete$epsilon)
  ALPHA=as.numeric(ModelMatrixComplete$alpha)
  Q=as.numeric(ModelMatrixComplete$q)
  ETA=as.numeric(ModelMatrixComplete$eta)
  PHIE=as.numeric(ModelMatrixComplete$phiE)
  PHII=as.numeric(ModelMatrixComplete$phiI)
  PHIX=as.numeric(ModelMatrixComplete$phiX)
  THETAE=as.numeric(ModelMatrixComplete$thetaE)
  THETAI=as.numeric(ModelMatrixComplete$thetaI)
  
  # Initialize compound variables.
  N=DENSITY*AREA
  MUS=MORTALITY+HARVEST
  MUL=MORTALITY+HARVEST
  MUEW=HARVEST
  MUED=MORTALITY
  MUE=MUEW+MUED
  MUIW=HARVEST
  MUID=MORTALITY
  MUI=MUIW+MUID
  
  # Initialize OutputMatrix. 
  OutputMatrix=as.data.frame(cbind(
      SubAdminID = ModelMatrixComplete$SubAdminID,
      SubAdminName = ModelMatrixComplete$SubAdminName)
  )
  
  # Calculate the Population Growth Rate. 
  # Initialize empty vector to store calculations.
  PopulationGrowthRate=vector(length=nrow(ModelMatrixComplete))
  # Calculate growth rate for each subadmin.
  for(i in 1:nrow(ModelMatrixComplete)){
    # Calculate survival.
    Survival=1-(HARVEST[i]+MORTALITY[i])
    # Create matrix.
    top=c(0,FECUNDITY[i],FECUNDITY[i])
    middle=c(Survival,0,0)
    bottom=c(0,Survival,Survival)
    M=rbind(top,middle,bottom)
    # Calculate growth rate from matrix.
    PopulationGrowthRate[i]=round(eigen(M)$values[1],digits=3)
  }
  # Append PopulationGrowthRate onto Output Matrix.
  OutputMatrix=cbind(OutputMatrix,PopulationGrowthRate)
  
  # Compute the Epizootic Potential.
  # First term.
  a_numerator=GAMMAE*FECUNDITY*OMEGA
  a_denominator=MUS*N^Q*(MUL+OMEGA)*(MUEW+MUED+EPSILON)
  ratio_a=a_numerator/a_denominator
  # Second term.
  b_numerator=GAMMAI*FECUNDITY*OMEGA*EPSILON
  b_denominator=MUS*N^Q*(MUL+OMEGA)*(MUEW+MUED+EPSILON)*(MUIW+MUID+ALPHA)
  ratio_b=b_numerator/b_denominator
  # Third term.
  c_numerator=SIGMA*FECUNDITY*OMEGA*(THETAE+PHIE*MUED)
  c_denominator=MUS*ETA*(MUL+OMEGA)*(MUEW+MUED+EPSILON)
  ratio_c=c_numerator/c_denominator
  # Fourth term.
  d_numerator=SIGMA*FECUNDITY*OMEGA*EPSILON*(THETAI+PHII*MUID+PHIX*ALPHA)
  d_denominator=MUS*ETA*(MUL+OMEGA)*(MUEW+MUED+EPSILON)*(MUIW+MUID+ALPHA)
  ratio_d=d_numerator/d_denominator
  
  # Add all four terms and round to three digits.
  EpizooticPotential=round(ratio_a+ratio_b+ratio_c+ratio_d, digits=3)

  # Append EpizooticPotential onto Output Matrix.
  OutputMatrix=cbind(OutputMatrix,EpizooticPotential)
  
  # Rank Epizootic Potential. 
  EpizooticPotentialRank=rank(-EpizooticPotential,ties.method="min")
  
  # Append EpizooticPotential onto Output Matrix.
  OutputMatrix=cbind(OutputMatrix,EpizooticPotentialRank)
  
  # Compute Influences on Epizootic Potential. 
  E_N_numerator=(N/EpizooticPotential)*-1*Q*FECUNDITY*OMEGA*(GAMMAE*(MUIW+MUID+ALPHA)-GAMMAI*EPSILON)
  E_N_denominator=MUS*(N^(Q+1))*(MUL+OMEGA)*(MUEW+MUED+EPSILON)*(MUIW+MUID+ALPHA)
  E_N=E_N_numerator/E_N_denominator
  
  E_FECUNDITY_numerator=(FECUNDITY/EpizooticPotential)*ETA*GAMMAE*OMEGA*(MUIW+MUID+ALPHA)+ETA*GAMMAI*OMEGA*EPSILON+N^Q*SIGMA*OMEGA*(THETAE+PHIE*MUED)*(MUIW+MUID+ALPHA)+N^Q*SIGMA*OMEGA*EPSILON*(THETAI+PHII*MUID+PHIX*ALPHA)
  E_FECUNDITY_denominator=MUS*ETA*N^Q*(MUL+OMEGA)*(MUEW+MUED+EPSILON)*(MUIW+MUID+ALPHA)
  E_f=E_FECUNDITY_numerator/E_FECUNDITY_denominator
  
  E_MUS_numerator=(MUS/EpizooticPotential)*-1*GAMMAE*FECUNDITY*OMEGA*(MUIW+MUID+ALPHA)-ETA*GAMMAI*FECUNDITY*OMEGA*EPSILON-SIGMA*FECUNDITY*OMEGA*N^Q*(THETAE+PHIE*MUED)*(MUIW+MUID+ALPHA)-SIGMA*FECUNDITY*OMEGA*EPSILON*N^Q*(THETAI+PHII*MUID+PHIX*ALPHA)
  E_MUS_denominator=MUS^2*ETA*N^Q*(MUL+OMEGA)*(MUEW+MUED+EPSILON)*(MUIW+MUID+ALPHA)
  E_muS=E_MUS_numerator/E_MUS_denominator
  
  E_MUL_numerator=(MUL/EpizooticPotential)*-1*GAMMAE*FECUNDITY*OMEGA*ETA*(MUIW+MUID+ALPHA)-GAMMAI*FECUNDITY*OMEGA*EPSILON*ETA-SIGMA*FECUNDITY*OMEGA*N^Q*(THETAE+PHIE*MUED)*(MUIW+MUID+ALPHA)-SIGMA*FECUNDITY*OMEGA*EPSILON*N^Q*(THETAI+PHII*MUID+PHIX*ALPHA)
  E_MUL_denominator=MUS*ETA*N^Q*(MUL+OMEGA)^2*(MUEW+MUED+EPSILON)*(MUIW+MUID+ALPHA)
  E_muL=E_MUL_numerator/E_MUL_denominator
  
  E_MUED_numerator=(MUED/EpizooticPotential)*-1*GAMMAE*FECUNDITY*OMEGA*ETA*(MUIW+MUID+ALPHA)-GAMMAI*FECUNDITY*OMEGA*EPSILON*ETA+SIGMA*FECUNDITY*OMEGA*N^Q*(PHIE*(MUEW+MUED+EPSILON)-THETAE-PHIE*MUED)*(MUIW+MUID+ALPHA)-SIGMA*FECUNDITY*OMEGA*EPSILON*N^Q*(THETAI+PHII*MUID+PHIX*ALPHA)
  E_MUED_denominator=MUS*ETA*N^Q*(MUL+OMEGA)*(MUEW+MUED+EPSILON)^2*(MUIW+MUID+ALPHA)
  E_muEd=E_MUED_numerator/E_MUED_denominator
  
  E_MUEW_numerator=(MUEW/EpizooticPotential)*-1*ETA*GAMMAE*FECUNDITY*OMEGA*(MUIW+MUID+ALPHA)-ETA*GAMMAI*FECUNDITY*OMEGA*EPSILON-SIGMA*FECUNDITY*OMEGA*N^Q*(THETAE+PHIE*MUED)*(MUIW+MUID+ALPHA)-SIGMA*FECUNDITY*OMEGA*EPSILON*N^Q*(THETAI+PHII*MUID+PHIX*ALPHA)
  E_MUEW_denominator=MUS*ETA*N^Q*(MUL+OMEGA)*(MUEW+MUED+EPSILON)^2*(MUIW+MUID+ALPHA)
  E_muEw=E_MUEW_numerator/E_MUEW_denominator
  
  E_MUID_numerator=(MUID/EpizooticPotential)*-1*GAMMAI*FECUNDITY*OMEGA*EPSILON*ETA-SIGMA*FECUNDITY*OMEGA*EPSILON*THETAI*N^Q+SIGMA*FECUNDITY*OMEGA*EPSILON*PHII*N^Q*(MUIW+ALPHA)-SIGMA*FECUNDITY*OMEGA*EPSILON*PHIX*ALPHA*N^Q
  E_MUID_denominator=MUS*ETA*N^Q*(MUL+OMEGA)*(MUEW+MUED+EPSILON)*(MUIW+MUID+ALPHA)^2
  E_muId=E_MUID_numerator/E_MUID_denominator
  
  E_MUIW_numerator=(MUIW/EpizooticPotential)*-1*GAMMAI*FECUNDITY*OMEGA*EPSILON*ETA-SIGMA*FECUNDITY*OMEGA*EPSILON*N^Q*(THETAI+PHII*MUID+PHIX*ALPHA)
  E_MUIW_denominator=MUS*ETA*N^Q*(MUL+OMEGA)*(MUEW+MUED+EPSILON)*(MUIW+MUID+ALPHA)^2
  E_muIw=E_MUIW_numerator/E_MUIW_denominator
  
  E_GAMMAE_numerator=(GAMMAE/EpizooticPotential)*FECUNDITY*OMEGA
  E_GAMMAE_denominator=MUS*N^Q*(MUL+OMEGA)*(MUEW+MUED+EPSILON)
  E_gammaE=E_GAMMAE_numerator/E_GAMMAE_denominator
  
  E_GAMMAI_numerator=(GAMMAI/EpizooticPotential)*FECUNDITY*OMEGA*EPSILON
  E_GAMMAI_denominator=MUS*N^Q*(MUL+OMEGA)*(MUEW+MUED+EPSILON)*(MUIW+MUID+ALPHA)
  E_gammaI=E_GAMMAI_numerator/E_GAMMAI_denominator
  
  E_SIGMA_numerator=(SIGMA/EpizooticPotential)*FECUNDITY*OMEGA*(THETAE+PHIE*MUED)*(MUIW+MUID+ALPHA)+FECUNDITY*OMEGA*EPSILON*(THETAI+PHII*MUID+PHIX*ALPHA)
  E_SIGMA_denominator=MUS*ETA*(MUL+OMEGA)*(MUEW+MUED+EPSILON)*(MUIW+MUID+ALPHA)
  E_sigma=E_SIGMA_numerator/E_SIGMA_denominator
  
  E_OMEGA_numerator=(OMEGA/EpizooticPotential)*FECUNDITY*MUL*(GAMMAE*ETA*(MUIW+MUID+ALPHA)+GAMMAI*EPSILON*ETA+SIGMA*N^Q*(THETAE+PHIE*MUED)*(MUIW+MUID+ALPHA)+SIGMA*EPSILON*N^Q*(THETAI+PHII*MUID+PHIX*ALPHA))
  E_OMEGA_denominator=MUS*ETA*N^Q*(MUL+OMEGA)^2*(MUEW+MUED+EPSILON)*(MUIW+MUID+ALPHA)
  E_omega=E_OMEGA_numerator/E_OMEGA_denominator
  
  E_EPSILON_numerator=(EPSILON/EpizooticPotential)*-1*GAMMAE*FECUNDITY*OMEGA*ETA*(MUIW+MUID+ALPHA)+GAMMAI*FECUNDITY*OMEGA*ETA*(MUEW+MUED)-SIGMA*FECUNDITY*OMEGA*N^Q*(THETAE+PHIE*MUED)*(MUIW+MUID+ALPHA)+SIGMA*FECUNDITY*OMEGA*N^Q*(THETAI+PHII*MUID+PHIX*ALPHA)*(MUEW+MUED)
  E_EPSILON_denominator=MUS*ETA*N^Q*(MUL+OMEGA)*(MUEW+MUED+EPSILON)^2*(MUIW+MUID+ALPHA)
  E_epsilon=E_EPSILON_numerator/E_EPSILON_denominator
  
  E_ALPHA_numerator=(ALPHA/EpizooticPotential)*-1*GAMMAI*FECUNDITY*OMEGA*EPSILON*ETA-SIGMA*FECUNDITY*OMEGA*EPSILON*THETAI*N^Q-SIGMA*FECUNDITY*OMEGA*EPSILON*PHII*MUID*N^Q+SIGMA*FECUNDITY*OMEGA*EPSILON*PHIX*N^Q*(MUIW+MUID)
  E_ALPHA_denominator=MUS*ETA*N^Q*(MUL+OMEGA)*(MUEW+MUED+EPSILON)*(MUIW+MUID+ALPHA)^2
  E_alpha=E_ALPHA_numerator/E_ALPHA_denominator
  
  E_THETAE_numerator=(THETAE/EpizooticPotential)*SIGMA*FECUNDITY*OMEGA
  E_THETAE_denominator=MUS*ETA*(MUL+OMEGA)*(MUEW+MUED+EPSILON)
  E_thetaE=E_THETAE_numerator/E_THETAE_denominator
  
  E_THETAI_numerator=(THETAI/EpizooticPotential)*SIGMA*FECUNDITY*OMEGA*EPSILON
  E_THETAI_denominator=MUS*ETA*(MUL+OMEGA)*(MUEW+MUED+EPSILON)*(MUIW+MUID+ALPHA)
  E_thetaI=E_THETAI_numerator/E_THETAI_denominator
  
  E_PHIE_numerator=(PHIE/EpizooticPotential)*SIGMA*FECUNDITY*OMEGA*MUED
  E_PHIE_denominator=MUS*ETA*(MUL+OMEGA)*(MUEW+MUED+EPSILON)
  E_phiE=E_PHIE_numerator/E_PHIE_denominator
  
  E_PHII_numerator=(PHII/EpizooticPotential)*SIGMA*FECUNDITY*OMEGA*EPSILON*MUID
  E_PHII_denominator=MUS*ETA*(MUL+OMEGA)*(MUEW+MUED+EPSILON)*(MUIW+MUID+ALPHA)
  E_phiI=E_PHII_numerator/E_PHII_denominator	
  
  E_PHIX_numerator=(PHIX/EpizooticPotential)*SIGMA*FECUNDITY*OMEGA*EPSILON*ALPHA
  E_PHIX_denominator=MUS*ETA*(MUL+OMEGA)*(MUEW+MUED+EPSILON)*(MUIW+MUID+ALPHA)
  E_phiX=E_PHIX_numerator/E_PHIX_denominator
  
  E_ETA_numerator=(ETA/EpizooticPotential)*-1*SIGMA*FECUNDITY*OMEGA*(THETAE+PHIE*MUED)*(MUIW+MUID+ALPHA)-SIGMA*FECUNDITY*OMEGA*EPSILON*(THETAI+PHII*MUID+PHIX*ALPHA)
  E_ETA_denominator=MUS*ETA^2*(MUL+OMEGA)*(MUEW+MUED+EPSILON)*(MUIW+MUID+ALPHA)
  E_eta=E_ETA_numerator/E_ETA_denominator
  
  E_Q_numerator=(Q/EpizooticPotential)*-1*log(N)*FECUNDITY*OMEGA*(GAMMAE*(MUIW+MUID+ALPHA)+GAMMAI*EPSILON)
  E_Q_denominator=MUS*(MUL+OMEGA)*(MUEW+MUED+EPSILON)*(MUIW+MUID+ALPHA)
  E_q=E_Q_numerator/E_Q_denominator
  
  # Put all into one data frame. 
  elast=data.frame(E_N,E_f,E_muS,E_muL,E_muEd,E_muEw,E_muId,E_muIw,E_gammaE,E_gammaI,E_sigma,E_omega,E_epsilon,E_alpha,E_thetaE,E_thetaI,E_phiE,E_phiI,E_phiX,E_eta,E_q)
  
  # Calculate elasticity denominator. 
  sum=abs(E_N)+abs(E_f)+abs(E_muS)+abs(E_muL)+abs(E_muEd)+abs(E_muEw)+abs(E_muId)+abs(E_muIw)+abs(E_gammaE)+abs(E_gammaI)+abs(E_sigma)+abs(E_omega)+abs(E_epsilon)+abs(E_alpha)+abs(E_thetaE)+abs(E_thetaI)+abs(E_phiE)+abs(E_phiI)+abs(E_phiX)+abs(E_eta)+abs(E_q)
  
  # Calculate final elasticity. 
  InfluencesonEpizooticPotential=round(abs(elast)/sum,digits=3)
  
  # Append InfluencesonEpizooticPotential onto Output Matrix.
  OutputMatrix=cbind(OutputMatrix,InfluencesonEpizooticPotential)
  
  # Write the Output Matrix to the attachments working directory.
  setwd("/data/attachments")
  write.csv(OutputMatrix, "EpizooticRiskModelOutput.csv",  row.names = FALSE)
  
	} # End if subadmin area removed. 

	# If all the subadmin areas are removed.
	if (ModelMatrixCompleteDim==0){
	# Tell the user that insufficient data exists. 
    	line=''
    	write(line,file=model_log_filepath,append=TRUE)
    	line='Complete Cases'
    	write(line,file=model_log_filepath,append=TRUE)
	line=paste("This model requires each sub-administrative area to have values for all inputs. While you have some data, there does not exist a single subadministrative area that contains all the required fields needed to run this model. Please return to the Warehouse and select a different set of data to analyze.")
	write(line,file=model_log_filepath,append=TRUE) 
	# Write a blank file back to the Warehouse. 
	setwd("/data/attachments")
	NULLS=as.data.frame(matrix(rep(NA,ModelMatrixDim*26),ModelMatrixDim,26,byrow=TRUE))
	colnames(NULLS)=c("SubAdminID","SubAdminName","PopulationGrowthRate","EpizooticPotential","EpizooticPotentialRank","E_N",
	               "E_f","E_muS","E_muL","E_muEd","E_muEw","E_muId","E_muIw","E_gammaE",
	               "E_gammaI","E_sigma","E_omega","E_epsilon","E_alpha","E_thetaE","E_thetaI","E_phiE","E_phiI","E_phiX","E_eta","E_q")
	write.csv(NULLS, "EpizooticRiskModelOutput.csv", row.names = FALSE)
	} # End if subadmin areas not removed. 


