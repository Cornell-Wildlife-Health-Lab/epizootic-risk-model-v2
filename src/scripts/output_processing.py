'''
Script Name: Epizootic Risk Model Output Processing
Author: Nicholas Hollingshead, Cornell University
Description: Converts output files form the Epizootic Risk Model
             to file formats appropriate for the CWD Data Warehouse.
Inputs: 
  EpizooticRiskModelOutput.csv
Outputs: 
  attachments.json
  output.json
Date Created: 2024-08-19
Date Modified: 2024-08-19
Version: 1.0
'''


import pathlib
import json
import csv
import os
import sys
import logging

model_metadata_log_file = '/data/attachments/info.txt'
logging_path = '/data/attachments/execution_log.log'

################### 
# Functions

def model_log(line='', filename=model_metadata_log_file):
  """Writes a single line to the model_metadata_log text file.
  Args:
    line: The line to be written.
    filename: The name of the file.
  """
  with open(filename, 'a') as f:
    f.write(line + '\n')


################
# LOGGING CONFIG

logging.basicConfig(level = logging.DEBUG, # Alternatively, could use DEBUG, INFO, WARNING, ERROR, CRITICAL
                    filename = '/data/attachments/execution_log.log', 
                    filemode = 'a', # a is append, w is overwite
                    datefmt = '%Y-%m-%d %H:%M:%S',
                    format = '%(asctime)s - %(levelname)s - %(message)s')

#######
# MAIN

model_log("\n-----------------")
model_log("Model Exports")
model_log("")

## Convert the ERM output to a JSON
### First read in the data              
ERM_output_csv = '/data/attachments/EpizooticRiskModelOutput.csv'
ERM_output_dictlist = list()
with open(pathlib.Path(ERM_output_csv), 'r') as f:
  csvrdr = csv.DictReader(f, quoting = csv.QUOTE_NONNUMERIC)
  ERM_output_dictlist = list(csvrdr)

# Data read via csv.DictReader reads all fields as text.
# Convert fields that are numbers from strings to numbers.
float_fields = ("PopulationGrowthRate","EpizooticPotential","EpizooticPotentialRank","E_N","E_f","E_muS","E_muL","E_muEd","E_muEw","E_muId","E_muIw","E_gammaE","E_gammaI","E_sigma","E_omega","E_epsilon","E_alpha","E_thetaE","E_thetaI","E_phiE","E_phiI","E_phiX","E_eta","E_q")
try:
  for each_row in ERM_output_dictlist:
    for each_field in float_fields:
      each_row[each_field] = float(each_row[each_field])
except Exception as e:
  # The model cannot be executed without a params file. Exit with an error immediately.
  logging.exception("Failed to convert model output field value to floating point.")
  model_log("ERROR: Failed to create output file.")
  sys.exit(1)

# Write to JSON file
ERM_output_jsonpath = "/data/attachments/output.json"
with open(pathlib.Path(ERM_output_jsonpath), 'w', newline='') as f:
    writer = json.dump(ERM_output_dictlist, f)
    
# Generate the Output Matrix Attachment
attachments = list()
attachments.append({"filename": "EpizooticRiskModelInput.csv", "content_type": "text/csv", "role": "downloadable"})
attachments.append({"filename": "EpizooticRiskModelOutput.csv", "content_type": "text/csv", "role": "downloadable"})
attachments.append({"filename": "execution_log.log", "content_type": "text/csv", "role": "downloadable"})
attachments.append({"filename": "info.txt", "content_type": "text/csv", "role": "downloadable"})
attachments.append({"filename": "output.json", "content_type": "application/json", "role": "primary"})

with open(pathlib.Path("/data/attachments.json"), 'w', newline='') as f:
  writer = json.dump(attachments, f)
  
model_log("Model exports successfully created.")