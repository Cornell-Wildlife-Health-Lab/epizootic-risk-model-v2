'''
Script Name: Epizootic Risk Model Input Processing
Author: Nicholas Hollingshead, Cornell University
Description: Prepares data from the CWD Data Warehouse for the Epizootic
             Risk Model R script.
Inputs: 
  sample.ndjson
  params.json
  demography_deer_density.json (optional)
  demography_fecundity.json (optional)
  demography_harvest_mortality.json (optional)
  natural_mortality.json (optional)
Outputs: 
  samples.csv
  params.csv
  demography_deer_density.csv (optional)
  demography_fecundity.csv (optional)
  demography_harvest_mortality.csv (optional)
  natural_mortality.csv (optional)
  info.html
  execution_log.log
Date Created: 2024-08-19
Date Modified: 2024-08-19
Version: 1.0.0
'''

##############
# Environment
import sys
import os
import ndjson
import json
import pathlib
import csv
import logging
import datetime

##################
# SCRIPT VARIABLES
parameters_file = '/data/params.json'
sample_file_path = "/data/sample.ndJson"
demography_file_paths = [
  "/data/demography_deer_density.json",
  "/data/demography_fecundity.json",
  "/data/demography_harvest_mortality.json",
  "/data/demography_natural_mortality.json"
  ]
subadmins_filepath = "/data/sub_administrative_area.ndJson"
model_metadata_log_file = '/data/attachments/info.html'
logging_path = '/data/attachments/execution_log.log'

###################
# FUNCTIONS

def model_log(line='', filename=model_metadata_log_file):
  """Writes a single line to the model_metadata_log text file.
  Args:
    line: The line to be written.
    filename: The name of the file.
  """
  with open(filename, 'a') as f:
    f.write(line + '<br>' + '\n')

def model_log_html(line='', html_element="p", filename=model_metadata_log_file):
    """
    Writes a single line to the model_metadata_log text file with specified HTML element.

    Args:
        line: The line to be written.
        filename: The name of the file.
        html_element: The HTML element tag to use (e.g., "h1", "h2", "p", "div").
    """
    with open(filename, 'a') as f:
        f.write(f"<{html_element}>{line}</{html_element}>" + '\n') 

def json_stringify(data, indent=3):
  """Custom formats a nested dictionary into a string with spaces for indentation.

  Args:
    data: The nested dictionary.
    indent: The number of spaces for indentation.

  Returns:
    A formatted string.
  """

  def format_helper(data, level):
    lines = []
    for key, value in data.items():
      if isinstance(value, dict):
        lines.append(f"{(' ' * level)}{key}:")
        lines.extend(format_helper(value, level + indent))
      else:
        lines.append(f"{(' ' * level)}{key}: {value}")
    return lines

  return '\n'.join(format_helper(data, indent))


def json_stringify_html(data, indent=3):
  """Custom formats a nested dictionary into a string with spaces for indentation and html breaks.

  Args:
    data: The nested dictionary.
    indent: The number of spaces for indentation.

  Returns:
    A formatted string.
  """

  def format_helper(data, level):
    lines = []
    for key, value in data.items():
      if isinstance(value, dict):
        lines.append(f"{(' ' * level)}{key}:<br>")
        lines.extend(format_helper(value, level + indent))
      else:
        lines.append(f"{(' ' * level)}{key}: {value}<br>")
    return lines

  return '\n'.join(format_helper(data, indent))

def dict_to_html_list(data, list_type='unordered'):
  """
  Converts a Python dictionary to an HTML string representing a list.

  Args:
    data: The input dictionary.
    list_type: 'unordered' (default) or 'ordered' to specify the list type.

  Returns:
    An HTML string representing the dictionary.
  """

  def _dict_to_html_helper(data):
    """Recursive helper function to handle nested dictionaries."""
    html_str = ""
    if list_type == 'unordered':
      html_str += "<ul>"
    elif list_type == 'ordered':
      html_str += "<ol>"
    else:
      raise ValueError("Invalid list_type. Use 'unordered' or 'ordered'.")

    for key, value in data.items():
      html_str += f"<li>{key}: "
      if isinstance(value, dict):
        html_str += _dict_to_html_helper(value)
      elif isinstance(value, list):
        html_str += "<ul>"
        for item in value:
          html_str += f"<li>{item}</li>"
        html_str += "</ul>"
      else:
        html_str += f"{value}"
      html_str += "</li>"

    if list_type == 'unordered':
      html_str += "</ul>"
    elif list_type == 'ordered':
      html_str += "</ol>"

    return html_str

  return _dict_to_html_helper(data)

def rename_key(dict_, old_key, new_key):
  """Renames a key in a dictionary.

  Args:
    dict_: The dictionary to modify.
    old_key: The key to be renamed.
    new_key: The new key name.
  """

  if old_key in dict_:
    dict_[new_key] = dict_.pop(old_key)

def add_item_to_json_file_list(file_path, new_item):
  """
  Adds a new item to the list within a JSON file.

  Args:
    file_path: Path to the JSON file.
    new_item: The item to be added to the list.

  Raises:
    FileNotFoundError: If the specified file does not exist.
    json.JSONDecodeError: If the file content is not valid JSON.
  """

  try:
    with open(file_path, 'r') as f:
      data = json.load(f)

    if isinstance(data, list):
      data.append(new_item)
    else:
      raise ValueError("The JSON file does not contain a list.")

    with open(file_path, 'w') as f:
      json.dump(data, f, indent=2) 

  except FileNotFoundError:
    print(f"Error: File '{file_path}' not found.")
    raise
  except json.JSONDecodeError:
    print(f"Error: Invalid JSON in '{file_path}'.")
    raise
  except ValueError as e:
    print(f"Error: {e}")
    raise

######################
# SETUP FILE STRUCTURE

# Create the attachments directory structure recursively if it doesn't already exist.
os.makedirs(os.path.dirname(pathlib.Path(model_metadata_log_file)), exist_ok=True)

# Create attachments.json file which will contain a list of all attachments generated
# Initially, the attachments is simply an empty list
attachments_json_path = pathlib.Path("/", "data", "attachments.json")
with open(attachments_json_path, 'w', newline='') as f:
  writer = json.dump(list(), f)

# Append execution log to attachments.json for developer feedback
attachment = {
  "filename": "execution_log.log", 
  "content_type": "text/plain", 
  "role": "downloadable"
  }
add_item_to_json_file_list(attachments_json_path, attachment)

# append info log to the attachments.json for user feedback
attachment = {
  "filename": "info.html", 
  "content_type": "text/html", 
  "role": "feedback"}
add_item_to_json_file_list(attachments_json_path, attachment)

###################
# SETUP LOGGING

# Create log file including any parent folders (if they don't already exist)
os.makedirs(os.path.dirname(pathlib.Path(logging_path)), exist_ok=True)

logging.basicConfig(level = logging.DEBUG, # Alternatively, could use DEBUG, INFO, WARNING, ERROR, CRITICAL
                    filename = logging_path, 
                    filemode = 'w', # a is append, w is overbite
                    datefmt = '%Y-%m-%d %H:%M:%S',
                    format = '%(asctime)s - %(levelname)s - %(message)s')

# Uncaught exception handler
def handle_uncaught_exception(type, value, traceback):
  logging.error(f"{type} error has occurred with value: {value}. Traceback: {traceback}")
sys.excepthook = handle_uncaught_exception

## Initiate model metadata log

# Clear model log file contents if necessary.
open(pathlib.Path(model_metadata_log_file), 'w').close()
model_log_html("Model Execution Summary", "h3")
model_log_html("Model: Epizootic Risk Model")
model_log_html('Date: ' + datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S") + ' GMT', "p")
logging.info("Model: Epizootic Risk Model")
logging.info('Date: ' + datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S") + ' GMT')
logging.info("This log records data for debugging purposes in the case of a model execution error.")

######
# MAIN

####################
# Process Parameters
# Required

# Get model parameters file
try:
  with open(pathlib.Path(parameters_file), 'r') as f:
    params = json.load(f)
    logging.info("Parameters json file loaded successfully")
except:
  # The model cannot be executed without a params file. Exit with an error immediately.
  logging.error("params.json File does not exist.")
  model_log_html("ERROR", "h4")
  model_log_html("Parameters (params.json) file not found. Execution halted.", "p")
  sys.exit(1)
  
# Get provider admin area
provider_admin_area = params['_provider']['_administrative_area']['administrative_area']
# Remove Provider parameter, which is not used in this model and is nested
del(params['_provider'])

# Merge the list of season_year values into a single string
if 'season_year' in params:
  params['season_year'] = ', '.join(params['season_year'])

### Adding and editing params to fit revised model
### Section can be removed after corresponding Warehouse UI and data processing updates.
if 'omega' not in params:
  params['omega'] = 4
if 'epsilon' not in params:
  params['epsilon'] = 0.8
if 'alpha' not in params:
  params['alpha'] = 1
if 'deer_density_value' in params: 
  rename_key(params, 'deer_density_value', 'density_value')
if 'transmission_via_exposed_deer' in params:
  rename_key(params, 'transmission_via_exposed_deer', 'transmission_via_subclinical_deer')
if 'transmission_via_infectious_deer' in params:
  rename_key(params, 'transmission_via_infectious_deer', 'transmission_via_clinical_deer')
# Rename for file references
if "_demography_deer_density" in params:
  rename_key(params, '_demography_deer_density', 'demography_density_id')
if "_demography_density" in params:
  rename_key(params, '_demography_density', 'demography_density_id')
if "_demography_fecundity" in params:
  rename_key(params, '_demography_fecundity', 'demography_fecundity_id')
if "_demography_harvest_mortality" in params:
  rename_key(params, '_demography_harvest_mortality', 'demography_harvest_mortality_id')
if "_demography_natural_mortality" in params:
  rename_key(params, '_demography_natural_mortality', 'demography_natural_mortality_id')
###
  
# Write revised parameters to a CSV file
with open(pathlib.Path("/data/params.csv"), 'w', newline='') as f:
  field_names = params.keys()
  writer = csv.DictWriter(
    f, 
    quoting=csv.QUOTE_NONNUMERIC,
    fieldnames=field_names, 
    extrasaction='ignore')
  writer.writeheader()
  writer.writerow(params)

# Add parameter related content to the log
model_log_html('Provider area: ' + provider_admin_area)
model_log_html("Model Parameters", "h4")
model_log_html(dict_to_html_list(params))

#################
# Process Samples
# Required

try:
  with open(pathlib.Path(sample_file_path), 'r') as f:
    sample_data = ndjson.load(f)
    logging.info("samples ndjson loaded successfully")

except:
  logging.error("samples.ndjson file does not exist.")
  model_log_html("<h2>ERROR", "h2")
  model_log_html("ERROR: Samples (sample.ndjson) file not found. Sample data are required to run this model. Execution halted.")
  sys.exit(1)

# Log number of samples
model_log_html("Warehouse data provided to model", "h4")
model_log_html("Samples: " + str(len(sample_data)))

for sample_record in sample_data:

  ## For each sample, get the definitive test result
  
  # Create a list of tests that are flagged as selected_definitive (should have length 0 or 1)
  tests_selected_definitive = [test for test in sample_record["tests"] if test["selected_definitive"] == True]
  
  if len(tests_selected_definitive) == 0: # no match means no tests so set to None
    sample_record["result"] = None
  elif len(tests_selected_definitive) > 1: # more than one match - problem
    # This would be a problem and should never happen, though we could write error handling
    pass
  elif len(tests_selected_definitive) == 1: # one match so use that test
    # result is a required field; but check if has value. If so, use it, else set to None
    sample_record["result"] = tests_selected_definitive[0]["result"] if "result" in tests_selected_definitive[0] else None
  
  # Get sub-admin area for each sample
  # 
  # If the sample has no sub-administrative area or if the sub-administrative
  # area has no id, then return None. 
  if '_sub_administrative_area' in sample_record and sample_record['_sub_administrative_area'] is not None:
    if '_id' in sample_record['_sub_administrative_area'] and sample_record['_sub_administrative_area']['_id'] is not None:
      sample_record["sub_administrative_area_id"] = sample_record['_sub_administrative_area']['_id']
    else:sample_record["sub_administrative_area_id"] = None
  else:sample_record["sub_administrative_area_id"] = None
  
  # Rename _id field
  if '_id' in sample_record:
    rename_key(sample_record, '_id', 'id')
  
# Write to a CSV
with open(pathlib.Path("/data/sample.csv"), 'w', newline='') as f:
  writer = csv.DictWriter(
    f, 
    quoting=csv.QUOTE_NONNUMERIC,
    fieldnames=["id", "result", "sub_administrative_area_id"], 
    extrasaction='ignore')
  writer.writeheader()
  writer.writerows(sample_data)

##################################
# Process Sub-administrative areas
# Required

subadmins = list()
try:
  with open(pathlib.Path(subadmins_filepath), 'r') as f:
    subadmins = ndjson.load(f)
except:
  # The model cannot be executed without a sub_administrative areas file. Exit with an error immediately.
  logging.error("sub_administrative_area.json File does not exist.")
  model_log_html("ERROR", "h4")
  model_log_html("ERROR: Sub-administrative areas (sub_administrative_area.json) file not found. Execution halted.")
  sys.exit(1)

for subadmin in subadmins:
  # rename id key
  if '_id' in subadmin:
    rename_key(subadmin, '_id', 'id')
  # For each subadmin area, convert list of adjacent admin areas to a single string
  if '_adjacent' in subadmin and subadmin['_adjacent'] is not None:
    subadmin['adjacent_id'] = ', '.join(subadmin['_adjacent'])
  else: subadmin['adjacent_id'] = None
  # for each subadmin area, convert list of adjacent admin areas into strings
  subadmin['adjacent_administrative_area_id'] = list() # Add a list to hold adjacent admin IDs
  subadmin['adjacent_administrative_area_short_name'] = list() # Add a list to hold adjacent admin IDs
  adj_admin_area_ids = list()
  adj_admin_area_short_names = list()
  if '_adjacent_administrative_area' in subadmin and subadmin['_adjacent_administrative_area'] is not None:
    for each in subadmin['_adjacent_administrative_area']:
      adj_admin_area_ids.append(each["_id"])  
      adj_admin_area_short_names.append(each["administrative_area"])
  subadmin['adjacent_administrative_area_id'] = ', '.join(adj_admin_area_ids)
  subadmin['adjacent_administrative_area_short_name'] = ', '.join(adj_admin_area_short_names)
  subadmin['administrative_area_id'] = subadmin["_administrative_area"]["_id"]
  subadmin['administrative_area_short_name'] = subadmin["_administrative_area"]["administrative_area"]
  subadmin['administrative_area_name'] = subadmin["_administrative_area"]["name"]
     
# Write to a CSV
with open(pathlib.Path("/data/sub_administrative_area.csv"), 'w', newline='') as f:
  writer = csv.DictWriter(
    f, 
    quoting = csv.QUOTE_NONNUMERIC,
    fieldnames = ["id", 
                  "code", 
                  "name", 
                  "full_name", 
                  "aland", 
                  "administrative_area_id", 
                  "administrative_area_name", 
                  "administrative_area_short_name", 
                  "adjacent_administrative_area_id", 
                  "adjacent_administrative_area_short_name", 
                  "adjacent_id"],
    extrasaction='ignore')
  writer.writeheader()
  writer.writerows(subadmins)

 
##################
# Demography files
# Not required. Process them if they exist.

model_log_html("Demographic data:")

demography_file_provided = False # Set a dummy variable to false. 
                                 # Change to true (below) if at least one exists.
                                 # If still False after checking for any demography files, 
                                 # write to model_log that none were provided.

for demographic_file_path in demography_file_paths:
  # Check if file exists
  if pathlib.Path(demographic_file_path).exists():
    demography_file_provided = True
    # Open the source ndjson file and load as a list of dictionaries
    with open(demographic_file_path, 'r') as f:
      demographic_data = json.load(f)

    # Log demographic file
    model_log_html('   ' + demographic_data["species"] + " " + demographic_data["metric"] + " " + demographic_data["season_year"])

    # Create a list to hold the demographic data
    list_demographic_data = [{"sub_administrative_area_id": key, "value": value} for key, value in demographic_data['data'].items()]
    
    demographic_file_name = pathlib.Path(demographic_file_path).stem
    # TEMP for transition to new model version
    if demographic_file_name == 'demography_deer_density':
      demographic_file_name = 'demography_density'
    # END TEMP
    
    logging.info(f"Demography file {demographic_file_name} loaded successfully")
    
    # Write the demographic data to a file
    filepath = pathlib.Path("/data", demographic_file_name + ".csv")
    with open(filepath, 'w', newline='') as f:
      writer = csv.DictWriter(
        f,
        quoting=csv.QUOTE_NONNUMERIC,
        fieldnames=["sub_administrative_area_id", "value"], 
        extrasaction='ignore')
      writer.writeheader()
      writer.writerows(list_demographic_data)
  else:
    # Demography files are not required. If one doesn't exist, go to the next.
    pass

if demography_file_provided == False:
  model_log_html("None provided")