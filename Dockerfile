FROM rocker/tidyverse:4.4.0

# Install Python
RUN apt-get update && apt-get install -y python3 python3-pip

# Make a folder to hold model files
RUN mkdir /app

# Copy your model files
COPY src /app

# Install python packages as defined in a requirements file
RUN pip install -r /app/software/requirements.txt

# Change working directory
WORKDIR /app

CMD ["/bin/sh", "./scripts/run_scripts.sh"]