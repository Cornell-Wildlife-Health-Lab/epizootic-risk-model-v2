#!/bin/bash
python3 "/app/scripts/input_processing.py" && \
Rscript "/app/scripts/EpizooticRiskModel_Warehouse.R" && \
python3 "/app/scripts/output_processing.py" 