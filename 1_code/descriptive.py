import os

# # Get the current script directory and create paths to the data and output folders
script_dir = os.path.dirname(os.path.abspath(__file__))
data_folder_path = os.path.join(os.path.dirname(script_dir),'0_data')
pipeline_folder_path = os.path.join(os.path.dirname(script_dir),'2_pipeline/preprocessed')
tmp_folder_path =os.path.join(os.path.dirname(script_dir), '2_pipeline/tmp')


# read xlsx file, complete sheet
import pandas as pd
# create dir if not exists
if not os.path.exists(tmp_folder_path):
    os.makedirs(tmp_folder_path)

d = pd.read_excel(os.path.join(data_folder_path, 'combined_groups_alldata_LU_update.xlsx'), sheet_name="complete")
d.to_csv(os.path.join(tmp_folder_path, 'experience.csv'), index=False)

dfintake = pd.read_excel(os.path.join(data_folder_path, 'combined_groups_alldata_LU_update.xlsx'),sheet_name="no empty entries")
dfintake.to_csv(os.path.join(tmp_folder_path, 'intake.csv'), index=False)