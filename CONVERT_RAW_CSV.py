import pandas as pd

def transform(file_path, file_type):
    # Split the summary into lines
    raw_result = pd.read_csv(file_path, delimiter='\t')
    summary_lines = raw_result.values.tolist()
    
    # Find the start and end of the coefficient table
    try:
        if file_type not in ['python', 'r']:
            raise ValueError("The file type must be 'python' or 'r'")
        if file_type == 'python':
            start_index = summary_lines.index(['-------------------------------------------------------------------------------------------------------------------']) + 1
            end_index = summary_lines.index(['==================================================================================================================='], start_index)
            
            output_name = 'model_summary_python.csv'
        elif file_type == 'r':
            start_index = summary_lines.index(['Estimate Std. Error z value Pr(>|z|)']) + 1
            end_index = summary_lines.index(['---'])
            
            output_name = 'model_summary_r.csv'
        else: 
            print('Warning: wrong format indicator!')
        
        coef_table_lines = summary_lines[start_index:end_index]
        
        # Prepare data for CSV
        data_for_csv = []
        header = ['Variable', 'Coefficient', 'Std. Error', 'z-Value', 'P-Value']
        data_for_csv.append(header)
        
        for line in coef_table_lines:
            if line[0].strip():  # Avoid empty lines
                parts = line[0].split()
                variable = parts[0]
                coefficient = parts[1]
                std_error = parts[2]
                z_value = parts[3]
                p_value = parts[4]
                data_for_csv.append([variable, coefficient, std_error, z_value, p_value])
        
        # Create DataFrame and save to CSV
        df_summary = pd.DataFrame(data_for_csv[1:], columns=data_for_csv[0])
        df_summary.to_csv(output_name, index=False)
        
        print("Summary saved to " + output_name)
    
    except ValueError as e:
        print(f'Error: {e}')
        
        
python_file_path  = "model_summary_python_raw.txt"
# r_file_path = "model_summary_R_raw.txt"

transform(python_file_path, 'python')
# transform(r_file_path, 'r')
