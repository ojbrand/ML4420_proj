import pandas as pd
import glob

all_data = []

# Loop through every file
for json_file in glob.glob("very_active_minutes_*.json"):
    
    # 1. Extract User ID
    user_id = json_file.split('_')[-1].replace('.json', '')
    csv_file = f"reporting_{user_id}.csv"
    
    # 2. Load Activity Data (Input)
    df_act = pd.read_json(json_file)
    df_act = df_act.rename(columns={'dateTime': 'Date', 'value': 'VeryActiveMinutes'})
    df_act['Date'] = pd.to_datetime(df_act['Date'])

    # 3. Load Weight Data (Target)
    # dayfirst=True fixes European date format (DD/MM/YYYY) in CSV
    df_weight = pd.read_csv(csv_file)
    df_weight = df_weight.rename(columns={'date': 'Date', 'weight': 'WeightKg'})
    df_weight['Date'] = pd.to_datetime(df_weight['Date'], dayfirst=True)

    # 4. Merge tables on Date
    # outer join to keep days with have activity but no weight log + vice versa
    merged = pd.merge(df_act, df_weight[['Date', 'WeightKg']], on='Date', how='outer')
    merged = merged.sort_values('Date')

    # 5. Fix Missing Data
    # Fill weight gaps and set missing Activity to 0
    merged['WeightKg'] = merged['WeightKg'].interpolate(method='linear', limit_direction='both')
    merged['VeryActiveMinutes'] = merged['VeryActiveMinutes'].fillna(0)
    merged['User_ID'] = user_id

    all_data.append(merged)

# 6. Combine all users and save
final_df = pd.concat(all_data, ignore_index=True)
final_df.to_csv("final_training_database.csv", index=False)

print(f"Saved {len(final_df)} rows to 'final_training_database.csv'.")