"""Generate Figure 8: Income subgroup bar chart with Q1->Q5 order"""
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import json, urllib.request, os

# Step 1: Load country-to-income classification
# Try downloading World Bank income groups, fall back to saved file
income_file = r'C:\Users\dsc88\Desktop\NTD_China_GBD2021\data\wb_income.json'
if not os.path.exists(income_file):
    url = "https://api.worldbank.org/v2/country?per_page=300&format=json"
    try:
        with urllib.request.urlopen(url, timeout=15) as resp:
            data = json.loads(resp.read())
        # Save for reuse
        with open(income_file, 'w') as f:
            json.dump(data, f)
        print("Downloaded WB income data")
    except Exception as e:
        print(f"Download failed: {e}")
        income_file = None

# Build income lookup from downloaded data
income_lookup = {}
if income_file and os.path.exists(income_file):
    with open(income_file) as f:
        data = json.load(f)
    for c in data[1]:
        code = c.get('id', '')
        income = c.get('incomeLevel', {}).get('value', '')
        name = c.get('name', '')
        if income and code:
            income_lookup[code] = (name, income)
    print(f"Loaded {len(income_lookup)} countries")

# Step 2: Load GBD data for 2023
df = pd.read_csv(r'C:\Users\dsc88\Desktop\NTD_China_GBD2021\data\IHME-GBD_2023_DATA-04b137c8-1.csv')
ntd = df[(df['cause_name'] == 'Neural tube defects') &
         (df['metric_name'] == 'Rate') &
         (df['sex_name'] == 'Both') &
         (df['age_name'] == 'Age-standardized') &
         (df['year'] == 2023)].copy()

# Load ISO3 mapping
iso3_map = pd.read_csv(r'C:\Users\dsc88\Desktop\NTD_China_GBD2021\data\iso3_mapping.csv')

# Load urbanization quintile data
urb = pd.read_csv(r'C:\Users\dsc88\Desktop\NTD_China_GBD2021\output\table3_countries_2023.csv')

# Merge GBD with urbanization
m = ntd.merge(urb, left_on='location_name', right_on='Country')
m = m.dropna(subset=['Urban_Group']).copy()

# Add ISO3
m['iso3'] = m['location_name'].map(iso3_map.set_index('gbd_name')['iso3'])

# Add income group
def get_income(loc):
    iso = iso3_map[iso3_map['gbd_name'] == loc]['iso3'].values
    if len(iso) == 0:
        return None
    info = income_lookup.get(iso[0], None)
    if info:
        return info[1]
    return None

m['income'] = m['location_name'].apply(get_income)
print(f"Matched income for {m['income'].notna().sum()}/{len(m)} countries")
print(m['income'].value_counts())

# Map WB income labels to our groups
inc_map = {
    'Low income': 'Low',
    'Lower middle income': 'Lower_middle',
    'Upper middle income': 'Upper_middle',
    'High income': 'High'
}
m['Income_Group'] = m['income'].map(inc_map)
missing = m[m['Income_Group'].isna()]
if len(missing) > 0:
    print(f"Missing income groups: {missing['location_name'].tolist()}")

# Step 3: Compute mean + SD per subgroup
m = m.dropna(subset=['Income_Group'])
result = m.groupby(['Urban_Group', 'Income_Group']).agg(
    N=('val', 'count'),
    Mean_Rate=('val', 'mean'),
    SD_Rate=('val', 'std')
).reset_index()
print(result.to_string())

# Compare with existing table6
existing = pd.read_csv(r'C:\Users\dsc88\Desktop\NTD_China_GBD2021\output\table6_income_subgroup.csv')
print("\nMean diff (ours - existing):")
merged = result.merge(existing, on=['Urban_Group','Income_Group'], suffixes=('_new','_old'))
merged['diff'] = merged['Mean_Rate_new'] - merged['Mean_Rate_old']
print(merged[['Urban_Group','Income_Group','Mean_Rate_new','Mean_Rate_old','diff']].to_string())

# Step 4: Plot
grps = ['Q1_Lowest', 'Q2_Low', 'Q3_Middle', 'Q4_High', 'Q5_Highest']
inc_grps = ['Low', 'Lower_middle', 'Upper_middle', 'High']
cols = ['#2166AC', '#67A9CF', '#D1E5F0', '#F4A582', '#B2182B']

fig, ax = plt.subplots(figsize=(12, 7))
n_inc = len(inc_grps)
n_urb = len(grps)
width = 0.7 / n_urb  # bar width within each income group

for i, urb_g in enumerate(grps):
    vals = []
    errs = []
    for inc_g in inc_grps:
        row = result[(result['Urban_Group'] == urb_g) & (result['Income_Group'] == inc_g)]
        if len(row) > 0 and not pd.isna(row['Mean_Rate'].values[0]):
            vals.append(row['Mean_Rate'].values[0])
            errs.append(row['SD_Rate'].values[0])
        else:
            vals.append(0)
            errs.append(0)
    x = np.arange(n_inc) + (i - n_urb/2 + 0.5) * width
    ax.bar(x, vals, width, label=urb_g, color=cols[i], yerr=errs, capsize=3, edgecolor='white', linewidth=0.5)

ax.set_xticks(np.arange(n_inc))
ax.set_xticklabels(inc_grps)
ax.set_ylabel('NTD DALYs per 100,000')
ax.set_xlabel('World Bank Income Group')
ax.set_title('NTD DALY Rate by Urbanisation Quintile and Income Group, 2023')
ax.legend(title='Urbanisation Quintile', loc='upper right')
ax.set_ylim(bottom=0)

out = r'C:\Users\dsc88\Desktop\NTD_China_GBD2021\output\fig11_income_subgroup.png'
plt.tight_layout()
plt.savefig(out, dpi=300)
plt.close()
print(f"\nSaved to {out}")
