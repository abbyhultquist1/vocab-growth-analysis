import pandas as pd
import numpy as np
from scipy import stats


#Cleaning CDI Data
CDI_raw = pd.read_csv("/Users/abbyhultquist/Documents/First Year Project/CDI_raw.csv")
CDI = CDI_raw.copy()

print("Initial CDI shape:", CDI.shape)
CDI = CDI.sort_values(['child_id', 'session_num'])



#creating a list of ALL words 
word_cols = CDI.columns[19:].tolist()
metadata_cols = CDI.columns[:19].tolist()

print("total # words considered:", len(word_cols))

# Calculate initial vocab_size
CDI['vocab_size'] = CDI[word_cols].sum(axis=1)
print("Initial vocab_size calculated")




# Remove children with less than 4 sessions
session_counts = CDI.groupby('child_id')['session_num'].count()
valid_children = session_counts[session_counts >= 4].index
CDI = CDI[CDI['child_id'].isin(valid_children)]

print(f"After removing children with <4 sessions: {CDI.shape}")
print(f"Removed {len(session_counts) - len(valid_children)} children with < 4 sessions")


# Find and remove outliers
outliers_sessions = set(CDI[CDI['session_num'] > 12]['child_id'])
outliers_vocab = set(CDI[CDI['vocab_size'] > len(word_cols)]['child_id'])
z_scores = stats.zscore(CDI['vocab_size'])
outliers_zscore = set(CDI[np.abs(z_scores) > 3]['child_id'])

all_outliers = outliers_sessions | outliers_vocab | outliers_zscore
# Find and remove outliers
outliers_sessions = set(CDI[CDI['session_num'] > 12]['child_id'])
outliers_vocab = set(CDI[CDI['vocab_size'] > len(word_cols)]['child_id'])
# Explicit vocabulary cutoff to remove extreme values
vocab_cutoff = 700
outliers_cutoff = set(CDI[CDI['words_spoken'] > vocab_cutoff]['child_id'])
z_scores = stats.zscore(CDI['vocab_size'])
outliers_zscore = set(CDI[np.abs(z_scores) > 3]['child_id'])

all_outliers = outliers_sessions | outliers_vocab | outliers_zscore | outliers_cutoff

print(f"Removed {len(all_outliers)} additional outlier children:")
print(f"  - {len(outliers_sessions)} for session_num > 12")
print(f"  - {len(outliers_vocab)} for vocab_size > # words in CDI")
print(f"  - {len(outliers_zscore)} for zscore > 3 on vocab_size")
print(f"  - {len(outliers_cutoff)} for vocab_size > {vocab_cutoff} (explicit cutoff)")

CDI = CDI[~CDI['child_id'].isin(all_outliers)].copy()

print(f"After removing outliers: {CDI.shape}")






#Once a word is learned, it stays learned
grouped = CDI.groupby('child_id')
for child_id, group in grouped:
    group = group.sort_values('session_num')
    for word in word_cols:
        # Find first session where word is known
        known_sessions = group[group[word] == 1]['session_num']
        if not known_sessions.empty:
            first_known = known_sessions.min()
            # Set word as known from first_known onwards
            CDI.loc[(CDI['child_id'] == child_id) & (CDI['session_num'] >= first_known), word] = 1
# Update words_spoken to reflect the cumulative knowledge
CDI['words_spoken'] = CDI[word_cols].sum(axis=1)

#check maximum vocab_size after cumulative knowledge enforcement
print()
print("After enforcing cumulative knowledge")
print("Sample vocab_size stats:", CDI['vocab_size'].describe())
print("Sample words_spoken stats:", CDI['words_spoken'].describe())
print(max(CDI['vocab_size']), max(CDI['words_spoken']))

# Also update vocab_size
CDI['vocab_size'] = CDI['words_spoken']

print()
print("After ensuring cumulative knowledge and recalculating vocab_size")
print("Sample vocab_size stats:", CDI['vocab_size'].describe())
print("Updated words_spoken to match cumulative vocab_size")




# Categorize based on LT status at time 1 (first session) and time 12 (last session)
CDI["Talker_Type"] = ""

grouped = CDI.groupby('child_id')

for child_id, group in grouped:
    first_session = group['session_num'].min()
    last_session = group['session_num'].max()
    
    # Get percentiles at first and last sessions
    first_pct = group[group['session_num'] == first_session]['percentile'].values[0] if len(group[group['session_num'] == first_session]) > 0 else None
    last_pct = group[group['session_num'] == last_session]['percentile'].values[0] if len(group[group['session_num'] == last_session]) > 0 else None
    
    lt_at_time1 = first_pct < 20 if first_pct is not None else False
    lt_at_time12 = last_pct < 20 if last_pct is not None else False
    
    # Assign talker type
    if lt_at_time1 and lt_at_time12:
        talker_type = 'PLT'
    elif lt_at_time1 and not lt_at_time12:
        talker_type = 'LB'
    elif not lt_at_time1 and not lt_at_time12:
        talker_type = 'TT'
    elif not lt_at_time1 and lt_at_time12:
        talker_type = 'Faller'
    else:
        talker_type = 'Unknown'
    
    CDI.loc[CDI['child_id'] == child_id, 'Talker_Type'] = talker_type

# Verify counts
print("Talker Type Counts:")
print(CDI[['child_id', 'Talker_Type']].drop_duplicates()['Talker_Type'].value_counts())
print(f"  PLT (Late Talker at both time 1 & 12): {CDI[CDI['Talker_Type']=='PLT']['child_id'].nunique()}")
print(f"  LB (Late Talker at time 1 only): {CDI[CDI['Talker_Type']=='LB']['child_id'].nunique()}")
print(f"  TT (Neither): {CDI[CDI['Talker_Type']=='TT']['child_id'].nunique()}")
print(f"  Faller (Late Talker at time 12 only): {CDI[CDI['Talker_Type']=='Faller']['child_id'].nunique()}")

print("Final CDI shape:", CDI.shape)

# Reorder columns: metadata, added columns, word columns
added_cols = ['vocab_size', 'Talker_Type']
new_order = metadata_cols + added_cols + word_cols
CDI = CDI[new_order]

CDI.to_csv('CDI_cleaned.csv', index=False)

# Plot vocabulary size growth over sessions with trend lines by talker type
import matplotlib.pyplot as plt
from scipy import stats as sp_stats

color_map = {'PLT': 'red', 'LB': 'orange', 'TT': 'green', 'Faller': 'purple'}

fig, ax = plt.subplots(figsize=(14, 8))

# Plot individual trajectories
for child_id, group in CDI.groupby('child_id'):
    group = group.sort_values('session_num')
    talker_type = group['Talker_Type'].iloc[0]
    color = color_map.get(talker_type, 'gray')
    
    ax.plot(group['session_num'], group['vocab_size'], 
            color=color, alpha=0.2, linewidth=1, marker='o', markersize=3)

# Add trend lines for each talker type
for talker_type in ['PLT', 'LB', 'TT', 'Faller']:
    group_data = CDI[CDI['Talker_Type'] == talker_type]
    if len(group_data) == 0:
        continue
    
    # Prepare data for trendline
    x = group_data['session_num'].values
    y = group_data['vocab_size'].values
    
    # Calculate linear regression
    slope, intercept, r_value, p_value, std_err = sp_stats.linregress(x, y)
    line = slope * x + intercept
    
    # Plot trendline
    color = color_map.get(talker_type, 'gray')
    ax.plot(x, line, color=color, linewidth=3, alpha=0.9, 
            label=f'{talker_type} (slope={slope:.2f})')

ax.set_xlabel('Session Number', fontsize=12, fontweight='bold')
ax.set_ylabel('Vocabulary Size (Words Spoken)', fontsize=12, fontweight='bold')
ax.set_title('Vocabulary Growth Over Sessions by Talker Type (with Trend Lines)', fontsize=14, fontweight='bold')
ax.grid(True, alpha=0.3)
ax.legend(loc='lower right', fontsize=10)

plt.tight_layout()
plt.savefig('CDI_vocab_growth_trajectories.png', dpi=300)
plt.show()

print("Plot saved as CDI_vocab_growth_trajectories.png")