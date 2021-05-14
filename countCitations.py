import json
from datetime import datetime
from datetime import timedelta

preprintPrefix = '10.1101/'

with open('data.json', 'r') as f:
    d = json.load(f)

CUTOFF_IN_DAYS = 90


for entry in d:
    if 'pp_citations' in entry:
        entry['pp_num_citations'] = 0
        for k, v in entry['pp_citations'].items():
            if 'crossref_pub_date' in entry:
                pub_date = datetime.strptime(entry['crossref_pub_date'], '%Y-%m-%d').date()
            if v is None:
                continue
            if v is not None and preprintPrefix not in k and datetime.strptime(v, '%Y-%m-%d').date() <= pub_date + timedelta(days=CUTOFF_IN_DAYS):
                entry['pp_num_citations'] += 1
    if 'pub_citations' in entry:
        entry['pub_num_citations'] = 0
        for k, v in entry['pub_citations'].items():
            if 'crossref_pub_date' in entry:
                pub_date = datetime.strptime(entry['crossref_pub_date'], '%Y-%m-%d').date()
            if v is None:
                continue
            if 'pp_citations' in entry and k in 'pp_citations':
                continue
            if datetime.strptime(v, '%Y-%m-%d').date() < pub_date:
                if not 'pp_num_citations' in entry:
                    entry['pp_num_citations'] = 0
                entry['pp_num_citations'] += 1
                continue
            if v is not None and preprintPrefix not in k and datetime.strptime(v, '%Y-%m-%d').date() <= pub_date + timedelta(days=CUTOFF_IN_DAYS):
                entry['pub_num_citations'] += 1


with open('dataWithCiteCounts.json', 'w', encoding='utf-8') as f:
    f.write(json.dumps(d,ensure_ascii=False, indent=4, default=str))
    
