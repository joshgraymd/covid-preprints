import json
from concurrent.futures import ThreadPoolExecutor, as_completed
from bs4 import BeautifulSoup
import requests
import urllib.parse
from time import sleep
from datetime import datetime
with open('data.json', 'r') as f:
    data = json.load(f)

MAX_RETRIES = 5
headers = requests.utils.default_headers()
headers['User-Agent'] = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_4) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/81.0.4044.113 Safari/537.36'

def fetchDetailsForDoi(doi, i, lengthOfList):
    print('fetching doi: %s (%d of %d)' % (doi, i, lengthOfList))
    retries = 0
    while retries < MAX_RETRIES:
        try:
            r = requests.get('https://www.doi.org/%s' % doi, timeout=100)
            if r.status_code != 200:
                break
            resolvedURL = r.url
            metricsRes = requests.get('%s.article-metrics' % resolvedURL, timeout=100)
            metricSoup = BeautifulSoup(metricsRes.content, 'html.parser')
            numAuthors = len(metricSoup.select('.has-author-tooltip > div.highwire-cite-authors > span > span'))
            numMetrics = len(metricSoup.select('#highwire-highwire-stats-filter-form > div > table > thead > tr > th')) - 1
            engagementMetrics = {}
            for i in range(2, numMetrics + 2):
                sectionTitle = metricSoup.select('#highwire-highwire-stats-filter-form > div > table > thead > tr > th:nth-child(%d)' % i)[0].text
                values = metricSoup.select('#highwire-highwire-stats-filter-form > div > table > tbody > tr > td:nth-child(%d)' % i)
                total = 0
                for val in values:
                    total += int(val.text.replace(',',''))
                engagementMetrics[sectionTitle] = total
            return doi, numAuthors, engagementMetrics
        except Exception as e:
            print(e)
            print('error here, retrying')
            retries += 1


def fetchAllDOIDetails(doiList):
    print('fetching %d DOIs' % len(doiList))
    doiListLength = len(doiList)
    running_page_fetches = []
    with ThreadPoolExecutor(max_workers=50) as executor:
        for i, doi in enumerate(doiList):
            running_page_fetches.append(executor.submit(fetchDetailsForDoi, doi, i, doiListLength))
    for task in as_completed(running_page_fetches):
        taskResults = task.result()
        if taskResults is not None:
            selectedEntry = next((x for x in data if x['doi'] == taskResults[0]), None)
            if selectedEntry is not None:
                selectedEntry['pp_num_authors'] = taskResults[1]
                selectedEntry['engagement'] = taskResults[2]
    with open('updatedDataWithEngagement.json', 'w', encoding='utf-8') as f:
        f.write(json.dumps(data,ensure_ascii=False, indent=4, default=str))


def safeGetFromJSON(jsonToSearch, pathToSearch):
    head = jsonToSearch
    for el in pathToSearch:
        if el in head:
            head = head[el]
        else:
            return None
    return head

def fetchDateForDOI(entry, i, lengthOfList):
    doi = entry['published']
    print('fetching doi: %s (%d of %d)' % (doi, i, lengthOfList))
    retries = 0
    while retries < MAX_RETRIES:
        try: 
            r = requests.get('https://api.crossref.org/works/%s' % doi, headers=headers, timeout=600)
            req_json = json.loads(r.text)
            pub_created_date_raw = safeGetFromJSON(req_json, ['message', 'created', 'date-parts'])
            if pub_created_date_raw is not None:
                if len(req_json['message']['created']['date-parts'][0]) == 3:
                    fmt_string =  '%Y %m %d'
                elif len(req_json['message']['created']['date-parts'][0]) == 2:
                    fmt_string = '%Y %m'
                pub_date_created = datetime.strftime(datetime.strptime(' '.join(str(x) for x in req_json['message']['created']['date-parts'][0]), fmt_string), '%Y-%m-%d')
                entry['crossref_pub_date'] = pub_date_created
                #print('pub_date for doi: %s is %s' % (doi, pub_date_created))
                return doi, pub_date_created
            return doi, None
        except Exception as e:
            print(e)
            print('error, retrying for doi: %s' % doi)
            print(r.text)
            sleep(0.5)
            retries += 1
    return doi, None

def getAllDOIDatesFromCrossref(dataList):
    doiListLength = len(doiList)
    running_page_fetches = []
    with ThreadPoolExecutor(max_workers=10) as executor:
        for i, entry in enumerate(data):
            running_page_fetches.append(executor.submit(fetchDateForDOI, entry, i, doiListLength))
    for _ in as_completed(running_page_fetches):
        pass
            # for selectedEntry in selectedEntries:
            #     if selectedEntry is not None and len(taskResults) == 2 and taskResults[1] is not None:
            #         selectedEntry['crossref_pub_date'] = taskResults[1]
    with open('updatedDataWithPubDates.json', 'w', encoding='utf-8') as f:
        f.write(json.dumps(data,ensure_ascii=False, indent=4, default=str))

doiList = [x['doi'] for x in data]
uniqueDOIList = set(doiList)

pubDoiList = [x['published'] for x in data]
uniquePubDOIList = set(pubDoiList[:50])
#fetchAllDOIDetails(uniqueDOIList)
getAllDOIDatesFromCrossref(data)