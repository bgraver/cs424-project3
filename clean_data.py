import json
import pandas as pd

d = []
ret = []
# read file
with open('output.json', 'r') as f:
    for line in f:
        d.append(json.loads(line))

for value in d:
    t = {}
    t["id"] = value["id"]
    t["title"] = value["title"]
    t["kind"] = value["kind"]
    output = ''
    if value.get("genres") != None:
        for s in value["genres"]:
            output += "{0},".format(str(s))
        t["genres"] = output
    if value.get("release_dates") != None:
        t["release_dates_country"] = value["release_dates"][0]["country"]
        t["release_dates_date"] = value["release_dates"][0]["date"]
    if value.get("year") != None:
        for s in value["year"]:
            output += "{0},".format(str(s))
        t["year"] = output
    if value.get("rating") != None:
        t["rating_rank"] = value["rating"]["rank"]
        t["rating_votes"] = value["rating"]["votes"]
    if value.get("keywords") != None:
        for s in value["keywords"]:
            output += "{0},".format(str(s))
        t["keywords"] = output
    if value.get("running_times") != None:
        if value["running_times"][0].get("secs") != None:
            t["running_times_secs"] = value["running_times"][0]["secs"]
        if value["running_times"][0].get("country") != None:
            t["running_times_country"] = value["running_times"][0]["country"]
    if value.get("certificates") != None:
        t["certificates_country"] = value["certificates"][0]["country"]
        t["certificates_rating"] = value["certificates"][0]["rating"]
    ret.append(t)

df = pd.DataFrame.from_dict(ret)

df.to_csv("combined_data.csv", index=False)