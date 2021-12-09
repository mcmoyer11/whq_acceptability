#python script to 
import csv
import re

results = ./b_long.csv
pilot_results = ./pilot_results.csv

wtih open(resutls, 'wb+') as results

	results = list(csv.reader(results))

    pilot_results = csv.writer(pilot_results)
    pilot_results.writerow(['subject','native_lang','list','context','mat_verb','wh','finite','emb_verb','response'])

    subj_info = []
    by_trial_info = []

    # def convert_to_binary(response):
    #     if response == 'yes':
    #         return 1
    #     elif response == 'no':
    #         return 0
    #     else:
    #         return 'NA'


    for i,r in enumerate(results):
    # print r
    subject = r[1]
    native_lang = r[2]
    response = 

    if 