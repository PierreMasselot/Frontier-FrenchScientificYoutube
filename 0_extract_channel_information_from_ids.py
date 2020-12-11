# PRIVACY :
# YouTube ToS : https://www.youtube.com/t/terms
# Google Privacy policy : https://policies.google.com/privacy?hl=en-GB

import urllib.request 
import json
from datetime import date

input_filename='all_youtube_ids.txt' # list of youtube channel ids, one id per line
output_filename='output.csv' # will be tab-separated

api_key='' #to get an API key, create a project, enable Youtube data API v3 for it, and get API key from here : https://console.developers.google.com/apis/credentials?project=_. As of May 2020, 10,000 calls per day are allowed by Google (per api key)


channels_with_error_400=[''] # will not try to fetch information for these channels (channels that non longer exist for instance, they send an error and block the script)


with open(input_filename, 'r') as f:
    channel_ids = [line.strip() for line in f]


##### FIND DATA FOR EACH CHANNEL:

channel_ids = sorted(channel_ids) #removes possible duplicates
channel_count = len(channel_ids)

start_at = 1 # row number to start from (useful to avoid starting again from the start if the script crashes in the middle). Rename the first output file before, as it will be erased. 1=first row
with open(output_filename, "w", encoding='UTF-8') as f:
	today = date.today()
	f.write("File generated on "+str(today)+". Tab-separated csv. \n")
	f.write("channel_name\tchannel_description\tchannel_creation\tchannel_subscribers\tchannel_views\tchannel_videos\tchannel_id\tchannel_url\n")

	for index, channel_id in enumerate(channel_ids):
		if index < (start_at - 1) or channel_id.rstrip() in channels_with_error_400:
			continue
		
		print('Retrieving information for channel ' + channel_id + ' ('+str(index+1)+' / '+str(channel_count)+')')
		
		request='https://www.googleapis.com/youtube/v3/channels?part=snippet,statistics&id='+channel_id+'&key='+api_key 

		contents = urllib.request.urlopen(request).read()
		json_data = json.loads(contents)
		if 'items' in json_data and len(json_data['items']) > 0:
			json_items = json_data['items'][0]
			f.write(json_items['snippet']['title'].replace('\t',' ')+"\t"+json_items['snippet']['description'].replace('\n','. ').replace('\t',' ').replace('\r',' ')+"\t"+json_items['snippet']['publishedAt']+"\t"+json_items['statistics']['subscriberCount']+"\t"+json_items['statistics']['viewCount']+"\t"+json_items['statistics']['videoCount']+"\t"+channel_id+"\t"+"https://www.youtube.com/channel/"+channel_id+"\n")
		else:
			print("No data found for " + channel_id + ". This channel might have been deleted.")
		

