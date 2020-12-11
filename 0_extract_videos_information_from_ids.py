# PRIVACY :
# YouTube ToS : https://www.youtube.com/t/terms
# Google Privacy policy : https://policies.google.com/privacy?hl=en-GB

import urllib.request 
import datetime
import json
import sys
import os
import pickle
from datetime import datetime
today_date = datetime.now()
today_date = today_date.strftime('%Y-%m-%d')
from pyexcel_ods import save_data


input_filename='all_youtube_ids.txt' # list of youtube channel ids, one id per line
	
with open(input_filename, 'r') as f:
    channel_ids = [line.strip() for line in f]


api_keys=['',''] # list of API keys. Get an API key by creating a project and enabling Youtube data API v3 for it from here : https://console.developers.google.com/apis/credentials?project=_ . Each API has a limit of 10,000 resource usage per day (see below) so you’ll have to use multiple API if you query many channels.

video_list=[]

search_quota_cost = 102 #this is the cost of a "search" API call, see https://developers.google.com/youtube/v3/determine_quota_cost. Officially it’s supposed to be 100 but in practice it seems to be 102. See https://console.cloud.google.com/iam-admin/quotas?project= to get the current daily usage

count=1

quota_usage=0
api_index=0
api_key=api_keys[api_index]


first=1 # if you can’t get the data you want for all your channels in one run, even using multiple API keys (see above), you’ll have to limit the number of channels you query per script run. The results will be written in a temporary file video_list_*.txt which name you should change manually below, at each run. Then the next day you run your script again after changing the first, last, and temp_file variables. Be careful: if the script breaks, the temporary file will not be written (this behavior should change in a future version of the script). Files are written with the "pickle" python function so they won’t be human-readable, and they’ll have to be loaded again before running the second part of the script (see below)
last=1000
temp_file="video_list_temp_1.txt"


def get_videos(next_page):
	global quota_usage
	global api_index
	global api_key

	request='https://www.googleapis.com/youtube/v3/search?part=snippet&channelId='+channel_id+'&type=video&key='+api_key+'&maxResults=50&order=date'+next_page # request to get all videos from this channel

	contents = urllib.request.urlopen(request).read()
	json_data = json.loads(contents)
#		print(json_data)
	quota_usage+=search_quota_cost
	for index, video in enumerate(json_data['items']): #for each retrieved video on this channel, we keep only the desired information
		
		video_list.append( {
			'video_id' : video['id']['videoId'],
			'video_title' : video['snippet']['title'],
			'video_description' : video['snippet']['description'],
			'video_url' : 'https://www.youtube.com/watch?v='+video['id']['videoId'],
			'channel_id' : channel_id,
			'channel_title' : video['snippet']['channelTitle'],
			'video_date' : video['snippet']['publishedAt']
		})
	if quota_usage > 9800:
		api_index += 1
		api_key=api_keys[api_index]
		print('api key change')
		print('usage reached for this API key (assuming it was at 0 before): ' + str(quota_usage))
		print('new api key:'+api_key)
		quota_usage=0
	print(len(video_list))

	return json_data

# go through all the channel ids:

for channel_id in channel_ids:
	if count >= first and count <= last:
		
		print('channel id:' + channel_id)
		print('channel is number ' + str(count) + ' in the list')
		my_json = get_videos('')
			
		while "nextPageToken" in my_json: # the channel has more than 50 videos (youtube imposes a limit of 50 videos to get at the same time)
			next_page_token = '&pageToken='+my_json['nextPageToken']
			my_json = get_videos(next_page_token)
			
		count+=1
	else:
		count+=1

#print(video_list)

with open(temp_file, "wb") as fp:   #Pickling
	pickle.dump(video_list, fp)

#print('done') #uncomment these if you’re running the script over multiple days
#sys.exit() #same


def get_stat(stat_clean_name, yt_stat, my_video, index):
	if yt_stat in video['statistics']: 
		video_list[index][stat_clean_name] = int(my_video['statistics'][yt_stat])
	else:
		video_list[index][stat_clean_name] = "NA"

# load all temporary files and put them in a list:

video_list=[]
directory = os.fsencode(".")    
for file in os.listdir(directory):
	filename = os.fsdecode(file)
	if filename.startswith("video_list_"): 
		with open(filename, "rb") as fp:	# Unpickling
			video_list += pickle.load(fp)
			#print(filename)
#print(video_list)
#print(len(video_list))

# we will now make an API request to get likes, comments, etc for each video:

all_ids = [key['video_id'] for key in video_list] # keep only ids
n_ids = len(all_ids)
all_ids_50_chunks = [all_ids[i:i + 50] for i in range(0, len(all_ids), 50)] # create chunks of 50 ids as it's the limit for the next Youtube request

#video_list = []
yt_statistics = {'view_count': 'viewCount', 'like_count': 'likeCount', 'dislike_count': 'dislikeCount', 'comment_count': 'commentCount', 'favorite_count': 'favoriteCount'}	
index=0
for chunk in all_ids_50_chunks:
	string_chunk = ",".join(chunk) # transform list to string
	print("getting statistics for video " + str(index) + "/" + str(n_ids))
	
	contents = urllib.request.urlopen('https://www.googleapis.com/youtube/v3/videos?id='+string_chunk+'&key='+api_key+'&part=statistics').read() #this API call is costing 3 resource usage in the quota
	json_data = json.loads(contents)
	
	quota_usage+=3
	if quota_usage > 9800:
		api_index += 1
		api_key=api_keys[api_index]
		print('api key change')
		print('usage reached for this API key (assuming it was at 0 before): ' + str(quota_usage))
		print('new api key:'+api_key)
		quota_usage=0
	#print(json_data['items'][0]['statistics'])
	
	for video in json_data['items']: #for each retrieved video we keep only the stats
		for key, value in yt_statistics.items():
			get_stat(key,value,video,index)
		video_list[index]["statistics_accessed_on"] = today_date
		#print(video_list[index])
		index += 1
	
with open("video_list_final.txt", "wb") as fp:   #Pickling
	pickle.dump(video_list, fp)

#sys.exit()

# the rest of the code will transfrom the output file as a human-readable file:

with open("video_list_final.txt", "rb") as fp:   # Unpickling
	video_list = pickle.load(fp)


import codecs
with codecs.open("video_list_human_readable.csv", "w", "utf-8-sig") as temp:
	temp.write("video_id\tvideo_title\tvideo_description\tvideo_url\tchannel_id\tchannel_title\tvideo_date\tview_count\tlike_count\tdislike_count\tcomment_count\tfavorite_count\tstatistics_accessed_on\n")
	for my_dict in video_list:
		#print(my_dict)
		for key, value in my_dict.items():
			if key == "video_description" and value == "":
				value = "NA"
			temp.write("%s\t" % value)
		temp.write("\n")
		
#print(video_list[0])

# CAREFUL ! Check the output as if some videos were deleted between the date you listed them and the date you searched statistics for, your file will be messed up, and statistics will not correspond to the good video!

#save_data("your_file.ods", video_list)







