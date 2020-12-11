# Dataset codebook

The datasets created for this article are found in the `Data/` directory. It contains 5 files:
- `Survey_replies.csv`: contains raw replies to the survey (in French). List of survey fields can be found in the supplementary information of the paper.
- `Survey_data.Rdata`: dataset created from the raw survey replies and described below.
- `all_channels.csv`: channel level data extracted from the YouTube API and described below.
- `all_video.csv`: video level data extracted  from the YouTube API and described below.
- `channels_rated.csv`: list and ratings of channel reviewed by the authors, as described below.

## Survey data

Final survey data are found in the file `Survey_data.RData`. It is a cleaned dataset generated from the raw survey replies (`Survey_replies.csv`) by the program `0_PrepSurvey.R`.
The file contains a single data.frame `datatab` with 180 lines (respondent) and 37 variables.

Variable | Description | Type | Details
--- | --- | --- | ---
`time_stamp` | Time and day the answer was collected | Date-Time | 
`gender` | Gender of the respondent | Character | Only applies to individual creators
`npeople` | Number of people working on the channel | Categorical [1 ; >10] | ~ 1 means that other people may occasionally join the creator
`institution` | Whether the channel is individual of institutional | Character | 
`time_spent` | Proportion of working time spent on the channel | Categorical [1 ; 100] | ~100 means a full time spread over several persons
`EducationVideo` | Has the content creator a degree related to video | Categorical |
`degree` | Level of education | Categorical | 
`expertise` | Prior expertise on the main channel subject | Categorical |
`proCat` | Professional category | Categorical |
`Academia` | Whether the creator has an academia-related job | Binary |
`funding` | Sources of funding | Character | Possibly more than one per respondent
`balance` | Balance of the activity | Categorical | This field is hidden for confidentiality purposes
`mainSource` | Is this activity the creator's main source of income | Categorical |
`prowish` | Wish of the creator regarding channel balance | Categorical |
`institutionGoal` | Whether the channel is used to promote science or the institution | Categorical | Only applies to institutions
`Income` | Reported annual income | Numeric | This field is hidden for confidentiality purposes
`priority` | Priority given to channel in the communication strategy | Categorical (1: lower; 5: higher) | Only applies to institutions
`satisfaction` | Reported satisfaction in the institution | Categorical (1: lower; 5: higher) | Only applies to institutions
`reluctance` | Initial reluctance toward channel creation | Categorical (1: lower; 5: higher) | Only applies to institutions
`feedback` | Quality of feedback inside the institution | Categorical (1: lower; 5: higher) | Only applies to institutions
`field` | Fields spanned by the videos | Character | Possibly more than one per respondent
`nchannel` | Number of channels owned by the respondent | Categorical |
`pubFrequency` | Reported publication frequency | Numeric (video / year) | 
`faceCam` | Use of talking-head format | Categorical | 
`animation` | Use of animated format | Categorical | 
`subs` | Reported number of subscribers | Numeric | 
`target` | Targeted audience | Categorical | Possibly more than one per respondent
`propFemale` | Reported proportion of female viewers | Numeric (%) | As reported by YouTube Analytics
`prop13-17` | Reported proportion of viewers between 13 and 17 | Numeric (%) | As reported by YouTube Analytics
`prop18-24` | Reported proportion of viewers between 18 and 24 | Numeric (%) | As reported by YouTube Analytics
`prop25-34` | Reported proportion of viewers between 25 and 34 | Numeric (%) | As reported by YouTube Analytics
`prop35-44` | Reported proportion of viewers between 35 and 44 | Numeric (%) | As reported by YouTube Analytics
`prop45-54` | Reported proportion of viewers between 45 and 54 | Numeric (%) | As reported by YouTube Analytics
`prop55-64` | Reported proportion of viewers between 55 and 64 | Numeric (%) | As reported by YouTube Analytics
`prop65+` | Reported proportion of viewers 65 and older | Numeric (%) | As reported by YouTube Analytics
`Age` | Reported age | Numeric | Only applies to individuals
`Channel_age` | Age of the channel | Numeric (years) | 

## Channel level data

Channel level data are found in the file `all_channels.csv` and contains 628 channels for which we gathered 13 variables.

Variable | Description | Type | Details
--- | --- | --- | ---
`name` | Creator name | Character | 
`category` | Is the channel from an institution | Binary | Coded as 'Insitution' and 'Non-institution'
`subcategory` | Type of institution | Character | Only applies to institutions
`objective` | Whether the channel promotes science or the institution | Categorical | Determined by the authors by looking either at the descriptions or some videos; Only applies to institutions
`channel_name` | Channel name | Character | 
`channel_description` | Description of the channel as displayed on YouTube | Character | 
`channel_creation` | Date of creation | Date-Time | 
`channel_subscribers` | Number of subscribers | Numeric | Extracted July 2020 (day vary from channel to channel
`channel_views` | Total number of views | Numeric | Extracted July 2020 (day vary from channel to channel
`channel_videos` | Number of videos uploaded | Numeric | Extracted July 2020 (day vary from channel to channel
`channel_id` | YouTube unique channel ID | Character | 
`channel_url` | Channel URL | Character | 

## Video level data 

Video level data are found in the file `all_video.csv` and contains 70,795 videos for which we gathered 12 variables.

Variable | Description | Type | Details
--- | --- | --- | ---
`video_id` | Youtube unique video ID | Character |
`video_title` | Title of the video | Character |
`video_description` | First lines of the video description | Character |
`video_url` | URL to directly access the video | Character | 
`channel_id` | YouTube unique channel ID | Character | 
`channel_title` | Channel hosting the video | Character |
`video_date` | Date and time of upload | Date-Time | 
`view_count` | Number of views | Numeric | Extracted July 2020
`like_count` | Number of likes | Numeric | Extracted July 2020
`dislike_count` | Number of dislikes | Numeric | Extracted July 2020
`comment_count` | Number of comments | Numeric | Extracted July 2020
`statistics_accessed_on` | Day of exctraction | Date |

## Channels reviewing and rating

The file `channels_rated.csv` contains the list and data of channel reviewed by three of the authors before beginning the study. Authors told whether they think the channel is considered as a scientific one. It contains 13 variables for 2,539 channels.

Variable | Description | Type | Details
--- | --- | --- | ---
`channel_name` | Channel name | Character | 
`channel_description` | Description of the channel as displayed on YouTube | Character | 
`Keep ? 1= Yes, 0=No (Stephane)` | Votes from Stéphane Debove | Binary (0 = reject, 1 = keep) | 
`Keep ? 1= Yes, 0=No (Pierre)` | Votes from Pierre Masselot | Binary (0 = reject, 1 = keep) | 
`Keep ? 1= Yes, 0=No (Tania)` | Votes from Tania Louis | Binary (0 = reject, 1 = keep) | 
`channel_creation` | Date of creation | Date-Time | 
`channel_subscribers` | Number of subscribers | Numeric | Extracted July 2020 (day vary from channel to channel
`channel_views` | Total number of views | Numeric | Extracted July 2020 (day vary from channel to channel
`channel_videos` | Number of videos uploaded | Numeric | Extracted July 2020 (day vary from channel to channel
`channel_id` | YouTube unique channel ID | Character | 
`channel_url` | Channel URL | Character | 
`keep_score` | Final score attributed to the channel | Numeric [0 - 3] | Sum of the three votes
`Keep_this_channel` | Was the channel selected | Binary (0 = No; Yes = 1) | A channel was selected when its score is 2 or 3
