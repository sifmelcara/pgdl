# pgdl

a terminal user interface program that downloads a video from html and call vlc to play it immediately.
(Linux: vlc -f "video.mp4", MACOSX: open "video.mp4" -a vlc)

Client: Only for Linux and MACOSX.
Server: Only for nginx's file download page.

The program will sort the video files and folders by date while files in nginx's download page just sort by name.

## Installing

cabal update <br>
cabal install pgdl

## Command line argument

all arguments passed to the program become keywords to filter videos.

## Shortcut keys

q - quit
<br>
s - list files that similar with the highlighted video
<br>
Key Right - show the detailed information of the highlighted video
<br>
Key Left - go back to the previous page
<br>
Enter - play the video
<br>
/ - open a input box to enter a keyword to search
<br>
n - sort the current list by name

