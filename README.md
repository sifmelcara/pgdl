# pgdl

a terminal user interface program that downloads a video (or a file) from webpage and xdg-open it immediately.
(Linux: xdg-open "video.mp4", MACOSX: open "video.mp4")

Client: Only for Linux and MACOSX.
Server: Only for nginx's file download page.

The program will sort the video files and folders by date while files in nginx's download page just sort by name.

## Installing

cabal update  
cabal install pgdl

## Shortcut keys

q - quit  
s - list files that similar with the highlighted video  
Key Right - show the detailed information of the highlighted video  
Key Left - go back to the previous page  
Enter - play the video  
/ - open a input box to enter a keyword to search  
n - sort the current list by name  
v - show the videos already in the disk  
u - show the videos not in the disk

