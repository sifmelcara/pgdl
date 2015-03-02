# pgdl

a terminal user interface program that downloads a video from html and call vlc to play it immediately.
(Linux: vlc -f "video.mp4", MACOSX: open "video.mp4" -a vlc)

Client: Only for Linux and MACOSX.
Server: Only for nginx's file download page.

The program will sort the video files and folders by date while files in nginx's download page just sort by name.

## Installing

cabal update
cabal install pgdl

