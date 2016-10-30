# pgdl
[![Build Status](https://travis-ci.org/sifmelcara/pgdl.svg?branch=master)](https://travis-ci.org/sifmelcara/pgdl)
## What can this program do?

pgdl is a program for viewing and accessing directory listing webpage in terminal.

Browsing files on directory listings like
[this](https://www.kernel.org/pub/linux/)
is often annoying and hard to find the files we want.

pgdl provids a simple interface for browsing and downloading the files in web-engine-generated directory listings.

## Installing

by cabal:
```shell
cabal update  
cabal install pgdl
```

via nix:
```shell
nix-env -iA nixos.haskellPackages.pgdl
```

## example usage

```shell
pgdl https://www.kernel.org/pub/linux/
```
or simply type pgdl if you have set servpath attribute in the config file.

## Shortcut keys

'/' for file searching

press Enter to download the selected file

press 'q' to quit the program

press 'd' to delete currently selected file (which have been downloaded)

press Meta+Enter to resume the download progress of the currently selected file (like `curl -C` does)


## Config file

If you want to access webpage that uses basic authentication, you should at least set
*username* attribute in config file.
(if password is not set, you need to enter password manually when program launch)

(~/.pgdl)
```bash
# example: 
username = "jack"      # should be set if the webpage have basic authentication
password = "mypassw"   # optional
servpath = "example.org/videodir/" # default server location, optional
localdir = "/home/jack/Downloads/" # where to store downloaded files, optional
```
