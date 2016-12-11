# pls-mgr
A small command-line program for managing .pls playlists.

## Installation
### Cabal
(I haven't actually tested it with cabal, but it should work...)
```
$ git clone https://github.com/bradrn/pls-mgr
$ cabal configure
$ cabal build
$ cabal install
```
### Stack
```
$ git clone https://github.com/bradrn/pls-mgr
$ stack install
```

## Usage
pls-mgr is invoked as `pls-mgr COMMAND`, where `COMMAND` is one of the following:
* `add playlist_file title file length`, which adds a song to the end of the playlist;
* `add-index playlist_file title file length`, which adds a song at a specified index (selected interactively) in the playlist;
* `init playlist_file`, which creates a blank playlist file, replacing its contents if it already exists;
* `move playlist_file`, which moves a song from one index to another (both specified interactively) inside the playlist;
* `remove playlist_file`, which deletes a song at the specified index (again, specified interactively);
* `replace playlist_file title file length`, which replaces a song at the specified index (specified interactively) in the playlist; and
* `show playlist_file`, which prints the playlist in a human-readable format.
