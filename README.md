# hs-music

### Installation
Clone this repo, cd into it, use stack with LTS-14.14 resolver and run `stack build` and maybe `stack install` to copy the bin in your path dir.

### How to use
You need to create a file named "playlists.txt" in your music directory with 
each line containing a JSON object with the `name` of the folder that should 
be created on the disk and `url` to the youtube playlist as properties.

You could run it manually but I assume setting up some crontab could save you
some time :)

As of now, this is just to learn some Haskell, however there are some plans to
add some stuff

### TODOs:
- [x] Dockerize
- [ ] switch to YAML
- [ ] allow configurations through env vars
- [ ] allow some CLI arguments
- [ ] add notifications so you know when stuff happens (probabbly email or some push notifications)
- [ ] abstract some stuff for easier maintanace and
- [ ] tests :)
