# hs-music

### Installation
Clone this repo, cd into it, use stack with LTS-14.14 resolver and run `stack build` and maybe `stack install` to copy the bin in your path dir.

### How to use
You need to create a file named "playlists.yaml" in your music directory with 
a list of playlists in the following format:
```yaml
- name: techno
  url: https://www.youtube.com/playlist?list=PLMmqTuUsDkRJR0fglM0pBwNN5EUVGs4Y-
- name: pop
  url: https://www.youtube.com/playlist?list=PLMC9KNkIncKtPzgY-5rmhvj7fax8fdxoj
- name: rominimal
  url: https://www.youtube.com/playlist?list=PL1pZmgI6eOAI9DI8B6jDYPQ6rQHPHzICIu
```

You could run it manually but I assume setting up some crontab could save you
some time :)

As of now, this is just to learn some Haskell, however there are some plans to
add some stuff

### TODOs:
- [x] Dockerize
- [x] switch to YAML
- [ ] allow configurations through env vars
- [ ] allow some CLI arguments
- [ ] add notifications so you know when stuff happens (probabbly email or some push notifications)
- [ ] abstract some stuff for easier maintanace and
- [ ] tests :)
