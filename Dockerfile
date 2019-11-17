FROM haskell:8.6.5 as dependencies
COPY . /opt/build
WORKDIR /opt/build
RUN stack build --system-ghc --dependencies-only

FROM haskell:8.6.5 as build
COPY --from=dependencies /root/.stack /root/.stack
COPY /opt/build /opt/build
WORKDIR /opt/build
RUN stack build --system-ghc


# COPY --from=build /opt/build/.stack-work /usr/local/bin/hs-music
# ENTRYPOINT "/usr/local/bin/hs-music"