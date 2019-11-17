FROM haskell:8.6.5 as dependencies
RUN mkdir /opt/build
COPY ./stack.yaml ./hs-music.cabal /opt/build/
WORKDIR /opt/build
RUN stack build --system-ghc --dependencies-only
COPY ./ /opt/build
RUN ls -laF /opt/build
RUN stack install --system-ghc --ghc-options -static

FROM debian:8
RUN apt-get update && \
    apt-get install -y libgmp-dev
COPY --from=dependencies /root/.local/bin/hs-music /usr/local/bin/hs-music
RUN useradd -M music && \
    mkdir /music && \
    chown -R music:music /music
USER music
WORKDIR /music
VOLUME /music
ENTRYPOINT "/usr/local/bin/hs-music"