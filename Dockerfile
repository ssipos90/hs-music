FROM fpco/stack-build:lts-14.27 as build
WORKDIR /opt/build
COPY ./stack.yaml ./package.yaml ./
RUN stack build --system-ghc --dependencies-only

COPY . ./

RUN set -eu; \
    mkdir /opt/build/bin; \
    stack build \
      --system-ghc \
      --ghc-options="-fPIC" \
      --ghc-options="-O2" \
      --copy-bins \
      --local-bin-path=/usr/local/bin

FROM ubuntu:18.04

RUN apt-get update && apt-get install libgmp10
WORKDIR /app

COPY --from=build /usr/local/bin ./

CMD ["/app/hs-music-exe"]
