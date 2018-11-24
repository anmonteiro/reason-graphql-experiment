FROM alpine:3.8 as base

ENV TERM=dumb \
    LD_LIBRARY_PATH=/usr/local/lib:/usr/lib:/lib

WORKDIR /graphql

RUN apk add --no-cache nodejs-current npm \
    libev yarn libev-dev python jq \
    ca-certificates wget \
		bash curl perl-utils \
		git patch gcc g++ musl-dev make m4 util-linux

RUN wget -q -O /etc/apk/keys/sgerrand.rsa.pub https://alpine-pkgs.sgerrand.com/sgerrand.rsa.pub
RUN wget https://github.com/sgerrand/alpine-pkg-glibc/releases/download/2.28-r0/glibc-2.28-r0.apk
RUN apk add --no-cache glibc-2.28-r0.apk

RUN npm install -g esy@next --unsafe-perm

COPY server.json /graphql/esy.json

RUN esy install
RUN esy

COPY . /graphql

RUN esy b dune build @graphql --profile=docker

RUN mv $(esy command-env --json | jq --raw-output .cur__target_dir)/default/src/server/main.exe /graphql/main.exe

FROM scratch

WORKDIR /graphql

COPY --from=base /graphql/main.exe main.exe
