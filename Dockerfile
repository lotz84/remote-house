FROM haskell:8.8.3

ARG NODE_VERSION=v12.16.3
ARG NODE_DISTRO=linux-x64
ARG PURS_VERSION=0.13.6
ARG SPAGO_VERSION=0.15.2

ENV PATH /usr/local/bin/node-${NODE_VERSION}-${NODE_DISTRO}/bin:$PATH
RUN curl -fSL https://nodejs.org/dist/${NODE_VERSION}/node-${NODE_VERSION}-${NODE_DISTRO}.tar.xz -o node.tar.xz && \
    tar -xJvf node.tar.xz -C /usr/local/bin && \
    npm install -g --unsafe-perm purescript@${PURS_VERSION} && \
    npm install -g --unsafe-perm spago@${SPAGO_VERSION}

WORKDIR /work

COPY stack.yaml remote-house.cabal /work/
RUN stack build --only-dependencies

COPY . /work/
RUN stack build && stack install

RUN spago bundle-app --main Main --to frontend/static/main-ps.js

CMD remote-house-exe $PORT
