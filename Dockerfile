FROM haskell:8.8.3

WORKDIR /work

COPY stack.yaml remote-house.cabal /work/
RUN stack build --only-dependencies

COPY . /work/
RUN stack build && stack install

CMD remote-house-exe $PORT
