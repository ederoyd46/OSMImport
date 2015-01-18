FROM haskell:7.8
MAINTAINER Matthew Brown <matt@ederoyd.co.uk>
ENV DEBIAN_FRONTEND noninteractive

RUN cabal update && \
 	cabal install cabal-install cabal

ADD . /OSMImport
WORKDIR /OSMImport
RUN cabal install

ENV PATH $PATH:/root/.cabal/bin

ENTRYPOINT ["OSMImport"]