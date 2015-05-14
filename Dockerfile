FROM haskell:7.10.1
MAINTAINER Matthew Brown <matt@ederoyd.co.uk>
ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update && apt-get install -y make

RUN cabal update && \
 	cabal install cabal-install cabal

ADD . /OSMImport
WORKDIR /OSMImport
RUN ./build-platform.sh

ENV PATH $PATH:/OSMImport/platform/osmimport/bin

ENTRYPOINT ["OSMImport"]
