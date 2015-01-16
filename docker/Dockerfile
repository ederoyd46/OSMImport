FROM haskell:7.8
MAINTAINER Matthew Brown <matt@ederoyd.co.uk>
ENV DEBIAN_FRONTEND noninteractive

##################################################################################
## Install Required Software
##################################################################################
RUN apt-get update && \
	apt-get upgrade -y && \
	apt-get install -y git-core make

RUN git clone https://github.com/ederoyd46/OSMImport

RUN cabal update && \
 	cabal install cabal-install cabal

WORKDIR /OSMImport
RUN cabal install --force-reinstalls

ENV PATH $PATH:/root/.cabal/bin
