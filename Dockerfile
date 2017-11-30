FROM fpco/stack-build
LABEL AUTHOR Matthew Brown <matt@ederoyd.co.uk>
ENV DEBIAN_FRONTEND noninteractive

ADD . /OSMImport
WORKDIR /OSMImport
RUN stack setup
RUN stack install

ENV PATH $PATH:/root/.local/bin

ENTRYPOINT ["OSMImport"]
