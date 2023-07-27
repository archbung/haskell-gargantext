#!/bin/bash

# releases are here:
# https://stanfordnlp.github.io/CoreNLP/history.html

VERSION=4.5.4
FILE="stanford-corenlp-${VERSION}.zip"
DIR_V="stanford-corenlp-${VERSION}"
DIR="stanford-corenlp-current"
URL="http://nlp.stanford.edu/software/${FILE}"

[ ! -f ${FILE} ] && echo 'Fetching file' && wget ${URL} -O ${FILE}

[ ! -d ${DIR_V} ] && echo 'Unzipping file' && unzip ./${FILE}

[ ! -L ${DIR} ] && echo "Symlinking ${DIR_V} -> ${DIR}" && ln -s ${DIR_V} ${DIR}

[ ! -f ${DIR}/startServer.sh ] && echo "Copying startServer.sh" && cp ./startServer.sh ${DIR}/

echo "You can now build with: docker build -t cgenie/corenlp-garg:${VERSION}" --pull .
