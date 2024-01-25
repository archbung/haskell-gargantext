FROM adinapoli/gargantext:v3.1

## NOTA BENE: In order for this to be built successfully, you have to run ./devops/coreNLP/build.sh first.

ARG DEBIAN_FRONTEND=noninteractive
COPY ./shell.nix                                       /builds/gargantext/shell.nix
COPY ./nix/pkgs.nix                                    /builds/gargantext/nix/pkgs.nix
COPY ./nix/pinned-23.11.nix                            /builds/gargantext/nix/pinned-23.11.nix
COPY ./devops/coreNLP/build.sh                         /root/devops/coreNLP/build.sh
COPY ./devops/coreNLP/startServer.sh                   /root/devops/coreNLP/startServer.sh

ENV TZ=Europe/Rome

SHELL ["/bin/bash", "-o", "pipefail", "-c"]

ENV LANG='en_US.UTF-8' LANGUAGE='en_US:en' LC_ALL='en_US.UTF-8'
ENV USER=root
ENV SHELL /bin/bash
ENV PATH=/root/.nix-profile/bin:$PATH
ENV PATH=/root/.local/bin:$PATH

RUN cd /builds/gargantext && nix-shell
WORKDIR "/builds/gargantext/"
