FROM ubuntu:noble

## NOTA BENE: In order for this to be built successfully, you have to run ./devops/coreNLP/build.sh first.

ARG DEBIAN_FRONTEND=noninteractive
ARG GHC=9.4.7
ARG CORENLP=4.5.4
ARG CORE
COPY ./shell.nix                                       /builds/gargantext/shell.nix
COPY ./nix/pkgs.nix                                    /builds/gargantext/nix/pkgs.nix
COPY ./nix/pinned-23.11.nix                            /builds/gargantext/nix/pinned-23.11.nix
COPY ./devops/coreNLP/build.sh                         /root/devops/coreNLP/build.sh
COPY ./devops/coreNLP/startServer.sh                   /root/devops/coreNLP/startServer.sh
COPY ./bin/setup-ci-environment                        /builds/gargantext/bin/setup-ci-environment
COPY ./bin/install-cabal2stack                         /builds/gargantext/bin/install-cabal2stack

ENV TZ=Europe/Rome
RUN apt-get update && \
    apt-get install --no-install-recommends -y \
      apt-transport-https \
      autoconf \
      automake \
      build-essential \
      ca-certificates \
      curl \
      gcc \
      git \
      gnupg2 \
      libffi-dev \
      libffi8 \
      libgmp-dev \
      libgmp10 \
      libncurses-dev \
      libncurses6 \
      libnuma-dev \
      libtinfo6 \
      locales \
      lsb-release \
      software-properties-common \
      strace \
      sudo \
      wget \
      vim \
      xz-utils \
      zlib1g-dev \
      openjdk-21-jdk \
      unzip && \
    apt-get clean && rm -rf /var/lib/apt/lists/* && \
    mkdir -m 0755 /nix && groupadd -r nixbld && chown root /nix && \
    for n in $(seq 1 10); do useradd -c "Nix build user $n" -d /var/empty -g nixbld -G nixbld -M -N -r -s "$(command -v nologin)" "nixbld$n"; done

RUN gpg --batch --keyserver keys.openpgp.org     --recv-keys 7D1E8AFD1D4A16D71FADA2F2CCC85C0E40C06A8C && \
    gpg --batch --keyserver keyserver.ubuntu.com --recv-keys FE5AB6C91FEA597C3B31180B73EDE9E8CFBAEF01
SHELL ["/bin/bash", "-o", "pipefail", "-c"]

RUN cd /root/devops/coreNLP; ./build.sh

RUN set -o pipefail && \
    bash <(curl -L https://releases.nixos.org/nix/nix-2.15.0/install) --no-daemon && \
    locale-gen en_US.UTF-8 && chown root -R /nix

ENV LANG='en_US.UTF-8' LANGUAGE='en_US:en' LC_ALL='en_US.UTF-8'
ENV USER=root
ENV SHELL /bin/bash
RUN . "$HOME/.nix-profile/etc/profile.d/nix.sh" && \
    mkdir -p "/builds/gargantext/" && chmod 777 -R "/builds/gargantext" && \
    echo "source $HOME/.nix-profile/etc/profile.d/nix.sh" >> "$HOME/.bashrc" && \
    echo `which nix-env`

ENV PATH=/root/.nix-profile/bin:$PATH

RUN . $HOME/.bashrc && nix-env --version

ENV PATH=/root/.local/bin:$PATH

RUN cd /builds/gargantext && nix-shell --run "./bin/install-cabal2stack"
WORKDIR "/builds/gargantext/"

