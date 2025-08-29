ARG TAG=master
FROM ocaml/opam:ubuntu-lts-ocaml-5.2

# Install pre-requisites
RUN sudo apt-get update && \
    sudo DEBIAN_FRONTEND=noninteractive \
      apt-get -y install libgmp-dev pkg-config

# Clone herdtools7
RUN git clone https://github.com/herd/herdtools7

# Install herdtools7 suite at specified tag
RUN cd herdtools7 && git checkout ${TAG} && opam install .
