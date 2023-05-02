FROM ubuntu:focal-20230308

# STEP1 tools
RUN apt update && \
    DEBIAN_FRONTEND=noninteractive apt install -y --no-install-recommends \
    git \
    wget \
    curl \
    ca-certificates \
    vim \
    graphviz \
    firefox


# STEP2 python3
RUN DEBIAN_FRONTEND=noninteractive apt install -y --no-install-recommends \
    python3 \
    python3-pip && \
    python3 -m pip install numpy==1.24.2 matplotlib==3.7.1


# STEP3 racket8.3
RUN wget https://download.racket-lang.org/installers/8.3/racket-8.3-x86_64-linux-cs.sh && \
    bash racket-8.3-x86_64-linux-cs.sh --in-place --dest /usr/racket && \
    rm racket-8.3-x86_64-linux-cs.sh && \
    echo "export PATH=/usr/racket/bin:\$PATH" >> /root/.bashrc


# STEP4 rosette newest version at the time
RUN git clone https://github.com/emina/rosette.git && \
    cd rosette && git checkout 15647f24b4b942e5eaae19a8ec0fcd272ce7504f && cd .. && \
    /usr/racket/bin/raco pkg install --no-docs --copy --auto -i -t dir rosette && \
    rm -rf rosette


# STEP5 boolector 3.2.2
RUN DEBIAN_FRONTEND=noninteractive apt install -y --no-install-recommends \
    make \
    cmake \
    gcc \
    g++
RUN wget https://github.com/Boolector/boolector/archive/refs/tags/3.2.2.tar.gz && \
    tar -zxvf 3.2.2.tar.gz && \
    cd boolector-3.2.2 && \
    ./contrib/setup-lingeling.sh && \
    ./contrib/setup-btor2tools.sh && \
    ./configure.sh --prefix /usr/boolector && cd build && make -j8 && make install && \
    cd ../.. && rm -rf 3.2.2.tar.gz  boolector-3.2.2 && \
    echo "export PATH=/usr/boolector/bin:\$PATH" >> /root/.bashrc


# STEP6 DASK
RUN python3 -m pip install "dask[complete]"==2023.3.1

