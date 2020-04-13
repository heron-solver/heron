FROM shwestrick/mpl
WORKDIR /heron
RUN apt install -y --force-yes time
RUN git clone https://github.com/heron-solver/heron .
RUN make
ENV PATH="/heron:${PATH}"



