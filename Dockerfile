FROM shwestrick/mpl
WORKDIR /heron
RUN apt install -y --force-yes time
RUN git clone https://github.com/heron-solver/heron .
RUN make
RUN chmod +x /heron/lib/docker_motd.sh
RUN echo "/heron/lib/docker-motd.sh" >> ~/.bashrc
ENV PATH="/heron:${PATH}"
