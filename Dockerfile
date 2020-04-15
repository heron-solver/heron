FROM shwestrick/mpl
WORKDIR /heron
COPY . /heron
RUN apt install -y --force-yes time
# RUN git clone https://github.com/heron-solver/heron .
RUN make
RUN chmod +x /heron/lib/docker-motd.sh
RUN echo "/heron/lib/docker-motd.sh" >> ~/.bashrc
RUN mkdir ~/.ssh
RUN echo "Host github.com" >> ~/.ssh/config
RUN echo "    StrictHostKeyChecking no" >> ~/.ssh/config
ENV PATH="/heron:${PATH}"
