FROM emptystackexn/multimlton
WORKDIR /workspace
RUN git clone https://github.com/heron-solver/heron.git
RUN cd heron && make
ENV PATH="/workspace/heron:${PATH}"



