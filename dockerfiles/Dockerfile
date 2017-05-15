FROM erlang:19

MAINTAINER Joao Sousa <jpd.sousa@campus.fct.unl.pt>

ENV HANDOFF_PORT "8099"
ENV PB_PORT "8087"
ENV PB_IP "0.0.0.0"
ENV PBSUB_PORT "8086"
ENV LOGREADER_PORT "8085"
ENV RING_STATE_DIR "data/ring"
ENV PLATFORM_DATA_DIR "data"
ENV NODE_NAME "antidote@127.0.0.1"
ENV AQL_NAME "aql@127.0.0.1"
ENV SHORT_NAME "false"
ENV ANTIDOTE_REPO "https://github.com/SyncFree/antidote.git"
ENV ANTIDOTE_BRANCH "master"
ENV AQL_REPO "https://JPDSousa@bitbucket.org/JPDSousa/aql.git"
ENV AQL_BRANCH "master"

# Install basic dependencies
RUN set -xe \
  && apt-get update \
  && apt-get install -y --no-install-recommends git openssl ca-certificates

# AntidoteDB
RUN cd /usr/src \
  && git clone $ANTIDOTE_REPO \
  && cd antidote \
  && git checkout $ANTIDOTE_BRANCH \
  && make rel \
  && cp -R _build/default/rel/antidote /opt/ \
  && sed -e '$i,{kernel, [{inet_dist_listen_min, 9100}, {inet_dist_listen_max, 9100}]}' /usr/src/antidote/_build/default/rel/antidote/releases/0.0.1/sys.config > /opt/antidote/releases/0.0.1/sys.config \
  && rm -rf /usr/src/antidote /var/lib/apt/lists/*

#AQL
RUN cd ../ \
  && git clone $AQL_REPO \
  && cd aql \
  && git checkout $AQL_BRANCH \
  && ./rebar3 compile

ADD ./start_and_attach.sh /opt/antidote/
ADD ./entrypoint.sh /

RUN chmod a+x /opt/antidote/start_and_attach.sh \
  && chmod a+x /entrypoint.sh

# Distributed Erlang Port Mapper
EXPOSE 4368
# Ports for Antidote
EXPOSE 8085 8086 8087 8099

# Antidote RPC
EXPOSE 9100

VOLUME /opt/antidote/data

ENTRYPOINT ["/entrypoint.sh"]

CMD ["/opt/antidote/start_and_attach.sh"]
