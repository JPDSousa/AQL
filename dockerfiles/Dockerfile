FROM mweber/antidotedb

MAINTAINER Joao Sousa <jpd.sousa@campus.fct.unl.pt>

ENV AQL_REPO "https://JPDSousa@bitbucket.org/JPDSousa/aql.git"
ENV AQL_BRANCH "master"
ENV AQL_NAME "aql@127.0.0.1"

#AQL
RUN cd /usr/src \
  && git clone $AQL_REPO \
  && cd aql \
  && git checkout $AQL_BRANCH \
  && ./rebar3 compile

ENTRYPOINT ./rebar3

CMD --name=$AQL_NAME --setcookie antidote
