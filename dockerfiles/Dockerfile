FROM mweber/antidotedb

MAINTAINER Joao Sousa <jpd.sousa@campus.fct.unl.pt>

ENV AQL_REPO "git://github.com/JPDSousa/AQL.git"
ENV AQL_BRANCH "master"
ENV AQL_NAME "aql@127.0.0.1"

#AQL
RUN cd /usr/src \
  && git clone $AQL_REPO \
  && cd AQL \
  && git checkout $AQL_BRANCH \
  && ./rebar3 compile

ENTRYPOINT /usr/src/AQL/rebar3

CMD shell --name=$AQL_NAME --setcookie antidote
