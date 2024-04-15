FROM aeternity/builder:bionic-otp23 as pluginbuild

ADD rebar.config rebar.lock /aeplugin_dev_mode/
RUN cd /aeplugin_dev_mode && rebar3 compile -d

ADD priv /aeplugin_dev_mode/priv/
ADD src /aeplugin_dev_mode/src/
ADD include /aeplugin_dev_mode/include/

RUN cd /aeplugin_dev_mode && rebar3 ae_plugin

FROM aeternity/aeternity:master

COPY --from=pluginbuild /aeplugin_dev_mode/_build/default/aeplugin_dev_mode.ez plugins/
RUN mkdir -p data/aeplugin_dev_mode

# The priv/ and examples/devmode.yaml need to be copied explicitly
ADD examples/devmode.yaml devmode.yaml

EXPOSE 3313

ENV AETERNITY_CONFIG=/home/aeternity/node/devmode.yaml
