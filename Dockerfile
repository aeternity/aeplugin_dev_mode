FROM aeternity/builder:bionic-otp23 as pluginbuild
ADD rebar.config rebar.lock rebar.config.script /aeplugin_dev_mode/
ADD src /aeplugin_dev_mode/src/

RUN cd /aeplugin_dev_mode && rebar3 compile

FROM aeternity/aeternity:master

COPY --from=pluginbuild /aeplugin_dev_mode/_build/default/lib/aeplugin_dev_mode /home/aeternity/node/plugins/aeplugin_dev_mode

# The priv/ and examples/devmode.yaml need to be copied explicitly
ADD priv /home/aeternity/node/plugins/aeplugin_dev_mode/priv
ADD examples/devmode.yaml /home/aeternity/node/devmode.yaml

EXPOSE 3313

ENV AETERNITY_CONFIG=/home/aeternity/node/devmode.yaml
