FROM aeternity/builder:1804 as pluginbuild
ADD rebar.config rebar.lock rebar.config.script /aeplugin_dev_mode/
ADD src /aeplugin_dev_mode/src/
ADD priv /aeplugin_dev_mode/priv/

RUN cd /aeplugin_dev_mode && rebar3 compile

FROM aeternity/aeternity:master

RUN mkdir /home/aeternity/node/plugins
COPY --from=pluginbuild /aeplugin_dev_mode/_build/default/lib/aeplugin_dev_mode /home/aeternity/node/plugins/aeplugin_dev_mode

# I have no idea why this extra step is needed
# But without it, the JSON-Schema file in priv isn't copied
COPY --from=pluginbuild /aeplugin_dev_mode/_build/default/lib/aeplugin_dev_mode/priv /home/aeternity/node/plugins/aeplugin_dev_mode/priv

COPY ./examples/devmode.yaml /home/aeternity/node/devmode.yaml

EXPOSE 3313

ENV AETERNITY_CONFIG=/home/aeternity/node/devmode.yaml AE__SYSTEM__PLUGIN_PATH=/home/aeternity/node/plugins
