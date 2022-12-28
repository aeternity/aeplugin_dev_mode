FROM aeternity/builder:bionic-otp23 as pluginbuild
ADD rebar.config rebar.lock /aeplugin_dev_mode/
ADD priv /aeplugin_dev_mode/priv/
ADD src /aeplugin_dev_mode/src/
ADD include /aeplugin_dev_mode/include/

RUN cd /aeplugin_dev_mode && rebar3 ae_plugin

FROM aeternity/aeternity:master

COPY --from=pluginbuild /aeplugin_dev_mode/_build/default/aeplugin_dev_mode.ez /home/aeternity/node/plugins/

# The priv/ and examples/devmode.yaml need to be copied explicitly
ADD examples/devmode.yaml /home/aeternity/node/devmode.yaml

EXPOSE 3313

ENV AETERNITY_CONFIG=/home/aeternity/node/devmode.yaml
