FROM aeternity/aeternity:master

RUN mkdir /home/aeternity/node/plugins
COPY ./_build/default/lib/aeplugin_dev_mode /home/aeternity/node/plugins/aeplugin_dev_mode
COPY ./examples/devmode.yaml /home/aeternity/node/devmode.yaml
RUN sed -i 's/{{PLUGIN_PATH}}/\/home\/aeternity\/node\/plugins/g' /home/aeternity/node/devmode.yaml

ENV AETERNITY_CONFIG=/home/aeternity/node/devmode.yaml