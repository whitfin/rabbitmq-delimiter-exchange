FROM rabbitmq:3.6.10

ADD . rabbitmq-delimiter-exchange

ARG RABBITMQ_REF=rabbitmq_v3_6_10
RUN apt-get -y update && \
        DEBIAN_FRONTEND=noninteractive apt-get install -y locales && \
        sed -i 's/^# \(en_US.UTF-8 UTF-8\)/\1/' /etc/locale.gen && \
        locale-gen && \
        apt-get -y install build-essential elixir erlang-dev erlang-src git python zip && \
        cd rabbitmq-delimiter-exchange && \
        export LANG=en_US.UTF-8 && \
        export RABBITMQ_REF=${RABBITMQ_REF} && \
        make && \
        make dist && \
        cp plugins/rabbitmq_delimiter_exchange-*.ez ../plugins/ && \
        cd - && \
        apt-get -y remove build-essential elixir erlang-dev erlang-src git locales python zip && \
        apt-get -y autoremove && \
        apt-get clean && \
        rm -rf rabbitmq-delimiter-exchange && \
        rabbitmq-plugins enable --offline rabbitmq_management rabbitmq_delimiter_exchange

EXPOSE 15671 15672
