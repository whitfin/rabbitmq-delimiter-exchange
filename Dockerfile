ARG  RABBIT_VSN=latest
FROM rabbitmq:${RABBIT_VSN}

ADD . rabbitmq-delimiter-exchange
ENV LANG=C.UTF-8
RUN apt-get -y update && \
    apt-get -y install \
        curl \
        erlang-dev \
        erlang-src \
        git \
        make \
        python \
        zip && \
    cd rabbitmq-delimiter-exchange && \
    make && \
    make dist && \
    cp plugins/rabbitmq_delimiter_exchange-*.ez ../plugins/ && \
    cd - && \
    rm -rf rabbitmq-delimiter-exchange && \
    apt-get -y remove \
        curl \
        erlang-dev \
        erlang-src \
        git \
        make \
        python \
        zip && \
    apt-get -y autoremove && \
    apt-get clean && \
    rabbitmq-plugins enable --offline \
        rabbitmq_management \
        rabbitmq_delimiter_exchange

EXPOSE 15671 15672
