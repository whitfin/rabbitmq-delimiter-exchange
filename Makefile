PROJECT = rabbitmq_delimiter_exchange
PROJECT_DESCRIPTION = RabbitMQ Delimiter Exchange Type
PROJECT_MOD = rabbitmq_delimiter_exchange
PROJECT_VERSION := 1.0.0

DEPS = rabbit_common
DEP_EARLY_PLUGINS = rabbit_common/mk/rabbitmq-early-plugin.mk
DEP_PLUGINS = rabbit_common/mk/rabbitmq-plugin.mk
RABBITMQ_COMMIT ?= master

dep_rabbit_common = git https://github.com/rabbitmq/rabbitmq-common $(RABBITMQ_COMMIT)

include erlang.mk
