PROJECT = rabbitmq_delimiter_exchange
PROJECT_DESCRIPTION = RabbitMQ Delimiter Exchange Type
PROJECT_MOD = rabbitmq_delimiter_exchange

DEPS = rabbit_common
TEST_DEPS = rabbitmq_ct_helpers rabbitmq_ct_client_helpers amqp_client

DEP_EARLY_PLUGINS = rabbit_common/mk/rabbitmq-early-plugin.mk
DEP_PLUGINS = rabbit_common/mk/rabbitmq-plugin.mk

include rabbitmq-components.mk
include erlang.mk
