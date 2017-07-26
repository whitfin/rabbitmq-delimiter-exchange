const amqp = require('amqplib/callback_api');
const async = require('async');
const should = require('should');

let _connection;
let _cleanupTasks = [];

suite('RabbitMQ Delimiter Exchange', function () {

    suiteSetup('start connection', function (done) {
        amqp.connect(function (err, conn) {
            if (err) {
                return done(err);
            }

            _connection = conn;

            _scheduleForCleanup(function (complete) {
                _connection.close(complete);
            });

            done();
        });
    });

    test('creating an x-delimiter exchange', function (done) {
        _createExchange(function (err, channel, exchange) {
            if (err) {
                return done(err);
            }

            should(channel).be.ok();
            should(exchange).be.ok();

            done();
        });
    });

    test('receiving messages matching a binding', function (done) {
        _createExchange(function (err, channel, exchange) {
            let queue_name = _name();
            let routing_key = _name();
            let message_bytes = Buffer.from(_name());

            let steps = [
                _generateQueue(channel, queue_name),
                _generateBind(channel, queue_name, exchange, routing_key),
                _generatePublish(channel, exchange, _join(routing_key), message_bytes),
                _generateConsume(channel, queue_name, function validate(message) {
                    should(message.content).eql(message_bytes);
                })
            ];

            async.series(steps, done);
        });
    });

    test('receiving messages matching a binding after a delimiter', function (done) {
        _createExchange(function (err, channel, exchange) {
            let queue_name = _name();
            let routing_key1 = _name();
            let routing_key2 = _name();
            let publish_key = _join(routing_key1, routing_key2);
            let message_bytes = Buffer.from(_name());

            let steps = [
                _generateQueue(channel, queue_name),
                _generateBind(channel, queue_name, exchange, routing_key2),
                _generatePublish(channel, exchange, publish_key, message_bytes),
                _generateConsume(channel, queue_name, function validate(message) {
                    should(message.content).eql(message_bytes);
                })
            ];

            async.series(steps, done);
        });
    });

    test('receiving only a single copy with multiple binding matches', function (done) {
         _createExchange(function (err, channel, exchange) {
            let queue_name = _name();
            let routing_key1 = _name();
            let routing_key2 = _name();
            let publish_key = _join(routing_key1, routing_key2);
            let message_bytes = Buffer.from(_name());

            let steps = [
                _generateQueue(channel, queue_name),
                _generateBind(channel, queue_name, exchange, routing_key1),
                _generateBind(channel, queue_name, exchange, routing_key2),
                _generatePublish(channel, exchange, publish_key, message_bytes),
                _generateConsume(channel, queue_name, function validate(message) {
                    should(message.content).eql(message_bytes);
                })
            ];

            async.series(steps, done);
         });
    });

    test('splitting on the delimiter runs a global scan', function (done) {
         _createExchange(function (err, channel, exchange) {
            let queue_name = _name();
            let routing_key1 = _name();
            let routing_key2 = _name();
            let routing_key3 = _name();
            let publish_key = _join(routing_key1, routing_key2, routing_key3);
            let message_bytes = Buffer.from(_name());

            let steps = [
                _generateQueue(channel, queue_name),
                _generateBind(channel, queue_name, exchange, routing_key3),
                _generatePublish(channel, exchange, publish_key, message_bytes),
                _generateConsume(channel, queue_name, function validate(message) {
                    should(message.content).eql(message_bytes);
                })
            ];

            async.series(steps, done);
         });
    });

    test('treating a trailing delimiter as an empty routing match', function (done) {
        _createExchange(function (err, channel, exchange) {
            let queue_name = _name();
            let message_bytes = Buffer.from(_name());

            let steps = [
                _generateQueue(channel, queue_name),
                _generateBind(channel, queue_name, exchange, ''),
                _generatePublish(channel, exchange, ':', message_bytes),
                _generateConsume(channel, queue_name, function validate(message) {
                    should(message.content).eql(message_bytes);
                })
            ];

            async.series(steps, done);
         });
    });

    test('gracefully failing when no routing key is provided', function (done) {
        _createExchange(function (err, channel, exchange) {
            let queue_name = _name();
            let message_bytes = Buffer.from(_name());

            let steps = [
                _generateQueue(channel, queue_name),
                _generateBind(channel, queue_name, exchange, ''),
                _generatePublish(channel, exchange, '', message_bytes),
                _generateConsume(channel, queue_name, function validate(message) {
                    should.fail('Message should never be received!');
                }, true)
            ];

            async.series(steps, done);
         });
    });

    test('gracefully failing when no bindings match the routing key', function (done) {
        _createExchange(function (err, channel, exchange) {
            let queue_name = _name();
            let routing_key = _name();
            let message_bytes = Buffer.from(_name());

            let steps = [
                _generateQueue(channel, queue_name),
                _generatePublish(channel, exchange, _join(routing_key), message_bytes),
                _generateConsume(channel, queue_name, function validate(message) {
                    should.fail('Message should never be received!');
                }, true)
            ];

            async.series(steps, done);
         });
    });

    suiteTeardown('close connection', function (done) {
        async.eachSeries(_cleanupTasks, function (task, next) {
            task(next);
        }, done);
    });

});

/* Private helpers */

function _createExchange(handler) {
    _connection.createChannel(function (err, channel) {
        if (err) {
            return handler(err);
        }

        _scheduleForCleanup(function (complete) {
            channel.close(complete);
        });

        let exchange_name = _name();

        channel.assertExchange(exchange_name, 'x-delimiter', {}, function (err, ok) {
            if (err) {
                return handler(err);
            }

            should(ok).be.an.Object();
            should(ok).have.property('exchange');
            should(ok.exchange).eql(exchange_name);

            _scheduleForCleanup(function (completed) {
                channel.deleteExchange(exchange_name, {}, completed);
            });

            handler(undefined, channel, exchange_name);
        });
    });
}

function _generateBind(channel, queue, exchange, key) {
    return function bind(next) {
        channel.bindQueue(queue, exchange, key, {}, next);
    };
}

function _generateConsume(channel, queue, validator, exit) {
    return function consume(next) {
        channel.consume(
            queue,
            function (message) {
                validator(message);
                next();
            },
            {},
            function (err, ok) {
                if (err) {
                    return next(err);
                }
                should(ok).be.an.Object();
                should(ok).have.property('consumerTag');
                exit && process.nextTick(next);
            }
        );
    };
}

function _generateQueue(channel, queue) {
    return function create(next) {
        channel.assertQueue(queue, { durable: false }, function (err, ok) {
            if (err) {
                return next(err);
            }
            should(ok).be.an.Object();
            should(ok).have.property('queue');
            should(ok.queue).eql(queue);
            next();
        });
    };
}

function _generatePublish(channel, exchange, key, message) {
    return function publish(next) {
        channel.publish(exchange, key, message, {});
        next();
    };
}

function _join() {
    let key = '';
    for (var i = 0, j = arguments.length; i < j; i++) {
        key += ':' + arguments[i];
    }
    return key;
}

function _name() {
    return Math.random().toString(36).substring(7);
}

function _scheduleForCleanup(task) {
    _cleanupTasks.unshift(task);
}
