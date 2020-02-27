const amqp = require('amqplib');
const should = require('should');

let _connection;
let _cleanupTasks = [];

suite('RabbitMQ Delimiter Exchange', function () {

    suiteSetup('start connection', async function () {
        _connection = await amqp.connect();

        _scheduleForCleanup(async function () {
            await _connection.close();
        });
    });

    test('creating an x-delimiter exchange', async function () {
        let { name, channel } = await _createExchange();
    });

    test('receiving messages matching a binding', async function () {
        let { name, channel } = await _createExchange();

        let exchange = name;
        let queue_name = _name();
        let routing_key = _name();
        let message_bytes = Buffer.from(_name());

        await _createQueue(channel, queue_name);
        await _bindQueue(channel, queue_name, exchange, routing_key);
        await _publish(channel, exchange, _join(routing_key), message_bytes);
        await _consume(channel, queue_name, function validate(message) {
            should(message.content).eql(message_bytes);
        });
    });

    test('receiving messages matching a binding after a delimiter', async function () {
        let { name, channel } = await _createExchange();

        let exchange = name;
        let queue_name = _name();
        let routing_key1 = _name();
        let routing_key2 = _name();
        let publish_key = _join(routing_key1, routing_key2);
        let message_bytes = Buffer.from(_name());

        await _createQueue(channel, queue_name);
        await _bindQueue(channel, queue_name, exchange, routing_key2);
        await _publish(channel, exchange, publish_key, message_bytes);
        await _consume(channel, queue_name, function validate(message) {
            should(message.content).eql(message_bytes);
        });
    });

    test('receiving only a single copy with multiple binding matches', async function () {
        let { name, channel } = await _createExchange();

        let exchange = name;
        let queue_name = _name();
        let routing_key1 = _name();
        let routing_key2 = _name();
        let publish_key = _join(routing_key1, routing_key2);
        let message_bytes = Buffer.from(_name());

        await _createQueue(channel, queue_name);
        await _bindQueue(channel, queue_name, exchange, routing_key1);
        await _bindQueue(channel, queue_name, exchange, routing_key2);
        await _publish(channel, exchange, publish_key, message_bytes);
        await _consume(channel, queue_name, function validate(message) {
            should(message.content).eql(message_bytes);
        });
    });

    test('splitting on the delimiter runs a global scan', async function () {
        let { name, channel } = await _createExchange();

        let exchange = name;
        let queue_name = _name();
        let routing_key1 = _name();
        let routing_key2 = _name();
        let routing_key3 = _name();
        let publish_key = _join(routing_key1, routing_key2, routing_key3);
        let message_bytes = Buffer.from(_name());

        await _createQueue(channel, queue_name);
        await _bindQueue(channel, queue_name, exchange, routing_key3);
        await _publish(channel, exchange, publish_key, message_bytes);
        await _consume(channel, queue_name, function validate(message) {
            should(message.content).eql(message_bytes);
        });
    });

    test('treating a trailing delimiter as an empty routing match', async function () {
        let { name, channel } = await _createExchange();

        let exchange = name;
        let queue_name = _name();
        let message_bytes = Buffer.from(_name());

        await _createQueue(channel, queue_name);
        await _bindQueue(channel, queue_name, exchange, '');
        await _publish(channel, exchange, ':', message_bytes);
        await _consume(channel, queue_name, function validate(message) {
            should(message.content).eql(message_bytes);
        });
    });

    test('gracefully failing when no routing key is provided', async function () {
        let { name, channel } = await _createExchange();

        let exchange = name;
        let queue_name = _name();
        let message_bytes = Buffer.from(_name());

        await _createQueue(channel, queue_name);
        await _bindQueue(channel, queue_name, exchange, '');
        await _publish(channel, exchange, '', message_bytes);
        await _consume(channel, queue_name, function validate() {
            should.fail('Message should never be received!');
        }, true);
    });

    test('gracefully failing when no bindings match the routing key', async function () {
        let { name, channel } = await _createExchange();

        let exchange = name;
        let queue_name = _name();
        let routing_key = _name();
        let message_bytes = Buffer.from(_name());

        await _createQueue(channel, queue_name);
        await _publish(channel, exchange, _join(routing_key), message_bytes);
        await _consume(channel, queue_name, function validate() {
            should.fail('Message should never be received!');
        }, true);
    });

    suiteTeardown('close connection', async function () {
        for (let task of _cleanupTasks) {
            await task();
        }
    });

});

/* Private helpers */

async function _bindQueue(channel, queue, exchange, key) {
    await channel.bindQueue(queue, exchange, key, {});
}

async function _createExchange() {
    let channel = await _connection.createChannel();

    _scheduleForCleanup(async function () {
        await channel.close();
    });

    let name = _name();
    let result = await channel.assertExchange(name, 'x-delimiter', {});

    should(result).be.an.Object();
    should(result).have.property('exchange');
    should(result.exchange).eql(name);

    _scheduleForCleanup(async function () {
        return channel.deleteExchange(name, {});
    });

    return { name, channel };
}

async function _createQueue(channel, queue) {
    let result = await channel.assertQueue(queue, { durable: false });

    should(result).be.an.Object();
    should(result).have.property('queue');
    should(result.queue).eql(queue);
}

async function _consume(channel, queue, validator, exit) {
    return new Promise(function (resolve, reject) {
        channel
            .consume(queue, function (message) {
                validator(message);
                resolve();
            }, {})
            .then(function (result) {
                try {
                    should(result).be.an.Object();
                    should(result).have.property('consumerTag');
                } catch (err) {
                    reject(err);
                }
                exit && resolve();
            })
            .catch(function (err) {
                reject(err);
            });
    });
}

async function _publish(channel, exchange, key, message) {
    await channel.publish(exchange, key, message, {});
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
