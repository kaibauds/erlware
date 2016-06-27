user_fifo
=========

**An Erlang/OTP application that provides FIFO queuing service for each client that connects to this service through TCP port ( default 48088 ).**
**Started from version 0.2, user_fifo supports persisted FIFO queque for clients.**


Design
------

user_fifo_server is a coordinator and persistor implemeted with Erlang/OTP gen_server. It hooks up a socket connection and a fifo queue.

user_fifo_all_channels_sup is an Erlang/OTP supervisor that monitors all the user channels.

user_info_channle_sup is a supervisor, which will be created for each client channel, and it will start two workers: the channel and the fifo both are 'gen_server'.

fifo uses a simple queue implemention to serve two basic FIFO operation: in and out.

*Note about the up-to-date version:*

There are many ways handling persistence, mnesia for example; however, with current version of 0.2, user_fifo demonstrates the way of using a persistor server.
With v0.2, the queue will be stored only when the exit of the fifo is expected ( logout ) or trapped so that the code of "terminate" call back will be called. If the process is killed when it's receving (waiting for the message) at the socket, gen_server fifo's "termninate" callback won't be called, so the messages will lost.
Updating the service to TCP "active" mode to handle the TCP message as Erlang process message may improve it so that the gen_server can stay in state-concisous way so that it may always be able to be closed gracefully.  


Build
-----

```bash
$ rebar3 compile
```


Run
---

```bash
$ rebar3 shell
```

Test
----

Use "nc" on Linux

```bash
$ nc localhost 48088
Please enter a line in the format "log in as <user name>" in a minute or you will be disconnected
log in as Kai
Welcome Kai
in message #1
done
in message #2
done
in message #3
done
out
message #1
exit
$ nc localhost 48088
Please enter a line in the format "log in as <user name>" in a minute or you will be disconnected
log in as Kai
Welcome Kai
in message #1
done
out
message #2
^Z
[1]+  Stopped                 nc localhost 48088
$ nc localhost 48088
Please enter a line in the format "log in as <user name>" in a minute or you will be disconnected
log in as Kai
Welcome Kai
in message #1
done
out
message #3
out
message #1
out
message #1
out
(no more message)
out
(no more message)
exit
$ fg
nc localhost 48088
$
```

