#!/usr/bin/env python3
"""A very small IRC server, for trying the irc example without the Internet.

    python3 irc-test-server.py [port]        (default 6667, on 127.0.0.1)

From the DOS machine in QEMU the host is 10.0.2.2:
    (doslib-examples:run "irc" "10.0.2.2" 6667 "dosuser" "#test")

It knows NICK, USER, JOIN, PART, PRIVMSG, PING and QUIT, and one channel
is as good as another.  It refuses a nick that is in use (433) and sends
one PING after the welcome.  A robot called "host" is in every channel: it
greets whoever joins and repeats what is said to it.
"""
import socket, sys, threading

clients = {}            # socket -> nick
lock = threading.Lock()

def send(sock, line):
    try: sock.sendall((line + "\r\n").encode("latin-1"))
    except OSError: pass

def broadcast(line, skip=None):
    with lock:
        for sock in list(clients):
            if sock is not skip: send(sock, line)

def serve(sock):
    nick, data, user, welcomed = None, b"", False, False
    try:
        while True:
            chunk = sock.recv(4096)
            if not chunk: break
            data += chunk
            while b"\n" in data:
                raw, data = data.split(b"\n", 1)
                line = raw.decode("latin-1").rstrip("\r")
                print("<-", line, flush=True)
                command, _, rest = line.partition(" ")
                command = command.upper()
                if command == "NICK":
                    wanted = rest.strip().lstrip(":")
                    with lock: taken = wanted in clients.values()
                    if taken:
                        send(sock, f":test.server 433 * {wanted} :Nickname is already in use")
                        continue
                    old, nick = nick, wanted
                    with lock: clients[sock] = nick
                    if old: broadcast(f":{old}!u@dos NICK :{nick}")
                elif command == "USER":
                    user = True
                # the welcome waits for both: the first NICK may have been refused
                if command in ("NICK", "USER") and nick and user and not welcomed:
                    welcomed = True
                    send(sock, f":test.server 001 {nick} :Welcome to the test server, {nick}")
                    send(sock, "PING :test.server")     # a client must answer with PONG
                if command == "JOIN":
                    channel = rest.strip()
                    broadcast(f":{nick}!u@dos JOIN :{channel}")
                    send(sock, f":test.server 353 {nick} = {channel} :{nick} host")
                    send(sock, f":host!h@mac PRIVMSG {channel} :hello {nick}, welcome to {channel}")
                elif command == "PRIVMSG":
                    target, _, text = rest.partition(" :")
                    broadcast(f":{nick}!u@dos PRIVMSG {target} :{text}", skip=sock)
                    # the robot answers in the channel, or privately to whoever wrote to it
                    if not text.startswith("\x01") and (target.startswith("#") or target == "host"):
                        reply_to = target if target.startswith("#") else nick
                        send(sock, f":host!h@mac PRIVMSG {reply_to} :you said: {text}")
                elif command == "PING":
                    send(sock, f":test.server PONG test.server :{rest.lstrip(':')}")
                elif command == "PART":
                    broadcast(f":{nick}!u@dos PART {rest.strip()}")
                elif command == "QUIT":
                    return
    finally:
        with lock: clients.pop(sock, None)
        if nick: broadcast(f":{nick}!u@dos QUIT :gone")
        sock.close()

port = int(sys.argv[1]) if len(sys.argv) > 1 else 6667
listener = socket.socket()
listener.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
listener.bind(("127.0.0.1", port))
listener.listen()
print("IRC test server on 127.0.0.1:%d" % port, flush=True)
while True:
    connection, _ = listener.accept()
    threading.Thread(target=serve, args=(connection,), daemon=True).start()
