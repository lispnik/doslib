# Host side of the COM1 demo: talk to the Lisp listener through QEMU's serial port
import socket, sys, time
s = socket.socket(socket.AF_UNIX)
for _ in range(100):
    try: s.connect('ser.sock'); break
    except Exception: time.sleep(0.2)
s.settimeout(90)
def until_prompt():
    data = b''
    while not data.endswith(b'COM1> ') and b'Goodbye.' not in data:
        chunk = s.recv(4096)
        if not chunk: break
        data += chunk
    return data.decode('latin-1').replace('\r', '')
sys.stdout.write(until_prompt())
for form in open('forms.txt'):
    form = form.rstrip('\n')
    sys.stdout.write(form + '\n'); sys.stdout.flush()
    s.send(form.encode() + b'\r')
    sys.stdout.write(until_prompt())
print()
