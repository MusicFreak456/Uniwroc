import random

random.seed(None)

randomIntIP = lambda : random.randint(0,255)

lines = []

for i in range(20):
  time = "08:55:36"
  ip = ".".join( [str(randomIntIP()) for _ in range(4)] )
  method = "GET"
  resource = "/TheApplication/WebResource.axd"
  code = "200"
  line = " ".join([time, ip, method, resource, code])
  for _ in range(random.randint(1,100)):
    lines.append(line + '\n')

random.shuffle(lines)

with open('logs.txt', 'w') as file:
  for line in lines:
    file.write(line)