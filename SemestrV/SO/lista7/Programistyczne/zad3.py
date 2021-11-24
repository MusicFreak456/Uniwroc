from glob import glob
from io import TextIOWrapper
from typing import List, Dict

def mem(list: List[str], elem: str) -> int:
  try:
    return list.index(elem)
  except:
    return -1

def getProperties(file: TextIOWrapper, properties: List[str]) -> Dict:
  lines = file.read().split('\n')
  result = {}
  for line in lines:
    line_split = line.split(':')
    if len(line_split) != 2 : continue
    
    [prop, value] = line_split
    index = mem(properties, prop)
    if index != -1 :
      result[prop.strip()] = value.strip()
  return result

total = {
  "VmSize": 0,
  "VmRSS": 0
}

def updateTotal(total : Dict, props : Dict):
  for key, value in total.items():
    try: 
      total[key] = value + int(props[key].split(" ")[0])
    except:
      continue

for path in glob('/proc/[0-9]*'):
  with open(path + '/status', 'r') as file:
    props = getProperties(file, list(total.keys()))
    updateTotal(total, props)

print(total)