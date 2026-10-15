"""Compare parser-visible source records; executable Unity topology is outside this oracle."""
import argparse, json, pathlib, subprocess, os, re, collections, time
ROOT=pathlib.Path(__file__).resolve().parents[2]
ENV=os.environ.copy()
LNM=ROOT/'.lake/build/bin/simai-parser-cli'
ORACLE=None
FLAGS=['isBreak','isEX','isHanabi','isSlideBreak','isSlideNoHead','isForceStar','isFakeRotate']
KINDS=['tap','slide','hold','touch','touchHold']
def run(exe,req):
 p=subprocess.run([str(exe)],input=json.dumps(req)+'\n',text=True,capture_output=True,env=ENV,timeout=90)
 if p.returncode: return {'ok':False,'error':p.stderr[:500]}
 lines=p.stdout.splitlines()
 return json.loads(next(x for x in reversed(lines) if x.startswith('{')))
def canon_lnm(tokens):
 out=[];i=0
 while i<len(tokens):
  t=tokens[i];i+=1
  if t['kind'] in ['rest','unknown'] or (not t['slot'] and not t['sensorPos']):continue
  raw=t['rawText'];length=t['length'] or 0
  if t['kind']=='slide' and t['sourceGroupIndex']==0:
   size=t['sourceGroupSize'];others=tokens[i:i+size-1];i+=size-1
   raw+=''.join(u['rawText'][1:] for u in others)
   length+=sum(u['length'] or 0 for u in others)
  rec={'raw':re.sub('[bx!?$]','',raw),'kind':t['kind'],'timing':t['timing'],
       'position':int(t['slot'][1:]) if t['slot'] else (8 if t['sensorPos']=='C' else int(t['sensorPos'][1:])),
       'area':t['sensorPos'][0] if t['sensorPos'] else ' ', 'length':length,
       'wait':t['starWait'] or 0,'bpm':t['bpm']['num']/t['bpm']['den'],'hSpeed':t['hSpeed']['num']/t['hSpeed']['den']}
  rec.update({f:t[f] for f in FLAGS});out.append(rec)
 return out
def canon_ref(result):
 out=[]
 for e in result['notes']:
  n=e['note'];kind=KINDS[n['Type']]
  r={'raw':n['RawContent'],'kind':kind,'timing':round(e['timing']*1e6),'position':n['StartPosition'],
     'area':n['TouchArea'],'length':round((n['SlideTime'] if kind=='slide' else n['HoldTime'])*1e6),
     'wait':round((n['SlideStartTime']-(e['timing']-result['Offset']))*1e6) if kind=='slide' else 0,
     'bpm':e['bpm'],'hSpeed':e['hSpeed']}
  r.update({f:n['IsEx' if f=='isEX' else 'I'+f[1:]] for f in FLAGS});out.append(r)
 return out
def compare(a,b):
 counts=collections.Counter();examples=[];max_deltas={}
 if len(a)!=len(b):return {'count':(len(a),len(b))}
 for i,(x,y) in enumerate(zip(a,b)):
  diffs={k:(x[k],y[k]) for k in x if x[k]!=y[k]}
  if diffs:
   counts.update(diffs.keys())
   for k in ('timing','length','wait'):
    if k in diffs:max_deltas[k]=max(max_deltas.get(k,0),abs(x[k]-y[k]))
   if len(examples)<3: examples.append({'i':i,'raw':x['raw'],'diff':diffs})
 return {'notes':len(a),'differences':dict(counts),'maxMicrosDelta':max_deltas,'examples':examples}
def main():
 global ORACLE,LNM
 parser=argparse.ArgumentParser(description=__doc__)
 parser.add_argument('--oracle',required=True,type=pathlib.Path)
 parser.add_argument('--cli',type=pathlib.Path,default=LNM)
 parser.add_argument('--charts',required=True,type=pathlib.Path,help='MajDataPlay Original chart directory')
 parser.add_argument('--output',type=pathlib.Path,default=pathlib.Path('/tmp/lnmai-parity-results.json'))
 args=parser.parse_args();ORACLE=args.oracle;LNM=args.cli
 files=sorted(set([*ROOT.glob('tools/assets/*/maidata.txt'),*(ROOT/'../assets').glob('*/maidata.txt'),*args.charts.glob('*/maidata.txt')]))
 rows=[]
 for file in files:
  content=file.read_text(encoding='utf-8-sig')
  levels=sorted(set(map(int,re.findall(r'(?m)^\s*&inote_(\d+)\s*=',content))))
  for level in levels:
   req={'mode':'inspection','content':content,'levelIndex':level}
   a=run(LNM,req); b=run(ORACLE,req)
   row={'file':str(file),'level':level,'lnmOk':a['ok'],'refOk':b['ok']}
   if a['ok'] and b['ok']:
    row.update(compare(canon_lnm(a['result']['tokens']),canon_ref(b)))
   else: row.update({'lnmError':a.get('error'),'refError':b.get('error')})
   rows.append(row)
  print(file.parent.name, len(levels),flush=True)
 args.output.write_text(json.dumps(rows,indent=2,ensure_ascii=False))
 print('Charts',len(rows),'accepted',sum(r['lnmOk'] for r in rows),'reference accepted',sum(r['refOk'] for r in rows))
 for r in rows:
  if not r['lnmOk'] or r.get('count') or r.get('differences'):print(json.dumps(r,ensure_ascii=False))
if __name__=='__main__':main()
