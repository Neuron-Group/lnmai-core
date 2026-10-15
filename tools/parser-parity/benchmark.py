"""Time equal successful corpus requests; includes process startup and JSON serialization."""
import argparse
import hashlib
import json
import pathlib
import statistics
import subprocess
import tempfile
import time


def run(cli, requests, validate=False):
    with tempfile.TemporaryFile(mode='w+') as source:
        for request in requests:
            source.write(json.dumps(request) + '\n')
        source.seek(0)
        start = time.perf_counter()
        if validate:
            process = subprocess.Popen([str(cli)], stdin=source, stdout=subprocess.PIPE)
            digest = hashlib.sha256()
            accepted = []
            for line in process.stdout:
                digest.update(line)
                accepted.append(json.loads(line)['ok'])
            if process.wait() != 0 or len(accepted) != len(requests):
                raise RuntimeError('CLI failed or returned the wrong number of responses')
            return accepted, digest.hexdigest()
        subprocess.run([str(cli)], stdin=source, stdout=subprocess.DEVNULL, check=True)
        return time.perf_counter() - start


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--baseline', type=pathlib.Path, required=True)
    parser.add_argument('--cli', type=pathlib.Path, required=True)
    parser.add_argument('--corpus-results', type=pathlib.Path, required=True)
    args = parser.parse_args()
    rows = json.loads(args.corpus_results.read_text())
    requests = [{'mode': 'normalized', 'content': pathlib.Path(row['file']).read_text(encoding='utf-8-sig'),
                 'levelIndex': row['level']} for row in rows]
    before_ok, _ = run(args.baseline, requests, True)
    after_ok, digest = run(args.cli, requests, True)
    assert run(args.cli, requests, True) == (after_ok, digest), 'nondeterministic output'
    common = [r for r, a, b in zip(requests, before_ok, after_ok) if a and b]
    samples = {'before': [], 'after': []}
    for _ in range(3):
        for name, cli in [('before', args.baseline), ('after', args.cli)]:
            samples[name].append(run(cli, common))
    print(json.dumps({'mode': 'normalized', 'charts': len(common), 'seconds': samples,
                      'medianSeconds': {k: statistics.median(v) for k, v in samples.items()},
                      'deterministicAfterSha256': digest}, indent=2))


if __name__ == '__main__':
    main()
