"""Type example scripts into a real R PTY and save timed asciicast v2 output."""
from pathlib import Path
import argparse, codecs, errno, fcntl, json, os, pty, random, re, select
import shutil, struct, subprocess, termios, time
ROOT = Path(__file__).resolve().parents[1]
COLS, ROWS = (102, 28)

class Recording:

    def __init__(self, name, output_dir, r_command):
        self.name = name
        self.events = []
        self.marks = []
        self.transcript = ''
        r_binary = shutil.which(r_command)
        if r_binary is None:
            raise FileNotFoundError('R was not found. Put R on PATH or pass --r /path/to/R.')
        self.output = output_dir
        for folder in ['build', 'qa', 'work']:
            (self.output / folder).mkdir(parents=True, exist_ok=True)
        (self.output / 'build' / f'{self.name}-live-output.txt').write_text('')
        self.master, slave = pty.openpty()
        fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack('HHHH', ROWS, COLS, 0, 0))
        self.work = self.output / 'work' / name
        self.work.mkdir(exist_ok=True)
        shutil.copy2(ROOT / 'data/my_addresses.csv', self.work / 'my_addresses.csv')
        shutil.copy2(ROOT / 'data/reference_addresses.csv', self.work / 'reference_addresses.csv')
        env = os.environ.copy()
        env.update(TERM='xterm-256color', COLUMNS=str(COLS), LINES=str(ROWS), R_PROFILE_USER=str(ROOT / 'source/recording-profile.R'), R_ENVIRON_USER='/dev/null', R_HISTFILE='/dev/null', NO_COLOR='1', R_CLI_NUM_COLORS='0')
        self.start = time.monotonic()
        self.process = subprocess.Popen([r_binary, '--quiet', '--no-save', '--no-restore', '--no-readline'], stdin=slave, stdout=slave, stderr=slave, cwd=self.work, env=env, start_new_session=True)
        os.close(slave)
        os.set_blocking(self.master, False)
        self.decoder = codecs.getincrementaldecoder('utf-8')('replace')

    def pump(self, timeout=0.03):
        if not select.select([self.master], [], [], timeout)[0]:
            return ''
        try:
            data = os.read(self.master, 65536)
        except OSError as e:
            if e.errno == errno.EIO:
                return ''
            raise
        chunk = self.decoder.decode(data)
        if chunk:
            with (self.output / 'build' / f'{self.name}-live-output.txt').open('a') as live:
                live.write(chunk)
            self.events.append([round(time.monotonic() - self.start, 5), 'o', chunk])
            self.transcript += chunk
        return chunk

    def pause(self, seconds):
        end = time.monotonic() + seconds
        while time.monotonic() < end:
            self.pump(min(0.04, max(0, end - time.monotonic())))

    def prompt(self, timeout=120):
        since = ''
        end = time.monotonic() + timeout
        while time.monotonic() < end:
            since += self.pump(0.05)
            plain = re.sub('\\x1b\\[[0-?]*[ -/]*[@-~]', '', since)
            if re.search('(?:^|[\\r\\n])> $', plain):
                return since
            if self.process.poll() is not None:
                raise RuntimeError('R stopped unexpectedly: ' + since[-2000:])
        raise TimeoutError('R did not return to its prompt: ' + since[-2000:])

    def type_line(self, line):
        self.marks.append(dict(time=round(time.monotonic() - self.start, 5), command=line))
        for char in line:
            os.write(self.master, char.encode())
            delay = 0.025 + random.random() * 0.014
            if char in ',)':
                delay += 0.016
            self.pause(delay)
        self.pause(0.22)
        os.write(self.master, b'\n')
        output = self.prompt()
        if re.search('(?:^|[\\r\\n])Error(?: in|:)', output):
            raise RuntimeError(output)
        if line.startswith('#'):
            hold = 1.1
        elif line in ['addresses', 'reference', 'matched']:
            hold = 5.0
        elif line.startswith('table('):
            hold = 4
        elif line.startswith('write.csv'):
            hold = 4
        elif line.startswith('library'):
            hold = 0.6
        else:
            hold = 0.8
        self.pause(hold)

    def run(self):
        self.prompt()
        self.pause(0.7)
        source = (ROOT / 'source' / f'{self.name}.R').read_text()
        for line in source.splitlines():
            if line.strip():
                print(self.name + ': ' + line, flush=True)
                self.type_line(line)
        self.pause(2)
        duration = round(time.monotonic() - self.start, 5)
        self.events.append([duration, 'o', ''])
        header = dict(version=2, width=COLS, height=ROWS, timestamp=int(time.time()), title=self.name.replace('-', ' '), env={'TERM': 'xterm-256color'})
        path = self.output / f'{self.name}.cast'
        with path.open('w') as f:
            f.write(json.dumps(header) + '\n')
            for event in self.events:
                f.write(json.dumps(event, ensure_ascii=False) + '\n')
        (self.output / 'qa' / f'{self.name}-commands.json').write_text(json.dumps(self.marks, indent=2))
        (self.output / 'qa' / f'{self.name}-raw-output.txt').write_text(self.transcript)
        print(f'RECORDED {self.name}: {duration:.2f} seconds', flush=True)

    def close(self):
        if self.process.poll() is None:
            self.process.terminate()
            try:
                self.process.wait(timeout=10)
            except subprocess.TimeoutExpired:
                self.process.kill()
                self.process.wait()
        os.close(self.master)
if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--episode', choices=['address-matching', 'range-geocoding'])
    parser.add_argument('--output-dir', type=Path, default=ROOT, help='Recording destination (default: demo-videos/console).')
    parser.add_argument('--r', default='R', help='R executable name or path.')
    args = parser.parse_args()
    random.seed(124)
    for name in [args.episode] if args.episode else ['address-matching', 'range-geocoding']:
        recording = Recording(name, args.output_dir.resolve(), args.r)
        try:
            recording.run()
        finally:
            recording.close()
