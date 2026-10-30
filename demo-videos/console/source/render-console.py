"""Render an unedited timed terminal capture; no title cards or audio."""
from pathlib import Path
import argparse, functools, json, math, subprocess
from PIL import Image, ImageDraw, ImageFont
ROOT = Path(__file__).resolve().parents[1]
import imageio_ffmpeg, pyte
FFMPEG = imageio_ffmpeg.get_ffmpeg_exe()
W, H, FPS = (1920, 1080, 24)
FONT = None
CELL = 0
LINE = 36
X = 43
Y = 32
BG = '#151515'
FG = '#e7e7e7'

def load_font(path):
    candidates = [str(path)] if path else ['/System/Library/Fonts/Menlo.ttc', 'DejaVuSansMono.ttf', 'LiberationMono-Regular.ttf']
    for candidate in candidates:
        try:
            return ImageFont.truetype(candidate, 29)
        except OSError:
            pass
    raise FileNotFoundError('No monospaced font found. Pass --font /path/to/font.ttf.')

@functools.lru_cache(maxsize=1200)
def line_image(text):
    im = Image.new('RGB', (W, LINE), BG)
    d = ImageDraw.Draw(im)
    d.text((X, 0), text.rstrip(), font=FONT, fill=FG)
    return im

def read_cast(name, directory):
    records = [json.loads(line) for line in (directory / f'{name}.cast').read_text().splitlines()]
    header = records[0]
    events = records[1:]
    assert header['version'] == 2
    assert all((e[1] == 'o' for e in events))
    assert all((a[0] <= b[0] for a, b in zip(events, events[1:])))
    return (header, events)

def render(name, directory, preview=False, font_path=None):
    global FONT, CELL
    FONT = load_font(font_path)
    CELL = FONT.getlength('M')
    line_image.cache_clear()
    for folder in ['build', 'qa']:
        (directory / folder).mkdir(parents=True, exist_ok=True)
    header, events = read_cast(name, directory)
    screen = pyte.Screen(header['width'], header['height'])
    stream = pyte.Stream(screen)
    duration = events[-1][0]
    frames = math.ceil(duration * FPS)
    commands = json.loads((directory / 'qa' / f'{name}-commands.json').read_text())
    checkpoints = [min(duration - 0.1, c['time'] + max(2, len(c['command']) * 0.035 + 4)) for c in commands if c['command'] in ['addresses', 'reference', 'matched', 'table(geocode_stage(g))'] or c['command'].startswith('g <-') or c['command'].startswith('write.csv')]
    checkpoint_frames = {round(t * FPS): i for i, t in enumerate(checkpoints)}
    cmd = [FFMPEG, '-y', '-hide_banner', '-loglevel', 'error', '-f', 'rawvideo', '-pixel_format', 'rgb24', '-video_size', f'{W}x{H}', '-framerate', str(FPS), '-i', '-', '-an', '-c:v', 'libx264', '-preset', 'veryfast', '-crf', '18', '-pix_fmt', 'yuv420p', '-threads', '4', '-movflags', '+faststart', '-metadata', 'title=' + name.replace('-', ' '), '-metadata', 'comment=Real R console output; automated typing; unaltered timing.', str(directory / f'{name}.mp4')]
    canvas = Image.new('RGB', (W, H), BG)
    old_lines = [None] * header['height']
    event_index = 0
    pipe = None
    log = None
    if not preview:
        log = open(directory / 'build' / f'{name}-render.log', 'w')
        pipe = subprocess.Popen(cmd, stdin=subprocess.PIPE, stderr=log)
    for n in range(frames):
        now = n / FPS
        while event_index < len(events) and events[event_index][0] <= now:
            stream.feed(events[event_index][2])
            event_index += 1
        lines = screen.display
        for j, text in enumerate(lines):
            if text != old_lines[j]:
                canvas.paste(line_image(text), (0, Y + j * LINE))
                old_lines[j] = text
        im = canvas.copy()
        if not screen.cursor.hidden and int(now * 2) % 2 == 0:
            d = ImageDraw.Draw(im)
            cx = X + screen.cursor.x * CELL
            cy = Y + screen.cursor.y * LINE
            d.rectangle((round(cx), cy + 4, round(cx) + 2, cy + 30), fill=FG)
        if n in checkpoint_frames:
            i = checkpoint_frames[n]
            im.save(directory / 'qa' / f'{name}-{i:02d}.png')
        if pipe:
            pipe.stdin.write(im.tobytes())
        if n and n % (FPS * 30) == 0:
            print(f'{name}: rendered {n // FPS}/{math.ceil(duration)} seconds', flush=True)
    if pipe:
        pipe.stdin.close()
        assert pipe.wait() == 0
        log.close()
    for event in events[event_index:]:
        stream.feed(event[2])
    im.save(directory / 'qa' / f'{name}-poster.png')
    (directory / 'qa' / f'{name}-final-screen.txt').write_text('\n'.join(screen.display))
    stats = dict(duration_seconds=round(frames / FPS, 3), recording_seconds=duration, frames=frames, width=W, height=H, fps=FPS, audio_tracks=0, checkpoints=checkpoints)
    (directory / 'qa' / f'{name}-render.json').write_text(json.dumps(stats, indent=2))
    print('RENDERED', name, stats['duration_seconds'], 'seconds', flush=True)
if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--episode', choices=['address-matching', 'range-geocoding'])
    parser.add_argument('--preview', action='store_true')
    parser.add_argument('--directory', type=Path, default=ROOT, help='Directory containing the recordings.')
    parser.add_argument('--font', type=Path, help='Monospaced font file; defaults to Menlo or DejaVu Sans Mono.')
    args = parser.parse_args()
    for name in [args.episode] if args.episode else ['address-matching', 'range-geocoding']:
        render(name, args.directory.resolve(), args.preview, args.font)
