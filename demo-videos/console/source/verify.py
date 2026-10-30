"""Check saved R outputs, terminal recordings, and silent MP4 exports."""

import argparse
import csv
import hashlib
import json
from pathlib import Path
import re
import shutil
import subprocess

import imageio_ffmpeg

ROOT = Path(__file__).resolve().parents[1]
REPOSITORY = ROOT.parents[1]
EPISODES = ("address-matching", "range-geocoding")


def verify(directory):
    directory = directory.resolve()
    (directory / "data").mkdir(parents=True, exist_ok=True)
    (directory / "qa").mkdir(parents=True, exist_ok=True)
    ffmpeg = imageio_ffmpeg.get_ffmpeg_exe()
    report = {}

    with (directory / "work/address-matching/matched_addresses.csv").open() as f:
        matching = list(csv.DictReader(f))
    with (directory / "work/range-geocoding/geocoded_addresses.csv").open() as f:
        geocoding = list(csv.DictReader(f))
    assert len(matching) == len(geocoding) == 4
    assert [row["property_id"] for row in matching] == [
        "site_001", "site_001", "site_002", "NA"
    ]
    assert [row["match"] for row in geocoding] == ["range", "range", "range", "none"]
    assert geocoding[0]["longitude"] == geocoding[1]["longitude"]
    assert geocoding[0]["latitude"] == geocoding[1]["latitude"]
    assert geocoding[3]["longitude"] == geocoding[3]["latitude"] == "NA"
    for filename, episode in [
        ("matched_addresses.csv", "address-matching"),
        ("geocoded_addresses.csv", "range-geocoding"),
    ]:
        shutil.copy2(directory / "work" / episode / filename, directory / "data" / filename)

    for episode in EPISODES:
        cast = directory / f"{episode}.cast"
        video = directory / f"{episode}.mp4"
        events = [json.loads(line) for line in cast.read_text().splitlines()]
        assert events[0]["version"] == 2
        assert all(event[1] == "o" for event in events[1:])
        assert all(a[0] <= b[0] for a, b in zip(events[1:], events[2:]))
        text = "".join(event[2] for event in events[1:])
        assert "Could not detect number of cores" not in text
        assert not re.search(r"(?:^|[\r\n])Error(?: in|:)", text)
        asciinema = shutil.which("asciinema")
        if asciinema:
            subprocess.run([
                asciinema, "convert", "--overwrite", "-f", "txt", str(cast),
                str(directory / "qa" / f"{episode}-asciinema.txt"),
            ], check=True)
        decoded = subprocess.run(
            [ffmpeg, "-v", "error", "-i", str(video), "-f", "null", "-"],
            capture_output=True, text=True,
        )
        assert decoded.returncode == 0 and not decoded.stderr.strip(), decoded.stderr
        metadata = subprocess.run(
            [ffmpeg, "-hide_banner", "-i", str(video)], capture_output=True, text=True,
        ).stderr
        assert "1920x1080" in metadata and "24 fps" in metadata
        assert "Audio:" not in metadata and "Subtitle:" not in metadata
        stats = json.loads((directory / "qa" / f"{episode}-render.json").read_text())
        assert abs(stats["duration_seconds"] - events[-1][0]) < 1 / 24 + 0.001
        timestamps = [round(t, 3) for t in stats["checkpoints"]]
        timestamps.append(max(0, stats["duration_seconds"] - 0.5))
        for i, timestamp in enumerate(timestamps):
            subprocess.run([
                ffmpeg, "-v", "error", "-y", "-ss", str(timestamp), "-i", str(video),
                "-frames:v", "1", str(directory / "qa" / f"{episode}-exported-{i:02d}.png"),
            ], check=True)
        if video.is_relative_to(REPOSITORY):
            subprocess.run(
                ["git", "check-ignore", "-q", "--", str(video)], cwd=REPOSITORY, check=True,
            )
            tracked = subprocess.run(
                ["git", "ls-files", "--error-unmatch", str(video)], cwd=REPOSITORY,
                capture_output=True,
            )
            assert tracked.returncode != 0, f"Generated video is tracked: {video}"
        report[episode] = {
            "duration_seconds": stats["duration_seconds"],
            "bytes": video.stat().st_size,
            "sha256": hashlib.sha256(video.read_bytes()).hexdigest(),
            "audio_tracks": 0,
            "subtitle_tracks": 0,
            "cast_valid": True,
            "asciinema_conversion": "passed" if asciinema else "not installed",
            "full_decode": "passed",
            "output_rows": 4,
            "output_validation": "passed",
        }
        print(f"{episode}: {stats['duration_seconds']} seconds; checks passed", flush=True)
    (directory / "qa/validation.json").write_text(json.dumps(report, indent=2) + "\n")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--directory", type=Path, default=ROOT)
    verify(parser.parse_args().directory)
