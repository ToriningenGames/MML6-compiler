# MML Suite for Gameboy
By Orion Chandler

Inspired by the Professional Music Driver by M. Kajihara.
But so distinct it's incompatible.

Contained is a collection of programs designed to take a text description of a song, and produce a Gameboy ROM that plays it.
The resultant song file(s) and sound player can also be incorporated into an actual Gameboy game,
and, in theory, could be nigh seamlessly ported to other platforms.

The compiler is written in platform-neutral C, conforming to the C11 standard, though I see no reason why a compliant C99 compiler couldn't handle it.


Provided is a copy of WLA-DX to assemble the Gameboy player with,
a Windowsey Makefile (sorry Linux people!),
a few songs to test the player out with,
the source code of the Gameboy ROM,
and the source code of the conversion tool.

GCC and make are required to build MML6
WLA-GB and WLALINK are required to assemble the musPlayer ROM.

Songwriter/Player Use:
 - Write your song! A few files are provided in the Songs subdirectory to start with
	 - MMLspec.txt in the MML6 subdirectory contains the specifications for MML6. For writing songs, read the *back* half.
	 - The songs in the Songs subdirectory are two test bits (Test and scale), and two Touhou adaptations (Spark2 and Doll).
		Touhou Project is copyright Team Shanghai Alice and their presence here is incidental.
 - Run MML6 with your song file as the -i= argument, and "Song.mcs" as the -o= argument. Have "gb" as the -t= argument
 - Ensure Song.mcs is in the obj subdirectory.
 - Assemble musPlayer.asm. It is written for WLA-GB.
 - Link together the ROM. The linkfile is generated in the Makefile
 - Play the resulting ROM in your favorite Gameboy emulator. Or on actual Gameboy hardware!

Game development Use:
 - Write your songs. Use the above instructions.
 - Incorporate playerSongs.asm, Sound.asm, and Voicelist.asm into your project.
 - Instructions for assembly-side incorporation are in Sound.asm

Target development Use:
 - Make a new C file for your target
	 - You must include "MMLtargetDef.h"
	 - You must define a function with a particular signature. See MML_MML.c for an example.
 - Add an enum to MMLTargetDef.h
 - Add an entry to targetList.json with the function name, the enum, and a user-friendly "target name"

Q&A:
Q: Why should I ever use this?
A: Well, the output song files are small, and are designed to be played alongside a game.
	Also, I don't know of anything else that takes an MML-like language and puts it on a Gameboy.
	Therefore, this is an extensible and portable(ish) option for music-writing on a Gameboy, and your pieces can be put in games!

Q: Why is it called "MML6"? Where are the other five?
A: Because when I started, I knew nothing of planning, language parsing, or compiling, and went through 6 different approaches to making the format. I feel the result is decent. The others... not so much.

Q: Why does the Makefile make it with -O1?
A: Because it subtly breaks on higher optimisations: loops become misaligned on Gameboy. Undefined behaviour to blame? Likely.

Q: This makefile doesn't work!
A: It's designed for GCC on Windows via Mingw. Filepaths use the backslash, things are deleted with Windows-style `RMDIR` and `DEL`, and the command separator works just _slightly_ different.

Q: What's "MMLlangspec.xml" in the MML6 folder?
A: That's a language file for Notepad++. It syntax highlights your MML files.

Q: I'm on Linux and I hate you.
A: I really am sorry. Linux support is something I'd like to have more of, but I had to start somewhere, and, well...

Q: I'm on Mac and I hate you.
A: MacOS support is not currently planned.

Q: Your software sucks!
A: Sorry about that. No warranty.

Q: Can I use this with LSDJ?
A: Not yet. Though, it may be possible to write LSDJ as a target for the MML compiler, it hasn't been done yet.

Q: Can I use this for my game?
A: Yes! Please do! And please tell me about it!
	One thing: you _have_ to include LICENSE.txt with your game distribution/manual, and you add a line to the end of it, with info. READ THAT FILE!

Q: Can I sell songs with this program?
A: Sure. Just heed the license file provided with this package.

Q: I didn't get a license file.
A: Then you got shorted, and someone violated the license. The original code is on Github, so if that still exists at your time point, you can find the original.

Q: I have a question!
A: Ask away. You can reach me at orionchandler1 at gmail dot com, current as of April 21, 2019.
