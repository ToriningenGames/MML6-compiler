//MML Verifier
//Verifies two MMLs sources produce (basically) the same output when ran
//Done by running for a very long time (1 day of playback)
#include <stdio.h>
#include <stdlib.h>


struct state {
        long needle;            //Current playback position
        long looppoint[8];      //All loop dest pointers
        int loopcount[8];       //All running counters
        int length[16];         //All note lengths in ticks
        int volume;             //Raw value
        int instrument;         //Raw value
        int sustain;            //Raw value
        int tempo;              //Ticks per frame, (8.8)
        int sweep;              //Raw value
        int octave;             //From base
        int dirnum;             //Visible
} default_state;

struct note {
        int pitch;      //Absolute pitch/0 for rest
        int volume;     //Raw value
        int instrument; //Raw value
        int sustain;    //Raw value
        int sweep;      //Raw value
        int length;     //Absolute time (ms)
};

int compareNotes(struct note a, struct note b)
{
        return a.pitch == b.pitch
            && a.volume == b.volume
            && a.sweep == b.sweep
            && a.instrument == b.instrument
            && a.sustain == b.sustain
            && a.length == b.length;
}

int getAbsoluteTime(struct state frame, int note)
{
        int index = note & 0x0F;
        double temp = frame.length[index];      //Ticks
        //ticks * beats/tick * minutes/beat * ms/second == ms
        temp /= 40;             //Ticks per beat
        temp /= frame.tempo;    //Beats per minute
        temp *= 60;             //Seconds per minute
        temp *= 1000;           //Milliseconds per second
        return (int)temp;
}

int getAbsolutePitch(struct state frame, int note)
{
        int octpitch = ((note & 0xF0) >> 4) - 3;        //Make base note 1
        if (!octpitch) return 0;        //For rest catch
        return frame.octave * 12 + octpitch;
}

struct note getNextNote(struct state *frame, unsigned char *songdata)
{
        struct note nextnote = {0};
        while (!nextnote.length) {
                int dir = songdata[frame->needle++];
                frame->dirnum++;
                if (dir >= 0x30) {
                        //Note/Rest
                        //Timbre is determined when the note is started
                        nextnote.volume = frame->volume;
                        nextnote.instrument = frame->instrument;
                        nextnote.sustain = frame->sustain;
                        nextnote.sweep = frame->sweep;
                        nextnote.pitch = getAbsolutePitch(*frame, dir);
                        nextnote.length = getAbsoluteTime(*frame, dir);
                        while (songdata[frame->needle] == 3 && songdata[frame->needle+1] == 0) {
                                //Tie follows
                                frame->needle+=2;
                                nextnote.length += getNextNote(frame, songdata).length;
                                frame->dirnum--;        //Remove read note
                        }
                } else if (dir >= 0x20) {
                        //Octave
                        frame->octave = dir & 0x0F;
                } else if (dir >= 0x10) {
                        //Length
                        frame->length[dir & 0x0F] = songdata[frame->needle++];
                        frame->dirnum--;        //Lengths not printed
                } else if (dir >= 0x08) {
                        //Loop
                        dir &= 0x07;
                        int data = songdata[frame->needle++];
                        if (data & 0x80) {
                                //goto
                                data &= 0x7F;
                                if (data && data <= frame->loopcount[dir]++) {
                                        //Do not loop
                                } else {
                                        //Do loop
                                        frame->needle = frame->looppoint[dir];
                                }
                        } else {
                                //setloop
                                frame->looppoint[dir] = frame->needle + data - 2;     //From start of directive (2 bytes)
                                frame->loopcount[dir] = 0;
                        }
                } else if (dir >= 0x40) {
                        //Instrument
                        frame->instrument = ((dir & 0x03) << 8) | songdata[frame->needle++];
                } else if (dir == 0x03) {
                        //Tempo
                        frame->tempo = songdata[frame->needle++];
                } else if (dir == 0x02) {
                        //Stacatto
                        frame->sustain = songdata[frame->needle++];
                } else if (dir == 0x01) {
                        //Volume
                        frame->volume = songdata[frame->needle++];
                } else if (dir == 0x00) {
                        //Sweep
                        frame->sweep = songdata[frame->needle++];
                }
        }
        return nextnote;
}

int compareChannel(unsigned char *a, unsigned char *b)
{
        struct state sa, sb;
        sa = sb = default_state;
        long time = 0;
        while (time < 24 * 60 * 60 * 1000) {    //ms in a day
                struct note na, nb;
                na = getNextNote(&sa, a);
                nb = getNextNote(&sb, b);
                if (!compareNotes(na, nb)) {
                        printf("Inequality b/w dirs %i and %i, about %li ms in\n", sa.dirnum, sb.dirnum, time);
                        printf("Left  note: note %i\tvol %i\tinst %i\tsus %i\tsweep %i\tlen %i\n", na.pitch, na.volume, na.instrument, na.sustain, na.sweep, na.length);
                        printf("Right note: note %i\tvol %i\tinst %i\tsus %i\tsweep %i\tlen %i\n", nb.pitch, nb.volume, nb.instrument, nb.sustain, nb.sweep, nb.length);
                        return 0;
                }
                time += na.length;
        }
        return 1;
}

void compareFiles(unsigned char *a, unsigned char *b)
{
        long offa, offb;
        for (int i = 0; i < 5; i++) {
                offa = *(a+2*i) + *(a+2*i+1) * 256;
                offb = *(b+2*i) + *(b+2*i+1) * 256;
                if (!compareChannel(a+offa, b+offb)) {
                        printf("Channel %i failed\n", i);
                } else {
                        printf("Channel %i passed\n", i);
                }
        }
}

int main(int argc, char **argv)
{
        if (argc < 3) {
                puts("Usage: mmlcompare file1 file2");
                return 1;
        };
        default_state.needle = 0;
        default_state.volume = 0;
        default_state.instrument = 2;
        default_state.sustain = 0;
        default_state.tempo = 120;
        default_state.sweep = 0;
        default_state.octave = 2;
        default_state.dirnum = 0;
        FILE *leftfile = fopen(argv[1], "rb");
        FILE *rightfile = fopen(argv[2], "rb");
        unsigned char *leftdata = malloc(65536);         //maximum potential file size
        unsigned char *rightdata = malloc(65536);
        fread(leftdata, 1, 65536, leftfile);
        fread(rightdata, 1, 65536, rightfile);
        fclose(leftfile);
        fclose(rightfile);
        compareFiles(leftdata, rightdata);
        free(leftdata);
        free(rightdata);
        return 0;
}
