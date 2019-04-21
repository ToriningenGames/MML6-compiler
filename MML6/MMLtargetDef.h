#ifndef Target_hed
#define Target_hed

#include <stdio.h>

//These macros work stringing magic on other #define macros
#define _STR(X) #X
#define STR(X) _STR(X)
//These are convenient macros for version information
#define VER_MAJ 6
#define VER_MIN 1
#define VER STR(VER_MAJ) "." STR(VER_MIN)

//Add your platform name to the end here
enum platform { pt_none, pt_self, pt_gameboy };


enum directive { dir_none, dir_octShift, dir_imply, dir_macro, //You won't see these
    dir_note, dir_volume, dir_sweep, dir_tempo, dir_octave, dir_instrument, dir_stacatto, dir_loop, dir_channel, dir_rest, dir_label,
    dir_error = -1 };   //I hope you don't see this

typedef struct token {
    int line;
    int column;
    int primaryVal;     //-1 if not present
    int secondVal;      //-1 if not present, -2 if not used
    struct token* prev;
    struct token* next;
    enum directive kind;
    union {
        _Bool isaLoopSet;
        _Bool isUp;
        char notePitch;
    };
} MMLStruct;

//Parse list manipulation functions
extern void delMMLStruct(MMLStruct* entry);
extern MMLStruct* addMMLStruct(MMLStruct* curr, MMLStruct* next);
extern MMLStruct* copyMMLStruct(MMLStruct* src);
extern void clrMMLStruct(MMLStruct* start);

//Add your function's name here
extern int writeMML_none(struct token* curr, FILE* outfile);
extern int writeMML_self(struct token* curr, FILE* outfile);
extern int writeMML_gb(struct token* curr, FILE* outfile);

extern void fail(int line, int column, char* message);
extern void warn(int line, int column, char* message);

struct targetID {
    enum platform target;
    int (*compiler)(struct token* curr, FILE* outfile);
    char* name;
};

static const struct targetID targetList[] = {
    #include "targetList.json"
};

#endif //Target_hed

