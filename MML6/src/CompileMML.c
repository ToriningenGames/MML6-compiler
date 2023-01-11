//MML v6 (identical to v5)
//Argument flags:
/*
    -i=filename:
        open and use filename as MML data (- for piped input)
    -o=filename:
        write compiled output to filename (- for piped output)
    -t=platform:
        to which playing platform to target
            gb: Nintendo Gameboy/ "Marisa's Sound Engine"
            self: Text/ MML format
            none: Do not compile (default)
    -s:
        Put absolutely nothing in output
    -:
        Batch mode (use stdin and stderr, silent otherwise)
    -?, -h:
        Display help information and exit
    -v:
        Display version information and exit
 */

//News:
    //Now uses a temporary file, so output files aren't deleted on compiler errors.



#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdnoreturn.h>
#include <ctype.h>
#include <stdarg.h>
#include <string.h>
#include <stdbool.h>
#include "MMLtargetDef.h"



struct args {
    FILE** infile;
    char** inname;
    FILE** outfile;
    char** outname;
    enum platform target;
};
struct args* MAINARGS;

struct args*        parseArgs(int argc, char** argv);
extern MMLStruct*   readMML(FILE* infile);
extern int          writeMML(MMLStruct* intree, FILE* outfile, enum platform form);
extern MMLStruct*   processMML(MMLStruct* intree);

void closeInput(struct args* list);
void openOutput(FILE* code, struct args* list);


int main(int argc, char** argv) {
    MAINARGS = parseArgs(argc, argv);
    if (!MAINARGS) {
        return 0;
    };
    if (!MAINARGS->infile[0]) {
        puts("Error: no input files.");
        return 1;
    };
    if (MAINARGS->target == pt_none) {
        puts("Error: no target.");
    };
    MMLStruct* musData = readMML(MAINARGS->infile[0]);
    closeInput(MAINARGS);
    musData = processMML(musData);
    FILE* intermediate = tmpfile();
    //Microsoft is vile when it comes to C support
    //We can't use tmpfile, because that requires the program to run as admin!
    //tmpnam may not be usable as a file on its own; we need to prepend a dot and backslash
    //We can't do this everywhere because it would require intermediate folders on 'nix,
    //and those aren't created when making a file.
    char temp[L_tmpnam+2];
    temp[0] = '\0';
    if (!intermediate){
        strncpy(temp, ".\\", 3);
        tmpnam(temp+2);
        intermediate = fopen(temp, "wb+");
        if (!intermediate) {
            perror("Error writing output");
            return -1;
        }
    }
    int resCode = writeMML(musData, intermediate, MAINARGS->target);
    openOutput(intermediate, MAINARGS);
    if (temp[0])
        fclose(intermediate);
        remove(temp);
    return resCode;
}


enum platform selTar(char* arg) {
    for (int i = 0; targetList[i].target; i++) {
        if (!strcmp(arg, targetList[i].name)) return targetList[i].target;
    }
    puts("Note: unrecognised target");
    return pt_none;
}


//Call with command line args
//Returns pointer to static struct; changes after every call.
//Returns NULL when called without any commandline args to parse
struct args* parseArgs(int argc, char** argv) {
    //Check and see if arguments were even passed
    if (!argv || (argc == 0 && !argv[0])) return NULL;
    //Arguments were passed, even if it turns out they're null
    static struct args nowArgs = {0};
    //Start by counting in and out files
    int incount=0, outcount=0;
    for (int i = 1; argv[i]; i++) {
        if (argv[i][0] != '-') {
            //Source file implicitly passed, like filetype assoc. and the like
            incount++;
            continue;
        }
        switch (tolower(argv[i][1])) {
            case 'o' : ;
                outcount++;
                break;
            case 'i' : ;
                incount++;
                break;
            case 't' : ;
            case 's' : ;
            case '?' : ;
            case 'h' : ;
            case 'v' : ;
                break;
            default  : ;
                //Not a known flag; failure
                fprintf(stderr, "Unknown flag '%s'\n", argv[i]);
                return NULL;
        }
    }
    //Allocate FILE* arrays and char* arrays for all these files!
    free(nowArgs.infile);
    nowArgs.infile = calloc(incount+1, sizeof(FILE*));
    nowArgs.inname = calloc(incount+1, sizeof(char*));
    free(nowArgs.outfile);
    //If no output is specified, we will use a default output
    //Unless the user is just asking for help, in which case, don't do anything
    if (!outcount && incount) {
        nowArgs.outfile = calloc(2, sizeof(FILE*));
        nowArgs.outname = calloc(2, sizeof(char*));
        nowArgs.outname[0] = "out.mcf";
    } else {
        nowArgs.outfile = calloc(outcount+1, sizeof(FILE*));
        nowArgs.outname = calloc(outcount+1, sizeof(char*));
    }
    //This is safe, because we will never come across a flag using the outfiles
    //We already checked, and none were found
    //Actually act on each argument. Unknown flags are impossible here
    int inIndex = 0; int outIndex = 0;
    for (int i = 1; argv[i]; i++) {
        if (argv[i][0] != '-') {
            //Source file implicitly passed, like filetype assoc. and the like
            nowArgs.infile[inIndex] = fopen(argv[i]+3, "r");
            nowArgs.inname[inIndex] = argv[i]+3;
            if (!nowArgs.infile[inIndex++]) {
                fprintf(stderr, "Error opening file '%s': ", argv[i]);
                perror(NULL);
                return NULL;
            }
            continue;
        }
        switch (tolower(argv[i][1])) {
            case 'o' : ;
                //Check for pipe input
                if (argv[i][3] == '-') {
                    nowArgs.outfile[outIndex] = stdout;
                    nowArgs.outname[outIndex++] = "stdout";
                    break;
                }
                //file output; Named output file
                nowArgs.outname[outIndex] = argv[i]+3;
                break;
            case 'i' : ;
                //Check for pipe input
                if (argv[i][3] == '-') {
                    nowArgs.infile[inIndex] = stdin;
                    nowArgs.inname[inIndex++] = "stdin";
                    break;
                }
                //file input; Named input file
                nowArgs.infile[inIndex] = fopen(argv[i]+3, "r");
                nowArgs.inname[inIndex] = argv[i]+3;
                if (!nowArgs.infile[inIndex++]) {
                    fprintf(stderr, "Error opening file '%s': ", argv[i]+3);
                    perror(NULL);
                    return NULL;
                }
                break;
            case 't' : ;
                //target; choose our binary output format
                nowArgs.target = selTar(argv[i]+3);
                break;
            case 's' : ;
                //silent; say nothing. Equivalent to '-w0'
                fclose(stderr);
                break;
            case '?' : ;
            case 'h' : ;
                //help?; provide concise usage information
                puts("Available options: iotsvh?\nFor more information, "
                "use --help.");
                return NULL;
            case 'v' : ;
                //version; provide concise program information
                puts("Version no. " VER);
                return NULL;
        }
    }
    return &nowArgs;
}

//Closes all the input files
void closeInput(struct args* list) {
    for (int i = 0; list->inname[i]; i++) {
        fclose(list->infile[i]);
    }
}

#define CPYBUFSIZ 1024
//Copy a temoprary file to real outputs
void openOutput(FILE* code, struct args* list) {
    rewind(code);
    void* buf = malloc(BUFSIZ);
    for (int i = 0; list->outname[i]; i++) {
        list->outfile[i] = fopen(list->outname[i], "wb");
        if (!list->outfile[i]) {
            fprintf(stderr, "Error opening file '%s': ", list->outname[i]);
            perror(NULL);
        }
        long int remainder;
        for (remainder = fread(buf, 1, BUFSIZ, code); !feof(code); remainder = fread(buf, 1, BUFSIZ, code)) {
            fwrite(buf, 1, BUFSIZ, list->outfile[i]);
        }
        fwrite(buf, 1, remainder, list->outfile[i]);
    }
    free(buf);
    fclose(code);
}

//Callback for when compiling fails.
//Print the error in a standardised format, then quit
noreturn void fail(int line, int column, char* message) {
    if (!MAINARGS->inname || !MAINARGS->inname[0]) fprintf(stderr, "%i:%i: Error: %s.\n", line, column, message);
    else fprintf(stderr, "%s:%i:%i: Error: %s.\n", MAINARGS->inname[0], line, column, message);
    exit(1);
}

//Callback for when compiling goes weird.
//Print the error in a standardised format, then return
void warn(int line, int column, char* message) {
    if (!MAINARGS->inname || !MAINARGS->inname[0]) fprintf(stderr, "%i:%i: Warning: %s.\n", line, column, message);
    else fprintf(stderr, "%s:%i:%i: Warning: %s.\n", MAINARGS->inname[0], line, column, message);
}
