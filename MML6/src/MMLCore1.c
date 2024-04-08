//MML v6 (identical to v5)
//2019.01.19 ver 6.1
    //Added labels (: directive)
//2024.03.26 ver 6.5
    //Added several directives to work across GB and SID

#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdnoreturn.h>
#include <ctype.h>
#include <stdarg.h>
#include <string.h>
#include <stdbool.h>
#include "MMLtargetDef.h"

struct token* HEAD;

struct inchar {
    int line;
    int column;
    char letter;
};

struct string_base {
    int line;
    int column;
    char* word;
    struct string_base* next;
};
struct string_base* LEX_HEAD;

struct string_base*     lexer(FILE* infile);
struct token*           parser(struct string_base* instring);
noreturn extern void    fail(int line, int column, char* message);


//To add a new directive, append the directive's character
    //and the new directive enum (in MMLtargetDef.h) to this function
//Takes in the string of a string_base,
//and returns what kind of directive it declares
enum directive parseNewDir(char* text) {
    if (*text == 'A'
        || *text == 'B'
        || *text == 'C'
        || *text == 'D'
        || *text == 'E'
        || *text == 'F'
        || *text == 'G') return dir_note;
    if (*text == 'R') return dir_rest;
    if (*text == 'V') return dir_volume;
    if (*text == 'S') return dir_sweep;
    if (*text == 'T') return dir_tempo;
    if (*text == 'O') return dir_octave;
    if (*text == 'I') return dir_instrument;
    if (*text == 'H') return dir_stacatto;
    if (*text == '['
        || *text == ']') return dir_loop;
    if (*text == 'X') return dir_channel;
    if (*text == '^') return dir_imply;
    if (*text == '@') return dir_macro;
    if (*text == '<'
        || *text == '>') return dir_octShift;
    if (*text == ':') return dir_label;
    if (*text == 'W') return dir_wobble;
    if (*text == 'Q') return dir_control;
    return -1;
}

struct token* readMML(FILE* infile) {
    HEAD = NULL;
    LEX_HEAD = NULL;
    LEX_HEAD = lexer(infile);
    HEAD = parser(LEX_HEAD);
    //Avoid memory leaks
    for (struct string_base* curr = LEX_HEAD; curr; ) {
        struct string_base* prev = curr;
        free(curr->word);
        curr = curr->next;
        free(prev);
    }
    return HEAD;
}

//Do not compile
int writeMML_none(struct token* curr, FILE* outfile) {
    (void)curr;
    (void)outfile;
    return 0;
}


int writeMML(struct token* curr, FILE* outfile, enum platform form) {
    //Got a successful parse list, starting at curr
    for (int i = 0; targetList[i].target; i++) {
        if (targetList[i].target == form) {
            return targetList[i].compiler(curr, outfile);
        }
    }
    return -1;
}

//Call once with a file.
//Subsequent calls with (NULL) return read characters
//Returns pointer to internal static struct.
//Returns NULL when no file specified
//Can pass another file to start reading another file
//Will perform the following preprocessing:
    //Capitalizes all letters
    //Removes comments
    //Converts newlines to spaces
    //Contracts all spaces to one character
struct inchar* scanner(FILE* infile) {
    static struct inchar output = {0};
    static FILE* source = NULL;
    if (infile) {
        //New file; initialize
        source = infile;
        output.line = 1;
        output.column = 0;
        return NULL;
    }
    if (!source) return NULL;   //No input; can't do any work
    output.letter = toupper(fgetc(source));
    if (output.letter == '#') {
        while (output.letter == '#') {
            //Comment. Eat it.
            while (output.letter != '\n') {
                output.letter = fgetc(source);
                if (output.letter == EOF) return &output;
            }
            //Concatenate comments
            output.line++;
            output.column = 0;
            output.letter = fgetc(source);
        }
        ungetc(output.letter, source);
        output.letter = ' ';
    }
    while (isspace(output.letter)) {
        //Only return one whitespace
        if (output.letter == '\n') {
            output.line++;
            output.column = 0;
            output.letter = ' ';
        };
        char inchar = fgetc(source);
        if (isspace(inchar)) {
            output.letter = inchar;
            output.column++;
        } else {
            ungetc(inchar, source);
            output.column--;
            break;
        }
    };
    output.column++;
    return &output;
}

struct string_base* addStr(struct string_base* instr, struct inchar* toAdd) {
    instr->word = realloc(instr->word, strlen(instr->word)+2);
    instr->word[strlen(instr->word)+1] = '\0';
    instr->word[strlen(instr->word)] = toAdd->letter;
    return instr;
}
struct string_base* newStr(struct inchar* starter) {
    struct string_base* instr = malloc(sizeof(struct string_base));
    instr->line = starter->line;
    instr->column = starter->column;
    instr->word = malloc(sizeof(char) * 2);
    instr->word[1] = '\0';
    instr->word[0] = starter->letter;
    instr->next = NULL;
    return instr;
}

//Call, passing in the file to lex
//Returns pointer to singly linked list of read elements
struct string_base* lexer(FILE* infile) {
    //Dummy starter
    struct string_base starter = {0};
    starter.word = calloc(1, sizeof(char));
    LEX_HEAD = &starter;
    struct string_base* curr = LEX_HEAD;
    scanner(infile);
    bool onhex = false;
    struct inchar* inletter;
    while ((inletter = scanner(NULL))->letter != EOF) {
        //Spaces are ignored except for separating alphanumerics from notes.
        if (isspace(inletter->letter)) {
            onhex = false;
            continue;
        }
        //Divisions between directives are symbols not part of a number
        if (strchr("$%.+-", inletter->letter) || isdigit(inletter->letter) || (onhex && isxdigit(inletter->letter))) {
            addStr(curr, inletter);
            //Include hex only in hex numbers
            if (!isxdigit(inletter->letter)) {
                onhex = inletter->letter == '$';
            }
        } else {
            onhex = false;
            curr->next = newStr(inletter);
            curr = curr->next;
        }
    }
    //Delete the dummy starter (if it wasn't filled)
    if (!LEX_HEAD->word[0]) {
        free(LEX_HEAD->word);
        LEX_HEAD = LEX_HEAD->next;
    }
    return LEX_HEAD;
}

//Takes in the string of a string_base,
//and returns what goes in the extra data union of a token
char fillExtraData(char* text) {
    char pitch = 1;
    switch (*text) {
        case 'G' :
            pitch++;
            pitch++;
        case 'F' :
            pitch++;
        case 'E' :
            pitch++;
            pitch++;
        case 'D' :
            pitch++;
            pitch++;
        case 'C' :
            pitch++;
        case 'B' :
            pitch++;
            pitch++;
        case 'A' :
            return pitch;
        case '>' :
        case '[' :
            return true;
        case ']' :
        case '<' :
            return false;
        default :
            return -1;
    }
}

struct token* copyToken(struct token* intok) {
    struct token* retVal = malloc(sizeof(struct token));
    memcpy(retVal, intok, sizeof(struct token));
    return retVal;
}

//Call, passing the head of a singly linked list of string_bases
//Outputs a doubly linked list of parsed tokens
//You will need to free the structs returned by this
struct token* parser(struct string_base* instring) {
    //Expecting:
        //A mandatory : non-numeric symbol
        //An optional : plus or minus
        //An optional : base symbol
        //An optional : (alpha)numeric symbol (any number)
        //An optional : dot
        //An optional : base symbol
        //An optional : (alpha)numeric symbol (any number)
    //Start up the list
    HEAD = malloc(sizeof(struct token));
    HEAD->prev = NULL;
    HEAD->next = NULL;
    HEAD->kind = dir_none;
    struct token* curr = HEAD;
    //Loop through each lexed directive
    for (struct string_base* string = instring; string; string = string->next) {
        //Create new entry at end of list
        curr->next = malloc(sizeof(struct token));
        curr->next->prev = curr;
        curr = curr->next;
        curr->next = NULL;
        curr->line = string->line;
        curr->column = string->column;
        curr->primaryVal = -1;
        curr->secondVal = -2;
        int i = 0;  //Index into the string word
        //Start with directive
        curr->kind = parseNewDir(string->word);
        if (curr->kind == dir_error) {
            char errmsg[] = "invalid directive ' '";
            errmsg[19] = string->word[i];
            fail(curr->line, curr->column, errmsg);
        }
        curr->notePitch = fillExtraData(string->word);
        i++;
        //Look for + or - (only valid for notes and some controls)
        if (curr->kind == dir_sweep || curr->kind == dir_instrument) {
            curr->isUp = true;
        }
        if (string->word[i] == '+' || string->word[i] == '-') {
            switch (curr->kind) {
                case dir_note :
                    if (string->word[i] == '+') curr->notePitch++;
                    if (string->word[i] == '-') curr->notePitch--;
                    if (curr->notePitch == 0) curr->notePitch = 12;
                    if (curr->notePitch == 13) curr->notePitch = 1;
                    break;
                case dir_sweep :
                case dir_instrument :
                    if (string->word[i] == '+') curr->isUp = true;
                    if (string->word[i] == '-') curr->isUp = false;
                    break;
                default :
                    fail(string->line, string->column+i, "unexpected '+/-'");
            }
            i++;
        }
        //Look for base sign
        int base = 10;
        if (string->word[i] == '$') {
            base = 16;
            i++;
        } else if (string->word[i] == '%') {
            base = 2;
            i++;
        }
        //Look for number
        char* endcheck;
        curr->primaryVal = (int)strtol(string->word+i, &endcheck, base);
        //Special case: strtol() will return 0, not -1 when there's no number
        if (string->word+i == endcheck) curr->primaryVal = -1;
        if (*endcheck && *endcheck != '.') {
            fail(string->line, string->column+i, "bad numeral");
        }
        i = endcheck - string->word;
        //Look for decimal
        if (string->word[i] == '.') {
            curr->secondVal++;
            i++;
            //Look for base sign
            base = 10;
            if (string->word[i] == '$') {
                base = 16;
                i++;
            } else if (string->word[i] == '%') {
                base = 2;
                i++;
            }
            //Look for number
            curr->secondVal = (int)strtol(string->word+i, &endcheck, base);
            if (string->word+i == endcheck) curr->secondVal = -1;
            if (*endcheck) {
                fail(string->line, string->column+i, "bad second numeral");
            }
        }
    }
    //Remove dummy first entry (if unused)
    if (HEAD->kind == dir_none) {
        HEAD = HEAD->next;
        free(HEAD->prev);
        HEAD->prev = NULL;
    }
    return HEAD;
}
